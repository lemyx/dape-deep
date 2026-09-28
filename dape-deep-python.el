;;; dape-deep-python.el --- Remote Python sources for local ty -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Di Xiu

;; Author: Di Xiu <dyi.shiou@gmail.com>
;; Assisted-by: Codex:gpt-6
;; URL: https://github.com/lemyx/dape-deep
;; License: GPL-3.0-or-later

;; This file is part of dape-deep.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Copy Python source files from the target environment for a local language
;; server.  Neither importing the dependencies nor installing them locally is
;; necessary.  Publish a fresh snapshot only after every transfer succeeds.

;;; Code:

(require 'dape-deep-config)
(require 'json)
(require 'subr-x)

(declare-function dape-deep--project-root "dape-deep-project")
(declare-function dape-deep--read-file "dape-deep-project" (path))
(declare-function dape-deep--atomic-write "dape-deep-project" (path content &optional mode))
(declare-function dape-deep--local-ty-body "dape-deep-project" (spec))
(declare-function dape-deep--managed-block "dape-deep-project" (name body))
(declare-function dape-deep--replace-managed-block "dape-deep-project"
                  (content name block conflict-re))
(defvar dape-deep--ty-environment-re)

(defconst dape-deep--python-path-code
  (concat
   "import json, os, sys, sysconfig\n"
   "standard = {os.path.realpath(sysconfig.get_path(k)) "
   "for k in ('stdlib', 'platstdlib')}\n"
   "standard |= {os.path.join(p, 'lib-dynload') for p in standard}\n"
   "paths = []\n"
   "for p in sys.path:\n"
   "    p = os.path.abspath(p)\n"
   "    if os.path.isdir(p) and os.path.realpath(p) not in standard "
   "and p not in paths:\n"
   "        paths.append(p)\n"
   "print('DAPE_DEEP_PATHS=' + json.dumps(paths))\n")
  "Probe import paths without importing third-party packages.
Keep site-packages, PYTHONPATH, and ordinary editable-install paths.  The
standard library is supplied by ty's bundled typeshed instead.")

(defun dape-deep--python-cache-directory (spec)
  "Return the source cache directory belonging to project SPEC."
  (expand-file-name
   (secure-hash 'sha256
                (prin1-to-string
                 (list (file-name-as-directory
                        (expand-file-name (plist-get spec :root)))
                       (file-name-as-directory
                        (expand-file-name (plist-get spec :local-root)))
                       (plist-get spec :host)
                       (plist-get spec :remote-python)
                       (file-name-as-directory
                        (plist-get spec :remote-root)))))
   (locate-user-emacs-file "dape-deep/python/")))

(defun dape-deep--python-cached-paths (spec)
  "Read the successfully synchronized import paths for SPEC, if any."
  (when (and (memq (plist-get spec :backend) '(ssh auto nil))
             (plist-get spec :host))
    (let ((file (expand-file-name "paths.json"
                                  (dape-deep--python-cache-directory spec))))
      (when (file-readable-p file)
        (condition-case nil
            (let ((paths (json-parse-string (dape-deep--read-file file)
                                             :array-type 'list)))
              (when (and (listp paths) paths
                         (seq-every-p
                          (lambda (path)
                            (and (stringp path)
                                 (file-name-absolute-p path)
                                 (not (file-remote-p path))
                                 (file-directory-p path)))
                          paths))
                paths))
          (error nil))))))

(defun dape-deep--python-ssh-arguments ()
  "Return noninteractive SSH options without target port forwarding."
  (append '("-T" "-o" "BatchMode=yes")
          (list "-o" (format "ConnectTimeout=%d"
                              (max 1 dape-deep-ssh-probe-timeout)))
          (seq-remove (lambda (arg) (member arg '("-t" "-tt")))
                      dape-deep-ssh-arguments)))

(defun dape-deep--python-probe-command (spec)
  "Return argv to inspect the selected interpreter in project SPEC."
  (let* ((relative (file-relative-name (plist-get spec :root)
                                      (plist-get spec :local-root)))
         (cwd (expand-file-name relative (plist-get spec :remote-root)))
         (body (format "cd %s && %s -c %s"
                       (shell-quote-argument cwd)
                       (shell-quote-argument (plist-get spec :remote-python))
                       (shell-quote-argument dape-deep--python-path-code))))
    (append (list dape-deep-ssh-program)
            (dape-deep--python-ssh-arguments)
            (list "--" (plist-get spec :host)
                  (concat (mapconcat #'shell-quote-argument
                                     dape-deep-shell-command " ")
                          " " (shell-quote-argument body))))))

(defun dape-deep--python-parse-paths (output)
  "Read absolute import paths from probe OUTPUT, ignoring login banners."
  (unless (string-match "^DAPE_DEEP_PATHS=\\(.*\\)\r?$" output)
    (error "Python did not report import paths"))
  (let ((paths (json-parse-string (match-string 1 output) :array-type 'list)))
    (unless (and (listp paths) paths
                 (seq-every-p
                  (lambda (path)
                    (and (stringp path)
                         (string-prefix-p "/" path)
                         (not (file-remote-p path))
                         (not (string-match-p "[\n\r]" path))))
                  paths))
      (error "Python returned invalid import paths"))
    (delete-dups paths)))

(defun dape-deep--python-rsync-options ()
  "Capture the current project's source-transfer program and options."
  (list dape-deep-rsync-program "-rLtz" "--prune-empty-dirs"
         "--protect-args"
         "-e" (mapconcat #'shell-quote-argument
                         (cons dape-deep-ssh-program
                               (dape-deep--python-ssh-arguments)) " ")
         "--include=*/" "--include=*.py" "--include=*.pyi"
         "--include=py.typed" "--exclude=*" "--"))

(defun dape-deep--python-rsync-command (host source destination options)
  "Copy HOST SOURCE to DESTINATION using captured program and OPTIONS."
  (append
   options
   (list (concat host ":" (file-name-as-directory source))
         (file-name-as-directory destination))))

(defun dape-deep--python-ty-content (spec paths before)
  "Prepare ty configuration for SPEC and PATHS, preserving BEFORE."
  (let* ((spec (plist-put (copy-sequence spec) :python-paths paths))
         (after
          (dape-deep--replace-managed-block
           (or before "") "local-ty"
           (dape-deep--managed-block "local-ty"
                                     (dape-deep--local-ty-body spec))
           dape-deep--ty-environment-re)))
    (when (or (eq after 'conflict)
              (and before (string-match-p "^<<<<<<<" before)))
      (user-error "Resolve ty.toml's environment settings with project setup first"))
    after))

;;;###autoload
(defun dape-deep-sync-python-environment ()
  "Fetch remote Python sources and configure local ty for definition lookup.
Run from a configured local SSH project using `dape-deep-local-lsp' = `ty'.
The selected remote interpreter supplies its import paths.  Only .py, .pyi,
and py.typed files are copied, using rsync 3 or later.  Project import paths
inside the source mapping use their local directories instead.  No packages
are installed and no remote language server is needed.

Transfers run asynchronously in *dape-deep Python sources*.  A failed transfer
or a concurrent edit to ty.toml leaves the previous configuration and snapshot
intact.  Run this again after changing the remote environment, then reconnect
Eglot if it has not reloaded the configuration."
  (interactive)
  (require 'dape-deep-project)
  (unless (eq dape-deep-local-lsp 'ty)
    (user-error "Set dape-deep-local-lsp to ty first"))
  (let* ((root (dape-deep--project-root))
         (settings (dape-deep--remote-settings))
         (spec (append (list :root root
                             :remote-python dape-deep-remote-python
                             :local-python dape-deep-local-python
                             :python-version dape-deep-python-version)
                       settings))
         (ty-file (expand-file-name "ty.toml" root))
         (before (dape-deep--read-file ty-file))
         (buffer (get-buffer-create "*dape-deep Python sources*"))
         (cache (dape-deep--python-cache-directory spec))
         (rsync-options (dape-deep--python-rsync-options))
         (default-directory root)
         staging)
    (when (file-remote-p root)
      (user-error "Run this command from the local project"))
    (unless (dape-deep--literal-absolute-path-p dape-deep-remote-python)
      (user-error "Set dape-deep-remote-python to the target interpreter"))
    (when (process-live-p (get-buffer-process buffer))
      (user-error "A Python source synchronization is already running"))
    ;; Refuse a conflicting user-owned table before doing any network work.
    (dape-deep--python-ty-content spec nil before)
    (make-directory cache t)
    (setq staging (make-temp-file (expand-file-name "snapshot-" cache) t))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer)))
    (message "dape-deep: reading Python import paths on %s ..."
             (plist-get settings :host))
    (letrec
        ((failed
          (lambda (err)
            (when (file-directory-p staging) (delete-directory staging t))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert "\n" (error-message-string err) "\n"))
            (display-buffer buffer)
            (message "dape-deep: Python source sync failed: %s"
                     (error-message-string err))))
         (run
          (lambda (command next)
            (let ((start (with-current-buffer buffer (point-max))))
              (make-process
               :name "dape-deep Python sources" :buffer buffer
               :command command :connection-type 'pipe
               :coding 'utf-8-unix :noquery t
               :sentinel
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   (condition-case err
                       (if (and (eq (process-status process) 'exit)
                                (zerop (process-exit-status process)))
                           (funcall next
                                    (with-current-buffer buffer
                                      (buffer-substring-no-properties
                                       start (point-max))))
                         (error "Command failed (%s): %s"
                                (process-exit-status process) (car command)))
                     (error (funcall failed err)))))))))
         (publish
          (lambda (paths)
            (unless (equal before (dape-deep--read-file ty-file))
              (error "File ty.toml changed during synchronization; run again"))
            (let ((after (dape-deep--python-ty-content spec paths before)))
              (dape-deep--atomic-write ty-file after)
              (condition-case err
                  (dape-deep--atomic-write
                   (expand-file-name "paths.json" cache)
                   (json-serialize (vconcat paths)))
                (error
                 (if before
                     (dape-deep--atomic-write ty-file before)
                   (delete-file ty-file))
                 (signal (car err) (cdr err)))))
            (message (concat "dape-deep: Python sources ready; "
                             "reconnect Eglot to reload ty.toml"))))
         (copy-next
          (lambda (remaining paths)
            (if (null remaining)
                (funcall publish (nreverse paths))
              (let* ((source (file-name-as-directory (car remaining)))
                     (remote-root (plist-get settings :remote-root))
                     (local (when (string-prefix-p remote-root source)
                              (let ((path (expand-file-name
                                           (string-remove-prefix remote-root source)
                                           (plist-get settings :local-root))))
                                (when (file-directory-p path) path))))
                     (destination (or local (expand-file-name
                                             (number-to-string (length paths))
                                             staging))))
                (if local
                    (funcall copy-next (cdr remaining) (cons local paths))
                  (make-directory destination t)
                  (message "dape-deep: copying Python sources from %s ..." source)
                  (funcall run
                           (dape-deep--python-rsync-command
                            (plist-get settings :host) source destination
                            rsync-options)
                           (lambda (_output)
                             (funcall copy-next (cdr remaining)
                                      (cons destination paths))))))))))
      (condition-case err
          (funcall run (dape-deep--python-probe-command spec)
                   (lambda (output)
                     (funcall copy-next
                              (dape-deep--python-parse-paths output) nil)))
        (error (funcall failed err))))))

(provide 'dape-deep-python)

;;; dape-deep-python.el ends here
