;;; dape-deep-process.el --- Process management for dape-deep -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Di Xiu

;; Author: Di Xiu <dyi.shiou@gmail.com>
;; Assisted-by: Claude Code:claude-fable-5
;; Assisted-by: Claude Code:deepseek-v4.1-flash
;; Assisted-by: Codex:gpt-5.6-sol
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
;; SSH command construction, rsync synchronization, and PTY-backed target
;; process management for `dape-deep'.

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'dape)
(require 'dape-deep-config)
(require 'subr-x)

(defun dape-deep--shell-join (arguments)
  "Render ARGUMENTS as a shell-safe command string."
  (mapconcat #'shell-quote-argument arguments " "))

(defun dape-deep--ssh-command (target-command cwd &optional settings)
  "Build an argv list that runs TARGET-COMMAND remotely from CWD.
SETTINGS defaults to `dape-deep--remote-settings'."
  (let* ((settings (or settings (dape-deep--remote-settings)))
         (remote-cwd (dape-deep--remote-cwd cwd settings))
         (remote-body
          (format "cd %s && %s"
                  (shell-quote-argument remote-cwd)
                  target-command))
         (wrapped-command
          (concat (dape-deep--shell-join dape-deep-shell-command)
                  " " (shell-quote-argument remote-body)))
         (forward-arguments
          (mapcan (lambda (port)
                    (list "-L" (format "%d:localhost:%d" port port)))
                  (plist-get settings :ports))))
    (append (list dape-deep-ssh-program)
            dape-deep-ssh-arguments
            forward-arguments
            (list "--" (plist-get settings :host) wrapped-command))))

(defun dape-deep--rsync-command (&optional settings)
  "Build the rsync argv list for SETTINGS."
  (let ((settings (or settings (dape-deep--remote-settings))))
    (append
     (list dape-deep-rsync-program)
     dape-deep-rsync-arguments
     (list
      (format "--rsync-path=mkdir -p -- %s && rsync"
              (shell-quote-argument (plist-get settings :remote-root))))
     (mapcar (lambda (pattern) (concat "--exclude=" pattern))
             dape-deep-rsync-excludes)
     (list "--"
           (plist-get settings :local-root)
           (format "%s:%s"
                   (plist-get settings :host)
                   (plist-get settings :remote-root))))))

(defun dape-deep--format-command (command)
  "Format argv list COMMAND for display."
  (dape-deep--shell-join command))

;;;###autoload
(defun dape-deep-sync (&optional callback settings)
  "Synchronize local source to the remote host asynchronously.
Call CALLBACK without arguments after a successful synchronization.  A failed
synchronization does not invoke CALLBACK.  SETTINGS is an internal normalized
SSH settings plist; interactively it is derived from directory-local options."
  (interactive)
  (let* ((settings (or settings (dape-deep--remote-settings)))
         (command (dape-deep--rsync-command settings))
         (buffer (get-buffer-create dape-deep-sync-buffer))
         (old-process (get-buffer-process buffer)))
    (when (process-live-p old-process)
      (user-error "A synchronization is already running in %s"
                  (buffer-name buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "$ " (dape-deep--format-command command) "\n")))
    (message "dape-deep: syncing %s -> %s:%s ..."
             (plist-get settings :local-root)
             (plist-get settings :host)
             (plist-get settings :remote-root))
    (make-process
     :name "dape-deep rsync"
     :buffer buffer
     :command command
     :coding 'utf-8-unix
     :noquery t
     :file-handler t
     :sentinel
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (if (and (eq (process-status process) 'exit)
                  (zerop (process-exit-status process)))
             (progn
               (message "dape-deep: sync complete")
               (when callback
                 (funcall callback)))
           (display-buffer (process-buffer process))
           (message "dape-deep: sync failed (%s), see %s"
                    (string-trim event)
                    (buffer-name (process-buffer process)))))))))

(defun dape-deep--display-target-buffer (buffer)
  "Display target BUFFER with Dape's standard window placement.
That placement lives in a private function of Dape, so a release that renames
or drops it falls back to `display-buffer' rather than leaving the target
unreachable."
  (if (fboundp 'dape--display-buffer)
      (dape--display-buffer buffer)
    (display-buffer buffer)))

(defun dape-deep--start-target-process (command cwd &optional settings)
  "Start argv list COMMAND in CWD and return the process.
When SETTINGS is non-nil, copy the normalized target settings into the target
buffer so invoking Dape from that buffer retains the correct attach mapping."
  (let* ((buffer (get-buffer-create dape-deep-target-buffer))
         (old-process (get-buffer-process buffer)))
    (when (process-live-p old-process)
      (user-error "A target is already running in %s" (buffer-name buffer)))
    (with-current-buffer buffer
      (setq default-directory cwd)
      (unless (derived-mode-p 'dape-shell-mode)
        (dape-shell-mode))
      (when settings
        (setq-local dape-deep-backend (plist-get settings :backend)
                    dape-deep-host nil
                    dape-deep-local-root nil
                    dape-deep-remote-root nil
                    dape-deep-forward-ports nil
                    dape-deep-port (plist-get settings :port)
                    dape-deep-rank (plist-get settings :rank)
                    dape-command '(dape-deep-attach))
        (when (eq (plist-get settings :backend) 'ssh)
          (setq-local dape-deep-host (plist-get settings :host)
                      dape-deep-local-root
                      (plist-get settings :local-root)
                      dape-deep-remote-root
                      (plist-get settings :remote-root)
                      dape-deep-forward-ports
                      (plist-get settings :ports))))
      (ansi-color-for-comint-mode-on)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((process
             (make-process
              :name "dape-deep target"
              :buffer buffer
              :command command
              :connection-type 'pty
              :coding 'utf-8-unix
              :filter #'comint-output-filter
              :sentinel #'shell-command-sentinel
              :file-handler t)))
        (dape-deep--display-target-buffer buffer)
        process))))

(provide 'dape-deep-process)

;;; dape-deep-process.el ends here
