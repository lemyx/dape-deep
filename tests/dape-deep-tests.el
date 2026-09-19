;;; dape-deep-tests.el --- Tests for dape-deep -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Di Xiu

;; Author: Di Xiu <dyi.shiou@gmail.com>
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
;; Unit and isolated process integration tests for `dape-deep'.

;;; Code:

(require 'cl-lib)
(require 'dape-deep)
(require 'dape-deep-fakes)
(require 'ert)

;; The fixtures below describe the ty setup, which is no longer the default.
;; The suite selects it here, so the declared default is captured first and the
;; tests that cover the default assert on the captured value.
(defconst dape-deep-tests--default-local-lsp
  (default-value 'dape-deep-local-lsp))

(setq dape-deep-local-lsp 'ty)

(ert-deftest dape-deep-local-lsp-defaults-to-none-test ()
  (should (eq dape-deep-tests--default-local-lsp 'none)))

(ert-deftest dape-deep-default-local-python-test ()
  (should
   (equal (default-value 'dape-deep-local-python)
          (locate-user-emacs-file ".venv/bin/python"))))

(ert-deftest dape-deep-local-python-prompt-uses-default-directory-test ()
  (let* ((root (make-temp-file "dape-deep-prompt-root-" t))
         (default-python (expand-file-name ".emacs.d/.venv/bin/python" "~"))
         (dape-deep-local-python default-python)
         read-file-name-args)
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest _) root))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest args)
                     (setq read-file-name-args args)
                     default-python))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "local"))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) (or initial "")))
                  ((symbol-function 'read-number)
                   (lambda (_prompt default &rest _) default))
                  ((symbol-function 'read-shell-command)
                   (lambda (&rest _)
                     (ert-fail "Project setup must not prompt for a command"))))
          (let ((buffer-file-name (expand-file-name "debug.py" root)))
            (dape-deep--read-setup-spec))
          (should
           (equal read-file-name-args
                  (list "Local Python interpreter: "
                        (file-name-directory default-python)
                        default-python nil
                        (file-name-nondirectory default-python)))))
      (delete-directory root))))

(ert-deftest dape-deep-remote-root-prompt-preserves-git-relative-path-test ()
  (let* ((parent (make-temp-file "dape-deep-prompt-" t))
         (root (expand-file-name "example-repo" parent))
         (nested (expand-file-name "src/package" root))
         (default-python (expand-file-name ".emacs.d/.venv/bin/python" "~"))
         (dape-deep-remote-root nil)
         remote-root-default)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root) t)
          (make-directory nested t)
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'read-directory-name)
                     (lambda (&rest _) nested))
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _) default-python))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _) "ssh"))
                    ((symbol-function 'read-string)
                     (lambda (prompt &optional initial &rest _)
                       (when (string-equal prompt "Remote source mapping root: ")
                         (setq remote-root-default initial))
                       (or initial "")))
                    ((symbol-function 'read-number)
                     (lambda (_prompt default &rest _) default))
                    ((symbol-function 'read-shell-command)
                     (lambda (&rest _)
                       (ert-fail "Project setup must not prompt for a command"))))
            (let ((buffer-file-name (expand-file-name "train.py" root)))
              (should (equal (plist-get (dape-deep--read-setup-spec)
                                        :python-script)
                             "train.py")))
            (should (equal remote-root-default
                           "/root/example-repo/src/package"))))
      (delete-directory parent t))))

(ert-deftest dape-deep-remote-python-probe-populates-setup-defaults-test ()
  (let* ((root (make-temp-file "dape-deep-prompt-" t))
         (default-python (expand-file-name ".emacs.d/.venv/bin/python" "~"))
         remote-python-default
         version-default)
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest _) root))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) default-python))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "ssh"))
                  ((symbol-function 'dape-deep--probe-remote-python)
                   (lambda (host)
                     (should (equal host "gpu-box"))
                     '(:executable "/opt/venv/bin/python" :version "3.12")))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _)
                     (cond
                      ((string-equal prompt "SSH Host alias: ") "gpu-box")
                      ((string-equal prompt "Remote Python interpreter: ")
                       (setq remote-python-default initial))
                      ((string-equal prompt "Python version (optional): ")
                       (setq version-default initial))
                      (t (or initial "")))))
                  ((symbol-function 'read-number)
                   (lambda (_prompt default &rest _) default)))
          (let* ((buffer-file-name (expand-file-name "train.py" root))
                 (spec (dape-deep--read-setup-spec)))
            (should (equal remote-python-default "/opt/venv/bin/python"))
            (should (equal version-default "3.12"))
            (should (equal (plist-get spec :remote-python)
                           "/opt/venv/bin/python"))
            (should (equal (plist-get spec :python-version) "3.12"))))
      (delete-directory root t))))

(ert-deftest dape-deep-remote-python-probe-failure-allows-manual-input-test ()
  (let* ((root (make-temp-file "dape-deep-prompt-" t))
         (default-python (expand-file-name ".emacs.d/.venv/bin/python" "~")))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest _) root))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) default-python))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "ssh"))
                  ((symbol-function 'dape-deep--probe-remote-python)
                   (lambda (_host) nil))
                  ((symbol-function 'read-string)
                   (lambda (prompt &optional initial &rest _)
                     (cond
                      ((string-equal prompt "SSH Host alias: ") "gpu-box")
                      ((string-equal prompt "Remote Python interpreter: ")
                       (should-not initial)
                       "/manual/venv/bin/python")
                      ((string-equal prompt "Python version (optional): ")
                       (should-not initial)
                       "3.10")
                      (t (or initial "")))))
                  ((symbol-function 'read-number)
                   (lambda (_prompt default &rest _) default)))
          (let* ((buffer-file-name (expand-file-name "train.py" root))
                 (spec (dape-deep--read-setup-spec)))
            (should (equal (plist-get spec :remote-python)
                           "/manual/venv/bin/python"))
            (should (equal (plist-get spec :python-version) "3.10"))))
      (delete-directory root t))))

(ert-deftest dape-deep-probes-remote-python-through-configured-shell-test ()
  (let* ((root (make-temp-file "dape-deep-probe-" t))
         (bin (expand-file-name "bin" root))
         (python (expand-file-name "python3" bin))
         (dape-deep-ssh-program
          (dape-deep-tests--write-fake-ssh root))
         (dape-deep-shell-command '("sh" "-c"))
         (dape-deep-ssh-probe-timeout 2)
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (make-directory bin)
          (with-temp-file python
            (insert "#!/bin/sh\n"
                    "printf '%s\\n' "
                    "'DAPE_DEEP_PYTHON=/remote/venv/bin/python' "
                    "'DAPE_DEEP_VERSION=3.11'\n"))
          (set-file-modes python #o700)
          (setenv "PATH" (mapconcat #'identity
                                    (list bin "/usr/bin" "/bin")
                                    path-separator))
          (should
           (equal (dape-deep--probe-remote-python "gpu-box")
                  (list :home (getenv "HOME")
                        :executable "/remote/venv/bin/python"
                        :version "3.11"))))
      (delete-directory root t))))

;; A temporary directory is not a fixed place: a runner keeps its temporary
;; files below its own home, which is an anchor for the mirror, while a
;; workstation keeps them elsewhere.  The three tests below therefore name the
;; home they expect instead of reading it from the environment.
;;
;; This project holds no repository and no home above it, so it covers the last
;; fallback: the basename alone.
(ert-deftest dape-deep-remote-root-default-falls-back-to-local-root-test ()
  (let* ((home (make-temp-file "dape-deep-home-" t))
         (parent (make-temp-file "dape-deep-prompt-" t))
         (root (expand-file-name "plain-project" parent))
         (process-environment (cons (concat "HOME=" home)
                                    (copy-sequence process-environment))))
    (unwind-protect
        (progn
          (make-directory root)
          (should (equal (dape-deep--default-remote-root root)
                         "/root/plain-project")))
      (delete-directory parent t)
      (delete-directory home t))))

(ert-deftest dape-deep-remote-root-mirrors-path-below-local-home-test ()
  (let* ((home (make-temp-file "dape-deep-home-" t))
         (nested (expand-file-name "projects/demo" home))
         (plain (expand-file-name "plain" home))
         (process-environment (cons (concat "HOME=" home)
                                    (copy-sequence process-environment))))
    (unwind-protect
        (progn
          (make-directory nested t)
          (make-directory plain)
          (should (equal (dape-deep--default-remote-root nested)
                         "/root/projects/demo"))
          (should (equal (dape-deep--default-remote-root
                          nested "/home/remote-user")
                         "/home/remote-user/projects/demo"))
          (should (equal (dape-deep--default-remote-root plain)
                         "/root/plain"))
          ;; The home directory itself would put every other path below /root,
          ;; so it keeps its basename instead of being mirrored onto /root.
          (should (equal (dape-deep--default-remote-root home)
                         (concat "/root/"
                                 (file-name-nondirectory
                                  (directory-file-name home))))))
      (delete-directory home t))))

(ert-deftest dape-deep-remote-root-prefers-repository-below-local-home-test ()
  (let* ((home (make-temp-file "dape-deep-home-" t))
         (repository (expand-file-name "dev/monorepo" home))
         (subtree (expand-file-name "projects/engine" repository))
         (process-environment (cons (concat "HOME=" home)
                                    (copy-sequence process-environment))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" repository) t)
          (make-directory subtree t)
          (should (equal (dape-deep--default-remote-root subtree)
                         "/root/monorepo/projects/engine")))
      (delete-directory home t))))

(ert-deftest dape-deep-remote-root-preserves-monorepo-subtree-test ()
  (let* ((parent (make-temp-file "dape-deep-prompt-" t))
         (repository (expand-file-name "monorepo" parent))
         (subtree (expand-file-name "projects/engine" repository)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" repository) t)
          (make-directory subtree t)
          (should
           (equal (dape-deep--default-remote-root
                   subtree "/home/remote-user")
                  "/home/remote-user/monorepo/projects/engine"))
          (should
           (equal (dape-deep--default-remote-root
                   repository "/home/remote-user")
                  "/home/remote-user/monorepo")))
      (delete-directory parent t))))

(ert-deftest dape-deep-rejects-mapping-root-outside-project-test ()
  (let ((project-root (make-temp-file "dape-deep-project-" t))
        (mapping-root (make-temp-file "dape-deep-mapping-" t)))
    (unwind-protect
        (let ((spec (dape-deep-tests--project-spec project-root)))
          (setq spec (plist-put spec :local-root mapping-root))
          (should-error (dape-deep-project-plan spec) :type 'user-error))
      (delete-directory project-root t)
      (delete-directory mapping-root t))))

(ert-deftest dape-deep-setup-requires-python-file-buffer-test ()
  (with-temp-buffer
    (should-error (dape-deep--current-python-script)
                  :type 'user-error)
    (setq buffer-file-name "/tmp/notes.txt")
    (should-error (dape-deep--current-python-script)
                  :type 'user-error)
    (setq buffer-file-name "/tmp/train.py")
    (should (equal (dape-deep--current-python-script) "train.py"))))

(ert-deftest dape-deep-normalizes-settings-test ()
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (dape-deep-host "gpu-box")
         (dape-deep-local-root root)
         (dape-deep-remote-root "/workspace/project")
         (dape-deep-port 5678)
         (dape-deep-forward-ports '(5678 5679))
         (settings (dape-deep--settings)))
    (unwind-protect
        (progn
          (should (equal (plist-get settings :local-root)
                         (file-name-as-directory root)))
          (should (eq (plist-get settings :backend) 'ssh))
          (should (equal (plist-get settings :remote-root)
                         "/workspace/project/"))
          (should (equal (plist-get settings :ports) '(5678 5679))))
      (delete-directory root))))

(ert-deftest dape-deep-forward-ports-add-to-the-debug-port-test ()
  "Extra forwarded ports must not displace the debug-adapter port."
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (dape-deep-host "gpu-box")
         (dape-deep-local-root root)
         (dape-deep-remote-root "/workspace/project")
         (dape-deep-port 5678)
         (dape-deep-forward-ports '(6006 5679)))
    (unwind-protect
        (let* ((settings (dape-deep--settings))
               (command (dape-deep--ssh-command "true" root settings)))
          (should (equal (plist-get settings :ports) '(5678 6006 5679)))
          ;; The attach configuration dials `dape-deep-port' on localhost, so
          ;; that forward has to be in the argv whatever else was requested.
          (should (member "5678:localhost:5678" command))
          (should (member "6006:localhost:6006" command))
          (should (member "5679:localhost:5679" command)))
      (delete-directory root t))))

(ert-deftest dape-deep-local-settings-need-no-ssh-values-test ()
  (let ((dape-deep-backend 'local)
        (dape-deep-host nil)
        (dape-deep-local-root nil)
        (dape-deep-remote-root nil)
        (dape-deep-port 5678)
        (dape-deep-rank 0))
    (let ((settings (dape-deep--settings)))
      (should (eq (plist-get settings :backend) 'local))
      (should (= (plist-get settings :port) 5678))
      (should (= (plist-get settings :rank) 0)))
    (should-error (dape-deep--remote-settings) :type 'user-error)))

(ert-deftest dape-deep-auto-backend-follows-host-test ()
  (let ((dape-deep-backend 'auto)
        (dape-deep-host nil))
    (should (eq (dape-deep--effective-backend) 'local))
    (setq dape-deep-host "gpu-box")
    (should (eq (dape-deep--effective-backend) 'ssh))))

(ert-deftest dape-deep-local-sync-is-rejected-test ()
  (let ((dape-deep-backend 'local))
    (should-error (dape-deep-sync) :type 'user-error)))

(ert-deftest dape-deep-explicit-ssh-backend-requires-host-test ()
  (let ((dape-deep-backend 'ssh)
        (dape-deep-host nil))
    (should-error (dape-deep--settings) :type 'user-error)))

(ert-deftest dape-deep-maps-working-directory-test ()
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (child (expand-file-name "projects/demo" root))
         (settings (list :local-root (file-name-as-directory root)
                         :remote-root "/workspace/project/")))
    (unwind-protect
        (progn
          (make-directory child t)
          (should (equal (dape-deep--remote-cwd child settings)
                         "/workspace/project/projects/demo/"))
          (should (equal
                   (dape-deep--remote-cwd
                    temporary-file-directory settings)
                   "/workspace/project/")))
      (delete-directory root t))))

(ert-deftest dape-deep-builds-ssh-command-with-forwarding-test ()
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (child (expand-file-name "project with spaces" root))
         (settings (list :host "gpu-box"
                         :local-root (file-name-as-directory root)
                         :remote-root "/workspace/project/"
                         :ports '(5678 5679))))
    (unwind-protect
        (progn
          (make-directory child)
          (let ((command
                 (dape-deep--ssh-command "DEBUGPY=1 python train.py"
                                            child settings)))
            (should (equal (car command) dape-deep-ssh-program))
            (should (member "5678:localhost:5678" command))
            (should (member "5679:localhost:5679" command))
            (should (equal (cadr (member "-S" command)) "none"))
            (should (equal (car (last command 2)) "gpu-box"))
            (should (string-match-p "bash.*-lc" (car (last command))))
            (should (string-match-p "DEBUGPY" (car (last command))))
            (should (string-match-p "train.py" (car (last command))))))
      (delete-directory root t))))

(ert-deftest dape-deep-builds-shell-free-rsync-command-test ()
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (dape-deep-rsync-excludes '(".git" "path with spaces"))
         (settings (list :host "gpu-box"
                         :local-root (file-name-as-directory root)
                         :remote-root "/workspace/project/"))
         (command (dape-deep--rsync-command settings)))
    (unwind-protect
        (progn
          (should (equal (car command) dape-deep-rsync-program))
          (should (member "--exclude=.git" command))
          (should (member "--exclude=path with spaces" command))
          (should
           (member
            "--rsync-path=mkdir -p -- /workspace/project/ && rsync"
            command))
          (should (member "--" command))
          (should (equal (car (last command))
                         "gpu-box:/workspace/project/")))
      (delete-directory root))))

(ert-deftest dape-deep-rejects-invalid-host-and-command-test ()
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (dape-deep-host "-oProxyCommand=bad")
         (dape-deep-local-root root)
         (dape-deep-remote-root "/workspace/project"))
    (unwind-protect
        (progn
          (should-error (dape-deep--settings) :type 'user-error)
          (let ((dape-deep-host nil))
            (should-error (dape-deep-start "  " root)
                          :type 'user-error)))
      (delete-directory root))))

(ert-deftest dape-deep-starts-local-target-test ()
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (dape-deep-host nil)
         (dape-deep-target-buffer " *dape-deep-test-target*")
         (process
          (dape-deep-start
           (concat "printf 'dape-deep-ok:%s:%s:%s' "
                   "\"$DAPE_DEEP\" \"$DAPE_DEEP_PORT\" "
                   "\"$DAPE_DEEP_RANK\"")
           root)))
    (unwind-protect
        (progn
          (while (process-live-p process)
            (accept-process-output process 0.1))
          (with-current-buffer dape-deep-target-buffer
            (should (string-match-p "dape-deep-ok:1:5678:0"
                                    (buffer-string)))))
      (when (process-live-p process)
        (delete-process process))
      (when (get-buffer dape-deep-target-buffer)
        (kill-buffer dape-deep-target-buffer))
      (delete-directory root))))

(ert-deftest dape-deep-runs-through-fake-ssh-login-shell-test ()
  (let* ((root (make-temp-file "dape-deep-integration-" t))
         (local-root (expand-file-name "local" root))
         (local-cwd (expand-file-name "projects/demo" local-root))
         (remote-root (expand-file-name "remote" root))
         (remote-cwd (expand-file-name "projects/demo" remote-root))
         (dape-deep-host "gpu-box")
         (dape-deep-local-root local-root)
         (dape-deep-remote-root remote-root)
         (dape-deep-ssh-program
          (dape-deep-tests--write-fake-ssh root))
         (dape-deep-sync-before-run nil)
         (dape-deep-target-buffer " *dape-deep-test-ssh-target*")
         process)
    (unwind-protect
        (progn
          (make-directory local-cwd t)
          (make-directory remote-cwd t)
          (setq process
                (dape-deep-start
                 "printf 'remote-cwd=%s' \"$PWD\"" local-cwd))
          (while (process-live-p process)
            (accept-process-output process 0.1))
          (with-current-buffer dape-deep-target-buffer
            (should
             (string-match-p
              (regexp-quote (concat "remote-cwd=" remote-cwd))
              (buffer-string)))
            (should (eq dape-deep-backend 'ssh))
            (should (equal dape-deep-host "gpu-box"))
            (should (equal dape-deep-local-root
                           (file-name-as-directory local-root)))
            (should (equal dape-deep-remote-root
                           (file-name-as-directory remote-root)))
            (should (equal dape-command '(dape-deep-attach)))))
      (when (process-live-p process)
        (delete-process process))
      (when (get-buffer dape-deep-target-buffer)
        (kill-buffer dape-deep-target-buffer))
      (delete-directory root t))))

(ert-deftest dape-deep-sync-callback-keeps-origin-cwd-test ()
  (let* ((root (make-temp-file "dape-deep-remote-root-" t))
         (child (expand-file-name "projects/demo" root))
         (dape-deep-host "gpu-box")
         (dape-deep-local-root root)
         (dape-deep-remote-root "/workspace/project")
         (dape-deep-rsync-program (executable-find "true"))
         (dape-deep-rsync-arguments nil)
         (dape-deep-rsync-excludes nil)
         (dape-deep-sync-before-run t)
         (dape-deep-sync-buffer " *dape-deep-test-sync*")
         started-command
         started-cwd)
    (unwind-protect
        (progn
          (make-directory child t)
          (cl-letf (((symbol-function 'dape-deep--start-target-process)
                     (lambda (command cwd &optional _settings)
                       (setq started-command command
                             started-cwd cwd))))
            (let ((sync-process
                   (dape-deep-start "DEBUGPY=1 python train.py" child)))
              (while (process-live-p sync-process)
                (accept-process-output sync-process 0.1))
              (accept-process-output nil 0.01)))
          (should (equal started-cwd (file-name-as-directory child)))
          (should (equal (car started-command) dape-deep-ssh-program))
          (should (string-match-p "/workspace/project/projects/demo"
                                  (car (last started-command)))))
      (when (get-buffer dape-deep-sync-buffer)
        (kill-buffer dape-deep-sync-buffer))
      (delete-directory root t))))

(ert-deftest dape-deep-registers-debugpy-configuration-test ()
  (dape-deep-register-debugpy-config)
  (let ((config (alist-get 'dape-deep-attach dape-configs)))
    (should (equal (plist-get config 'host) "localhost"))
    (should (eq (plist-get config 'port) 'dape-deep-port))
    (should (eq (plist-get config 'fn)
                'dape-deep--configure-attach))
    (should-not (plist-member config 'prefix-local))
    (should-not (plist-member config 'prefix-remote))))

(ert-deftest dape-deep-configures-attach-for-local-backend-test ()
  (let* ((dape-deep-backend 'local)
         (config (dape-deep--configure-attach
                  '(:request "attach"))))
    (should-not (plist-member config 'prefix-local))
    (should-not (plist-member config 'prefix-remote))))

(ert-deftest dape-deep-configures-attach-for-ssh-backend-test ()
  (let* ((root (make-temp-file "dape-deep-attach-root-" t))
         (dape-deep-backend 'ssh)
         (dape-deep-host "gpu-box")
         (dape-deep-local-root root)
         (dape-deep-remote-root "/workspace/project")
         (config (dape-deep--configure-attach
                  '(:request "attach"))))
    (unwind-protect
        (progn
          (should (equal (plist-get config 'prefix-local)
                         (file-name-as-directory root)))
          (should (equal (plist-get config 'prefix-remote)
                         "/workspace/project/")))
      (delete-directory root))))

(ert-deftest dape-deep-preserves-existing-dape-configuration-test ()
  (let* ((existing '(dape-deep-attach
                     modes (python-mode)
                     host "localhost"
                     port 9876))
         (legacy '(debugpy-attach-remote
                   modes (python-mode python-ts-mode)
                   host "localhost"
                   port 5678))
         (bystander '(debugpy-attach-rank
                      modes (python-mode python-ts-mode)
                      host "localhost"
                      port 5678))
         (dape-configs (list existing legacy bystander)))
    (dape-deep-register-debugpy-config)
    (dape-deep-register-debugpy-config)
    (should (= (length dape-configs) 3))
    (should (eq (car dape-configs) existing))
    (should (eq (cadr dape-configs) legacy))
    (should (eq (caddr dape-configs) bystander))
    (should (= (seq-count
                (lambda (config)
                  (eq (car-safe config) 'dape-deep-attach))
                dape-configs)
               1))))

(ert-deftest dape-deep-records-script-relative-to-project-root-test ()
  "An entry file in a subdirectory stays startable from the project root."
  (let* ((root (file-name-as-directory (make-temp-file "dape-deep-project-" t)))
         (nested (expand-file-name "scripts" root))
         (spec (dape-deep-tests--project-spec root)))
    (unwind-protect
        (progn
          (make-directory nested)
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "train.py" nested))
            ;; A file below the project root keeps its relative path, because
            ;; the generated launcher runs from the root.
            (should (equal (dape-deep--script-below-root "train.py" root)
                           "scripts/train.py")))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "train.py" root))
            (should (equal (dape-deep--script-below-root "train.py" root)
                           "train.py")))
          (with-temp-buffer
            (setq buffer-file-name (make-temp-file "elsewhere-" nil ".py"))
            (should (equal (dape-deep--script-below-root "train.py" root)
                           "train.py")))
          (let ((script (dape-deep--target-script-content
                         (plist-put (copy-sequence spec)
                                    :python-script "scripts/train.py"))))
            (should (string-match-p "scripts/train.py" script))))
      (delete-directory root t))))

(ert-deftest dape-deep-rejects-absolute-and-escaping-entry-files-test ()
  (let ((root (make-temp-file "dape-deep-project-" t)))
    (unwind-protect
        (progn
          (dolist (script '("/tmp/train.py" "../train.py"))
            (should-error (dape-deep-project-plan
                           (plist-put (dape-deep-tests--project-spec root)
                                      :python-script script))
                          :type 'user-error))
          ;; A path that folds back into the root stays usable, and so does an
          ;; entry file in a subdirectory.
          (dolist (script '("train.py" "scripts/train.py" "scripts/../train.py"))
            (should (dape-deep-project-plan
                     (plist-put (dape-deep-tests--project-spec root)
                                :python-script script)))))
      (delete-directory root t))))

(ert-deftest dape-deep-uses-one-fixed-target-script-test ()
  (should (equal dape-deep-target-script
                 "dape-deep.sh")))

(ert-deftest dape-deep-excludes-local-ty-config-test ()
  (should (member "ty.toml" dape-deep-rsync-excludes)))

(ert-deftest dape-deep-preserves-original-buffer-names-test ()
  (should (equal dape-deep-target-buffer "*dape-shell*"))
  (should (equal dape-deep-sync-buffer "*dape-sync*")))

(ert-deftest dape-deep-uses-dape-target-window-placement-test ()
  (let (displayed)
    (cl-letf (((symbol-function 'dape--display-buffer)
               (lambda (buffer) (setq displayed buffer))))
      (dape-deep--display-target-buffer 'target-buffer))
    (should (eq displayed 'target-buffer))))

(ert-deftest dape-deep-displays-the-target-without-dape-placement-test ()
  "A Dape release that drops its private placement helper still shows a target."
  (let ((placement (symbol-function 'dape--display-buffer))
        displayed)
    (unwind-protect
        (progn
          (fmakunbound 'dape--display-buffer)
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer &rest _) (setq displayed buffer))))
            (dape-deep--display-target-buffer 'target-buffer))
          (should (eq displayed 'target-buffer)))
      (fset 'dape--display-buffer placement))))

(ert-deftest dape-deep-injects-at-point-with-indentation-test ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "def main():\n    print('start')\n")
    (goto-char (point-min))
    (forward-line 1)
    (dape-deep-inject-at-point t)
    (goto-char (point-min))
    (should (search-forward "    # dape-deep: begin" nil t))
    (should (search-forward "    print(f\"[dape-deep] waiting for Dape" nil t))
    (should (search-forward "    debugpy.wait_for_client()" nil t))
    (should (search-forward
             "    # dape-deep: end\n\n    print('start')" nil t))
    (should-error (dape-deep-inject-at-point t) :type 'user-error)
    (dape-deep-remove-injection nil t)
    (should (equal (buffer-string) "def main():\n    print('start')\n"))))

(ert-deftest dape-deep-injects-with-tabs-test ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert (concat "def main():\n\tprint('start')\n\n"
                    "if __name__ == \"__main__\":\n\tmain()\n"))
    (dape-deep-inject-main t)
    (goto-char (point-min))
    ;; The block copies the file's tab indentation instead of mixing spaces in.
    (should (search-forward "\n\t# dape-deep: begin\n" nil t))
    (should (search-forward "\n\t\timport debugpy" nil t))
    (should-not (string-match-p "^    " (buffer-string)))
    (dape-deep-remove-injection nil t)
    (should (equal (buffer-string)
                   (concat "def main():\n\tprint('start')\n\n"
                           "if __name__ == \"__main__\":\n\tmain()\n")))))

(ert-deftest dape-deep-injects-at-point-with-tabs-test ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "def main():\n\tprint('start')\n")
    (goto-char (point-min))
    (forward-line 1)
    (dape-deep-inject-at-point t)
    (goto-char (point-min))
    (should (search-forward "\n\t# dape-deep: begin\n" nil t))
    (should (search-forward "\n\t\timport debugpy" nil t))))

(ert-deftest dape-deep-reports-duplicate-bootstrap-blocks-test ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "if __name__ == \"__main__\":\n    main()\n")
    (dape-deep-inject-main t)
    ;; A merge of two branches that both injected leaves two blocks behind.
    (goto-char (point-min))
    (insert (dape-deep--bootstrap-block "    "))
    (should (string-match-p
             "found 2 begin and 2 end markers"
             (error-message-string
              (should-error (dape-deep-remove-injection)
                            :type 'user-error))))
    (should (string-match-p
             "found 2 begin and 2 end markers"
             (error-message-string
              (should-error (dape-deep-inject-main t)
                            :type 'user-error))))))

(ert-deftest dape-deep-injects-under-main-guard-test ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "if __name__ == \"__main__\":\n    main()\n")
    (dape-deep-inject-main t)
    (should
     (string-prefix-p
      "if __name__ == \"__main__\":\n    # dape-deep: begin\n"
      (buffer-string)))
    (should (string-suffix-p
             "    # dape-deep: end\n\n    main()\n"
             (buffer-string)))
    (should-error (dape-deep-inject-main t) :type 'user-error)))

(ert-deftest dape-deep-injects-under-main-guard-past-a-docstring-example-test ()
  "A guard quoted in a docstring must not capture the injected block."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "USAGE = \"\"\"\nif __name__ == \"__main__\":\n    main()\n\"\"\"\n"
            "\n\ndef main():\n    print('start')\n"
            "\n\nif __name__ == \"__main__\":\n    main()\n")
    (python-mode)
    (dape-deep-inject-main t)
    ;; The docstring keeps its literal text: the block went after the real
    ;; guard at the end of the file instead of into the string.
    (should (string-match-p
             (regexp-quote "\"\"\"\nif __name__ == \"__main__\":\n    main()\n\"\"\"\n")
             (buffer-string)))
    (should (string-suffix-p "    # dape-deep: end\n\n    main()\n"
                             (buffer-string)))
    (should-error (dape-deep-inject-main t) :type 'user-error)))

(ert-deftest dape-deep-injection-and-removal-skip-diff-preview-test ()
  (when-let* ((preview (get-buffer "*dape-deep diff*")))
    (kill-buffer preview))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "if __name__ == \"__main__\":\n    main()\n")
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (ert-fail "Injection and removal must not prompt"))))
      (dape-deep-inject-main)
      (should-not (get-buffer "*dape-deep diff*"))
      (dape-deep-remove-injection)
      (should-not (get-buffer "*dape-deep diff*")))
    (should (equal (buffer-string)
                   "if __name__ == \"__main__\":\n    main()\n"))))

(ert-deftest dape-deep-keeps-future-imports-first-test ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "from __future__ import annotations\n\nprint('start')\n")
    (goto-char (point-min))
    (should-error (dape-deep-inject-at-point t) :type 'user-error)
    (forward-line 2)
    (dape-deep-inject-at-point t)
    (should (string-prefix-p "from __future__ import annotations"
                             (buffer-string)))))

(ert-deftest dape-deep-refuses-edited-bootstrap-removal-test ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/debug.py")
    (insert "print('start')\n")
    (goto-char (point-min))
    (dape-deep-inject-at-point t)
    (goto-char (point-min))
    (search-forward "debugpy.wait_for_client()")
    (replace-match "debugpy.wait_for_editor()")
    (should-error (dape-deep-remove-injection nil t)
                  :type 'user-error)
    (dape-deep-remove-injection t t)
    (should (equal (buffer-string) "print('start')\n"))))

(defun dape-deep-tests--project-spec (root)
  "Return a complete project specification rooted at ROOT."
  (list :backend 'ssh
        :root root
        :host "gpu-box"
        :local-root (file-name-as-directory root)
        :remote-root "/root/demo"
        :local-python (expand-file-name ".emacs.d/.venv/bin/python" "~")
        :remote-python "/root/demo/.venv/bin/python"
        :python-script "debug.py"
        :python-version "3.12"
        :port 5678
        :rank 0))

(defun dape-deep-tests--local-project-spec (root)
  "Return a complete local project specification rooted at ROOT."
  (list :backend 'local
        :root root
        :local-root (file-name-as-directory root)
        :local-python (expand-file-name ".emacs.d/.venv/bin/python" "~")
        :python-script "debug.py"
        :python-version "3.12"
        :port 5678
        :rank 0))

(ert-deftest dape-deep-rejects-home-relative-paths-test ()
  "A leading ~ is expanded by a shell but not by rsync, so refuse those paths."
  (let* ((root (make-temp-file "dape-deep-tilde-" t))
         (dape-deep-backend 'ssh)
         (dape-deep-host "gpu-box")
         (dape-deep-local-root root)
         (dape-deep-remote-root "~/workspace/project"))
    (unwind-protect
        (progn
          (should-error (dape-deep--settings) :type 'user-error)
          (setq dape-deep-remote-root "/workspace/project")
          (should (dape-deep--settings))
          (let ((spec (dape-deep-tests--project-spec root)))
            (dolist (override '((:remote-root . "~/workspace")
                                (:remote-python . "~/venv/bin/python")
                                (:local-python . "~/.venv/bin/python")))
              (should-error
               (dape-deep-project-plan
                (plist-put (copy-sequence spec) (car override) (cdr override)))
               :type 'user-error))))
      (delete-directory root t))))

(ert-deftest dape-deep-atomic-write-follows-a-symbolic-link-test ()
  "A linked file keeps being a link, so a dotfiles repository stays in charge."
  (let* ((root (make-temp-file "dape-deep-link-" t))
         (real (expand-file-name "dotfiles-dir-locals.el" root))
         (link (expand-file-name ".dir-locals.el" root))
         (content "((nil (dape-deep-port . 5678)))\n"))
    (unwind-protect
        (progn
          (write-region "((nil (dape-deep-port . 9999)))\n" nil real nil 'silent)
          (make-symbolic-link real link)
          (should (file-symlink-p link))
          (dape-deep--atomic-write link content)
          (should (file-symlink-p link))
          (should (equal (dape-deep--read-file link) content))
          ;; The file the link points at is the one that changed.
          (should (equal (dape-deep--read-file real) content)))
      (delete-directory root t))))

(ert-deftest dape-deep-project-plan-applies-idempotently-test ()
  (let ((root (make-temp-file "dape-deep-project-" t)))
    (unwind-protect
        (let* ((spec (dape-deep-tests--project-spec root))
               (plan (dape-deep-project-plan spec)))
          (should (= (seq-count (lambda (item)
                                  (eq (plist-get item :status) 'create))
                                plan)
                     5))
          (should (= (length (dape-deep-apply-project-plan plan)) 5))
          (should (equal (dape-deep--read-file
                          (expand-file-name ".gitignore" root))
                         ;; The ty.toml this setup creates is its own, so it
                         ;; is the block that lists it.
                         (dape-deep--gitignore-block t)))
          (should-not (file-exists-p (expand-file-name "pyproject.toml" root)))
          (should (string-match-p
                   (regexp-quote
                    (expand-file-name ".emacs.d/.venv/bin/python" "~"))
                   (dape-deep--read-file
                    (expand-file-name "ty.toml" root))))
          (should
           (string-match-p
            "dape-deep-host"
            (dape-deep--read-file
             (expand-file-name ".dir-locals.el" root))))
          (should-not (file-exists-p
                       (expand-file-name ".dir-locals-2.el" root)))
          (should
           (equal
            (dape-deep--read-file
             (expand-file-name dape-deep-target-script root))
                    (concat "#!/usr/bin/env bash\n"
                            "set -euo pipefail\n\n"
                            "PYTHONUNBUFFERED=1 "
                            "/root/demo/.venv/bin/python debug.py\n")))
          (with-temp-buffer
            (insert-file-contents (expand-file-name ".dir-locals.el" root))
            (let ((settings (cdar (read (current-buffer)))))
              (should (equal (cdr (assq 'dape-deep-host settings))
                             "gpu-box"))
              (should (equal
                       (cdr (assq 'dape-deep-remote-root settings))
                       "/root/demo"))
              (should (equal (cdr (assq 'dape-command settings))
                             '(dape-deep-attach)))))
          (should (seq-every-p
                   (lambda (item) (eq (plist-get item :status) 'unchanged))
                   (dape-deep-project-plan spec))))
      (delete-directory root t))))

(ert-deftest dape-deep-local-project-plan-test ()
  (let ((root (make-temp-file "dape-deep-local-project-" t)))
    (unwind-protect
        (let* ((spec (dape-deep-tests--local-project-spec root))
               (plan (dape-deep-project-plan spec))
               (local-python (plist-get spec :local-python)))
          (should (= (length plan) 5))
          (should (= (seq-count (lambda (item)
                                  (eq (plist-get item :status) 'create))
                                plan)
                     5))
          (should (= (length (dape-deep-apply-project-plan plan)) 5))
          (should-not (file-exists-p (expand-file-name "pyproject.toml" root)))
          (should
           (equal
            (dape-deep--read-file
             (expand-file-name dape-deep-target-script root))
            (format (concat "#!/usr/bin/env bash\n"
                            "set -euo pipefail\n\n"
                            "PYTHONUNBUFFERED=1 %s debug.py\n")
                    local-python)))
          (with-temp-buffer
            (insert-file-contents (expand-file-name ".dir-locals.el" root))
            (let ((settings (cdar (read (current-buffer)))))
              (should (eq (cdr (assq 'dape-deep-backend settings))
                          'local))
              (should-not (assq 'dape-deep-host settings))
              (should-not (assq 'dape-deep-remote-root settings))
              (should (equal (cdr (assq 'dape-command settings))
                             '(dape-deep-attach))))))
      (delete-directory root t))))

(ert-deftest dape-deep-generated-dir-locals-are-safe-test ()
  (let* ((root (make-temp-file "dape-deep-safe-locals-" t))
         (spec (dape-deep-tests--project-spec root))
         (form (car (read-from-string
                     (dape-deep--dir-locals-content spec))))
         (settings (cdar form)))
    (unwind-protect
        (dolist (entry settings)
          (should (safe-local-variable-p (car entry) (cdr entry))))
      (delete-directory root t))))

(ert-deftest dape-deep-safe-local-handlers-reject-invalid-values-test ()
  (should (safe-local-variable-p 'dape-deep-port 5678))
  (should-not (safe-local-variable-p 'dape-deep-port 0))
  (should-not (safe-local-variable-p 'dape-deep-port 65536))
  (should (safe-local-variable-p 'dape-deep-backend 'ssh))
  (should-not (safe-local-variable-p 'dape-deep-backend 'remote))
  (should (safe-local-variable-p 'dape-deep-forward-ports '(5678 5679)))
  (should-not (safe-local-variable-p 'dape-deep-forward-ports 5678))
  (should-not (safe-local-variable-p 'dape-deep-forward-ports '("5678"))))

(ert-deftest dape-deep-setup-previews-and-confirms-test ()
  (let* ((spec '(:root "/tmp/dape-deep-project/"))
         (plan '((:path "/tmp/dape-deep-project/.project" :status create)))
         displayed
         asked
         applied-plan)
    (cl-letf (((symbol-function 'dape-deep-project-plan)
               (lambda (_spec) plan))
              ((symbol-function 'dape-deep-display-project-plan)
               (lambda (value)
                 (setq displayed value)
                 (get-buffer-create " *dape-deep test plan*")))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) (setq asked t) t))
              ((symbol-function 'dape-deep-apply-project-plan)
               (lambda (value)
                 (setq applied-plan value)
                 '("/tmp/dape-deep-project/.project")))
              ((symbol-function 'dape-deep--revert-setup-buffer) #'ignore)
              ((symbol-function 'dape-deep--inject-setup-buffer) #'ignore))
      (should (equal (dape-deep-setup-project spec)
                     '("/tmp/dape-deep-project/.project")))
      (should (eq displayed plan))
      (should asked)
      (should (eq applied-plan plan))
      ;; The preview does not outlive the question it belongs to.
      (should-not (get-buffer " *dape-deep test plan*")))))

(ert-deftest dape-deep-setup-declined-confirmation-writes-nothing-test ()
  (let* ((spec '(:root "/tmp/dape-deep-project/"))
         (plan '((:path "/tmp/dape-deep-project/.project" :status create))))
    (cl-letf (((symbol-function 'dape-deep-project-plan)
               (lambda (_spec) plan))
              ((symbol-function 'dape-deep-display-project-plan)
               (lambda (_value) (get-buffer-create " *dape-deep test plan*")))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) nil))
              ((symbol-function 'dape-deep-apply-project-plan)
               (lambda (_value) (ert-fail "Declined setup wrote project files")))
              ((symbol-function 'dape-deep--revert-setup-buffer) #'ignore)
              ((symbol-function 'dape-deep--inject-setup-buffer) #'ignore))
      (should-not (dape-deep-setup-project spec))
      (should-not (get-buffer " *dape-deep test plan*")))))

(ert-deftest dape-deep-setup-no-confirm-skips-the-preview-test ()
  (let* ((spec '(:root "/tmp/dape-deep-project/"))
         (plan '((:path "/tmp/dape-deep-project/.project" :status create)
                 (:path "/tmp/dape-deep-project/ty.toml" :status conflict))))
    (cl-letf (((symbol-function 'dape-deep-project-plan)
               (lambda (_spec) plan))
              ((symbol-function 'dape-deep-display-project-plan)
               (lambda (_value)
                 (ert-fail "Unattended setup displayed the plan")))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "Unattended setup asked for confirmation")))
              ((symbol-function 'dape-deep-apply-project-plan)
               (lambda (_value) '("/tmp/dape-deep-project/.project")))
              ((symbol-function 'dape-deep--revert-setup-buffer) #'ignore)
              ((symbol-function 'dape-deep--inject-setup-buffer) #'ignore))
      (should (equal (dape-deep-setup-project spec t)
                     '("/tmp/dape-deep-project/.project"))))))

(ert-deftest dape-deep-setup-refreshes-current-dir-locals-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (script (expand-file-name "debug.py" root))
         (spec (dape-deep-tests--project-spec root))
         reverted-buffer
         revert-args
         buffer)
    (unwind-protect
        (progn
          (write-region (concat "if __name__ == \"__main__\":\n"
                                "    print('test')\n")
                        nil script nil 'silent)
          (setq buffer (find-file-noselect script))
          (let ((original-revert (symbol-function 'revert-buffer)))
            (with-current-buffer buffer
              (setq-local dape-deep-host nil)
              (setq-local dape-deep-remote-python nil)
              (cl-letf (((symbol-function 'revert-buffer)
                         (lambda (&rest args)
                           (setq reverted-buffer (current-buffer))
                           (setq revert-args args)
                           (apply original-revert args))))
                (dape-deep-setup-project spec t))
              (should (eq reverted-buffer buffer))
              (should (equal revert-args '(nil t)))
              (should (equal dape-deep-host "gpu-box"))
              (should (equal dape-deep-remote-python
                             "/root/demo/.venv/bin/python"))
              (should-not (buffer-modified-p))
              (should (= (dape-deep--string-count
                          dape-deep--bootstrap-begin
                          (buffer-string))
                         1))
              (should (string-match-p
                       (regexp-quote
                        "    # dape-deep: end\n\n    print('test')")
                       (dape-deep--read-file script)))
              ;; Re-running setup must not duplicate the managed bootstrap.
              (dape-deep-setup-project spec t)
              (should (= (dape-deep--string-count
                          dape-deep--bootstrap-begin
                          (buffer-string))
                         1)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest dape-deep-local-project-checks-skip-rsync-test ()
  (let ((root (make-temp-file "dape-deep-project-" t)))
    (unwind-protect
        (progn
          (should-not
           (seq-find
            (lambda (check)
              (equal (plist-get check :name) "ty.toml excluded from rsync"))
            (dape-deep--project-checks root 'local)))
          (should
           (seq-find
            (lambda (check)
              (equal (plist-get check :name) "ty.toml excluded from rsync"))
            (dape-deep--project-checks root 'ssh))))
      (delete-directory root t))))

(defun dape-deep-tests--check (checks name)
  "Return the doctor check named NAME in CHECKS, or nil."
  (seq-find (lambda (check) (equal (plist-get check :name) name)) checks))

(ert-deftest dape-deep-setup-without-ty-writes-no-ty-config-test ()
  "A project that does not use ty never sees a ty.toml or an ignore entry."
  (let ((root (make-temp-file "dape-deep-no-ty-" t))
        (dape-deep-local-lsp 'none))
    (unwind-protect
        (let* ((spec (dape-deep-tests--local-project-spec root))
               (plan (dape-deep-project-plan spec))
               (ty-path (expand-file-name "ty.toml" root)))
          (should-not (seq-find (lambda (item)
                                  (equal (plist-get item :path) ty-path))
                                plan))
          (dape-deep-apply-project-plan plan)
          (should-not (file-exists-p ty-path))
          (let ((content (dape-deep--read-file
                          (expand-file-name ".gitignore" root))))
            (should-not (string-match-p (regexp-quote "/ty.toml") content))
            (should (string-match-p (regexp-quote "/.dir-locals.el") content)))
          (should-not (dape-deep-tests--check (dape-deep--project-checks root 'local)
                                              "local ty Python"))
          (should-not (dape-deep-tests--check (dape-deep--project-checks root 'ssh)
                                              "ty.toml excluded from rsync"))
          (should (dape-deep-tests--check (dape-deep--project-checks root 'local)
                                          "local Python executable")))
      (delete-directory root t))))

(ert-deftest dape-deep-setup-without-ty-keeps-an-existing-ty-toml-test ()
  "Turning the option off stops the package from managing a file, not from
keeping it."
  (let ((root (make-temp-file "dape-deep-keep-ty-" t))
        (dape-deep-local-lsp 'none))
    (unwind-protect
        (let* ((path (expand-file-name "ty.toml" root))
               (original "[rules]\nunknown-rule = true\n"))
          (write-region original nil path nil 'silent)
          (let ((plan (dape-deep-project-plan
                       (dape-deep-tests--local-project-spec root))))
            (should-not (seq-find (lambda (item)
                                    (equal (plist-get item :path) path))
                                  plan))
            (dape-deep-apply-project-plan plan))
          (should (equal (dape-deep--read-file path) original)))
      (delete-directory root t))))

(ert-deftest dape-deep-setup-with-ty-writes-its-config-test ()
  "Asking for ty restores the files the package manages for it."
  (let ((root (make-temp-file "dape-deep-ty-" t))
        (dape-deep-local-lsp 'ty))
    (unwind-protect
        (let* ((spec (dape-deep-tests--local-project-spec root))
               (plan (dape-deep-project-plan spec))
               (ty-path (expand-file-name "ty.toml" root)))
          (should (seq-find (lambda (item)
                              (equal (plist-get item :path) ty-path))
                            plan))
          (dape-deep-apply-project-plan plan)
          (should (file-exists-p ty-path))
          (should (string-match-p
                   (regexp-quote "/ty.toml")
                   (dape-deep--read-file (expand-file-name ".gitignore" root))))
          (should (dape-deep-tests--check (dape-deep--project-checks root 'local)
                                          "local ty Python"))
          (should (dape-deep-tests--check (dape-deep--project-checks root 'ssh)
                                          "ty.toml excluded from rsync")))
      (delete-directory root t))))

(ert-deftest dape-deep-checks-accept-an-abbreviated-local-python-test ()
  "The option may name the interpreter the way its own default does."
  (let* ((root (make-temp-file "dape-deep-abbrev-" t))
         (expanded (expand-file-name "~/.emacs.d/.venv/bin/python"))
         (abbreviated (abbreviate-file-name expanded))
         (dape-deep-local-lsp 'ty)
         (dape-deep-local-python abbreviated))
    (unwind-protect
        (progn
          (should-not (equal expanded abbreviated))
          (write-region (concat "# dape-deep: begin local-ty\n"
                                "[environment]\npython = \"" expanded "\"\n"
                                "# dape-deep: end local-ty\n")
                        nil (expand-file-name "ty.toml" root) nil 'silent)
          (should (plist-get (dape-deep-tests--check
                              (dape-deep--project-checks root 'local)
                              "local ty Python")
                             :ok)))
      (delete-directory root t))))

(ert-deftest dape-deep-managed-block-only-file-test ()
  "Only a file that holds nothing but our own block counts as ours."
  (let ((block (dape-deep--managed-block
                "local-ty" "[environment]\npython = \"/venv/bin/python\"\n")))
    (should (dape-deep--managed-block-only-p block "local-ty"))
    (should (dape-deep--managed-block-only-p (concat "\n" block "\n") "local-ty"))
    (should-not (dape-deep--managed-block-only-p
                 (concat block "[rules]\nunknown-rule = true\n") "local-ty"))
    (should-not (dape-deep--managed-block-only-p
                 (concat "# mine\n" block) "local-ty"))
    (should-not (dape-deep--managed-block-only-p
                 "[environment]\npython = \"/venv/bin/python\"\n" "local-ty"))
    (should-not (dape-deep--managed-block-only-p "" "local-ty"))))

(ert-deftest dape-deep-gitignore-follows-ty-toml-ownership-test ()
  "A ty.toml the user has added settings to is not hidden from git."
  (let ((root (make-temp-file "dape-deep-ty-ignore-" t))
        (dape-deep-local-lsp 'ty))
    (unwind-protect
        (let* ((spec (dape-deep-tests--local-project-spec root))
               (ty-path (expand-file-name "ty.toml" root))
               (ignored (lambda ()
                          (dape-deep--read-file
                           (expand-file-name ".gitignore" root)))))
          ;; The package creates the file, so it keeps it out of the way.
          (dape-deep-apply-project-plan (dape-deep-project-plan spec))
          (should (string-match-p (regexp-quote "/ty.toml") (funcall ignored)))
          ;; These tests run in a temporary directory that is not a repository.
          (should (string-match-p (regexp-quote "/dape-deep.sh") (funcall ignored)))
          ;; The user's own settings make the file theirs, and the entry goes.
          (write-region (concat (dape-deep--read-file ty-path)
                                "\n[rules]\nunknown-rule = true\n")
                        nil ty-path nil 'silent)
          (dape-deep-apply-project-plan (dape-deep-project-plan spec))
          (should-not (string-match-p (regexp-quote "/ty.toml") (funcall ignored)))
          (should (string-match-p (regexp-quote "/.dir-locals.el")
                                  (funcall ignored)))
          (should (string-match-p (regexp-quote "unknown-rule")
                                  (dape-deep--read-file ty-path))))
      (delete-directory root t))))

(ert-deftest dape-deep-checks-report-unusable-dir-locals-test ()
  "The doctor must not call a project healthy while Emacs ignores its settings."
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name ".dir-locals.el" root))
         (check (lambda ()
                  (seq-find (lambda (candidate)
                              (equal (plist-get candidate :name)
                                     "directory-local settings"))
                            (dape-deep--project-checks root 'local)))))
    (unwind-protect
        (progn
          (dolist (case '((nil . t)
                          ("((nil . ((dape-deep-port . 5678))))\n" . t)
                          ("((nil\n  (dape-deep-port . 5678)\n" . nil)
                          ("((nil . ((dape-deep-port . 5678))))\n<<<<<<< existing (yours)\n"
                           . nil)))
            (write-region (car case) nil path nil 'silent)
            (should (equal (plist-get (funcall check) :ok) (cdr case)))
            (unless (cdr case)
              (should (stringp (plist-get (funcall check) :detail))))))
      (delete-directory root t))))

(ert-deftest dape-deep-local-doctor-skips-ssh-checks-test ()
  (let ((dape-deep-backend 'local)
        seen-backend
        displayed)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'dape-deep--project-checks)
               (lambda (_root backend)
                 (setq seen-backend backend)
                 '((:name "project" :ok t :detail "ok"))))
              ((symbol-function 'dape-deep--ssh-checks)
               (lambda (&rest _)
                 (ert-fail "Local doctor must not run SSH checks")))
              ((symbol-function 'dape-deep--display-checks)
               (lambda (_title checks) (setq displayed checks))))
      (let ((checks (dape-deep-doctor temporary-file-directory)))
        (should (eq seen-backend 'local))
        (should (equal checks displayed))
        (should (equal checks '((:name "project" :ok t :detail "ok"))))))))

(ert-deftest dape-deep-run-target-uses-project-root-script-test ()
  (let* ((root (make-temp-file "dape-deep-script-" t))
         (nested (expand-file-name "src/package" root))
         (script (expand-file-name dape-deep-target-script root))
         read-command-args
         started-command
         started-cwd)
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "#!/usr/bin/env bash\n" nil script nil 'silent)
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) 'test-project))
                    ((symbol-function 'project-root)
                     (lambda (_project) root))
                    ((symbol-function 'read-shell-command)
                     (lambda (&rest args)
                       (setq read-command-args args)
                       "python custom-target.py --flag"))
                    ((symbol-function 'dape-deep-start)
                     (lambda (command cwd)
                       (setq started-command command
                             started-cwd cwd))))
            (let ((default-directory nested))
              (dape-deep-run-target)))
          (should
           (equal read-command-args
                  '("Run target: " "bash dape-deep.sh"
                    dape-deep-target-command-history)))
          (should (equal started-command "python custom-target.py --flag"))
          (should (equal started-cwd (file-name-as-directory root))))
      (delete-directory root t))))

(ert-deftest dape-deep-run-target-prefers-project-marker-subtree-test ()
  "Run the script beside .project instead of at the enclosing monorepo root."
  (let* ((monorepo (make-temp-file "dape-deep-monorepo-" t))
         (subtree (expand-file-name "projects/engine" monorepo))
         (nested (expand-file-name "src/package" subtree))
         (script (expand-file-name dape-deep-target-script subtree))
         started-cwd)
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "project marker\n" nil
                        (expand-file-name ".project" subtree) nil 'silent)
          (write-region "#!/usr/bin/env bash\n" nil script nil 'silent)
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) 'test-project))
                    ;; Simulate project.el returning a cached outer VCS root.
                    ((symbol-function 'project-root)
                     (lambda (_project) monorepo))
                    ((symbol-function 'read-shell-command)
                     (lambda (&rest _) "python custom-target.py"))
                    ((symbol-function 'dape-deep-start)
                     (lambda (_command cwd) (setq started-cwd cwd))))
            (let ((default-directory nested))
              (dape-deep-run-target)))
          (should (equal started-cwd (file-name-as-directory subtree))))
      (delete-directory monorepo t))))

(ert-deftest dape-deep-project-root-prefers-nearest-marker-test ()
  (let* ((outer (make-temp-file "dape-deep-outer-" t))
         (inner (expand-file-name "projects/engine" outer))
         (nested (expand-file-name "src/package" inner)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "outer\n" nil (expand-file-name ".project" outer)
                        nil 'silent)
          (write-region "inner\n" nil (expand-file-name ".project" inner)
                        nil 'silent)
          (should (equal (dape-deep--project-root nested)
                         (file-name-as-directory inner))))
      (delete-directory outer t))))

(ert-deftest dape-deep-run-target-requires-fixed-script-test ()
  (let ((root (make-temp-file "dape-deep-script-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&rest _) 'test-project))
                  ((symbol-function 'project-root)
                   (lambda (_project) root)))
          (should-error (dape-deep-run-target) :type 'user-error))
      (delete-directory root t))))

(defun dape-deep-tests--normalized (path)
  "Return PATH with a home abbreviation and any symlinks resolved.
`project-root' abbreviates a root that lives below the home directory, which is
where the CI runners keep their temporary directories, and `file-truename'
drops the trailing slash that directory names carry."
  (file-name-as-directory (file-truename (expand-file-name path))))

(ert-deftest dape-deep-project-marker-is-recognized-test ()
  (let* ((root (make-temp-file "dape-deep-marker-" t))
         (nested (expand-file-name "src/package" root))
         (marker (expand-file-name ".project" root)))
    (unwind-protect
        (progn
          (make-directory nested t)
          (write-region "project marker\n" nil marker nil 'silent)
          (let* ((default-directory (file-name-as-directory nested))
                 (project (project-current nil)))
            (should project)
            (should (equal (dape-deep-tests--normalized (project-root project))
                           (dape-deep-tests--normalized root)))))
      (delete-directory root t))))

(ert-deftest dape-deep-merges-into-user-dir-locals-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name ".dir-locals.el" root))
         (original "((python-mode . ((fill-column . 88))))\n"))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (let* ((plan (dape-deep-project-plan
                        (dape-deep-tests--project-spec root)))
                 (item (seq-find (lambda (candidate)
                                   (equal (plist-get candidate :path) path))
                                 plan)))
            (should (eq (plist-get item :status) 'update))
            (dape-deep-apply-project-plan plan)
            (let ((content (dape-deep--read-file path)))
              (should (string-match-p "python-mode" content))
              (should (string-match-p "fill-column" content))
              (should (string-match-p "dape-deep-host" content))
              (should (string-match-p "dape-deep-remote-python" content)))))
      (delete-directory root t))))

(ert-deftest dape-deep-dir-locals-comments-stay-manual-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name ".dir-locals.el" root))
         (original ";; my own settings\n((nil . ((fill-column . 88))))\n"))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (let* ((plan (dape-deep-project-plan
                        (dape-deep-tests--project-spec root)))
                 (item (seq-find (lambda (candidate)
                                   (equal (plist-get candidate :path) path))
                                 plan)))
            (should (eq (plist-get item :status) 'manual))
            (dape-deep-apply-project-plan plan)
            (should (equal (dape-deep--read-file path) original))))
      (delete-directory root t))))

(ert-deftest dape-deep-dir-locals-conflict-writes-markers-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name ".dir-locals.el" root))
         (original "((nil . ((dape-deep-port . 9999))))\n"))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (let* ((plan (dape-deep-project-plan
                        (dape-deep-tests--project-spec root)))
                 (item (seq-find (lambda (candidate)
                                   (equal (plist-get candidate :path) path))
                                 plan)))
            (should (eq (plist-get item :status) 'conflict))
            (dape-deep-apply-project-plan plan)
            (let ((content (dape-deep--read-file path)))
              (should (string-match-p
                       (regexp-quote dape-deep--conflict-start) content))
              (should (string-match-p
                       (regexp-quote dape-deep--conflict-end) content))
              (should (string-match-p "9999" content))
              (should (string-match-p "dape-deep-host" content)))))
      (delete-directory root t))))

(ert-deftest dape-deep-dir-locals-conflict-keeps-every-setting-test ()
  "Both conflict sides list every disagreeing setting, in readable elisp."
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name ".dir-locals.el" root))
         (original
          (concat "((python-mode . ((fill-column . 80)))\n"
                  " (nil . ((dape-deep-port . 9999)\n"
                  "         (dape-deep-host . \"elsewhere\"))))\n")))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (let ((plan (dape-deep-project-plan
                       (dape-deep-tests--project-spec root))))
            (dape-deep-apply-project-plan plan)
            (let* ((content (dape-deep--read-file path))
                   (theirs (progn (string-match
                                   (regexp-quote dape-deep--conflict-start)
                                   content)
                                  (substring content (match-end 0)
                                             (string-match
                                              (regexp-quote dape-deep--conflict-separator)
                                              content))))
                   (ours (progn (string-match
                                 (regexp-quote dape-deep--conflict-separator)
                                 content)
                                (substring content (match-end 0)
                                           (string-match
                                            (regexp-quote dape-deep--conflict-end)
                                            content)))))
              ;; Each side names both disagreeing settings: a side that lost
              ;; entries would silently drop them when the user picks it.
              (dolist (variable '("dape-deep-port" "dape-deep-host"))
                (should (string-match-p variable theirs))
                (should (string-match-p variable ours)))
              (should (string-match-p "9999" theirs))
              (should (string-match-p "elsewhere" theirs))
              (should (string-match-p "5678" ours))
              ;; The group's closing parenthesis sits on its own line, so
              ;; deleting the marker lines leaves a readable form.
              (should (string-match-p
                       (concat (regexp-quote dape-deep--conflict-end) "\n)")
                       content))
              ;; The user's other group survives.
              (should (string-match-p "(python-mode (fill-column . 80))"
                                      content)))))
      (delete-directory root t))))

(defun dape-deep-tests--resolve-conflict (content keep)
  "Remove the conflict markers from CONTENT, keeping the KEEP side.
KEEP is the symbol `yours' or `ours'.  This reproduces what a user does after
setup writes markers: delete the three marker lines and the side not wanted."
  (let (result (skip nil))
    (dolist (line (split-string content "\n"))
      (cond
       ((string-prefix-p dape-deep--conflict-start line)
        (setq skip (not (eq keep 'yours))))
       ((string-prefix-p dape-deep--conflict-separator line)
        (setq skip (eq keep 'yours)))
       ((string-prefix-p dape-deep--conflict-end line)
        (setq skip nil))
       ((not skip)
        (push line result))))
    (mapconcat #'identity (nreverse result) "\n")))

(ert-deftest dape-deep-dir-locals-conflict-resolves-to-readable-elisp-test ()
  "Deleting the marker lines, as git users are used to, leaves valid elisp."
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name ".dir-locals.el" root)))
    (unwind-protect
        (progn
          (write-region "((nil . ((dape-deep-port . 9999))))\n"
                        nil path nil 'silent)
          (dape-deep-apply-project-plan
           (dape-deep-project-plan (dape-deep-tests--project-spec root)))
          (let ((content (dape-deep--read-file path)))
            (should (string-match-p (regexp-quote dape-deep--conflict-start)
                                    content))
            (dolist (case '((yours . 9999) (ours . 5678)))
              (let* ((resolved (dape-deep-tests--resolve-conflict
                                content (car case)))
                     (form (car (read-from-string resolved)))
                     (entries (cdr (assq nil form))))
                (should form)
                (should (equal (cdr (assq 'dape-deep-port entries))
                               (cdr case)))
                ;; The settings the user did not conflict on survive too.
                (should (assq 'dape-deep-backend entries))))))
      (delete-directory root t))))

(ert-deftest dape-deep-ty-conflict-writes-markers-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name "ty.toml" root))
         (original (concat "[environment]\npython = \"/usr/bin/python3\"\n"
                           "\n[rules]\nstrict = [\"all\"]\n")))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (let* ((plan (dape-deep-project-plan
                        (dape-deep-tests--project-spec root)))
                 (item (seq-find (lambda (candidate)
                                   (equal (plist-get candidate :path) path))
                                 plan)))
            (should (eq (plist-get item :status) 'conflict))
            (dape-deep-apply-project-plan plan)
            (let ((content (dape-deep--read-file path)))
              (should (string-match-p
                       (regexp-quote dape-deep--conflict-start) content))
              (should (string-match-p
                       (regexp-quote dape-deep--conflict-end) content))
              (should (string-match-p "python3" content))
              (should (string-match-p "dape-deep: begin local-ty" content))
              ;; Everything outside the conflicting table survives, with its
              ;; separation from the marked block intact.
              (should (string-match-p
                       (regexp-quote
                        (concat dape-deep--conflict-end "\n\n[rules]\nstrict = [\"all\"]\n"))
                       content)))
            ;; A conflict no longer stops setup from writing the other files.
            (should (file-exists-p (expand-file-name ".project" root)))))
      (delete-directory root t))))

(ert-deftest dape-deep-ty-conflict-without-environment-section-test ()
  "A damaged file without an [environment] table gains the marked block."
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name "ty.toml" root))
         (original (concat "# dape-deep: begin local-ty\n"
                           "[rules]\nstrict = [\"all\"]\n")))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (let ((plan (dape-deep-project-plan
                       (dape-deep-tests--project-spec root))))
            (dape-deep-apply-project-plan plan)
            (let ((content (dape-deep--read-file path)))
              (should (string-match-p
                       (regexp-quote dape-deep--conflict-start) content))
              (should (string-match-p "strict = \\[\"all\"\\]" content)))))
      (delete-directory root t))))

(ert-deftest dape-deep-ty-managed-block-keeps-other-tables-test ()
  "Rewriting the managed block leaves unrelated tables untouched."
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (path (expand-file-name "ty.toml" root))
         (original (concat "[rules]\nstrict = [\"all\"]\n\n"
                           "# dape-deep: begin local-ty\n[environment]\n"
                           "python = \"/usr/bin/python3\"\n"
                           "# dape-deep: end local-ty\n")))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (let ((plan (dape-deep-project-plan
                       (dape-deep-tests--project-spec root))))
            (dape-deep-apply-project-plan plan)
            (let ((content (dape-deep--read-file path)))
              (should (string-match-p "strict = \\[\"all\"\\]" content))
              (should (string-match-p "dape-deep: begin local-ty" content))
              (should-not (string-match-p
                           (regexp-quote dape-deep--conflict-start) content)))))
      (delete-directory root t))))

(ert-deftest dape-deep-removes-generated-legacy-dir-locals-2-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (legacy (expand-file-name ".dir-locals-2.el" root)))
    (unwind-protect
        (progn
          (write-region
           (concat dape-deep--dir-locals-header
                   "((nil . ((dape-deep-host . \"old-host\"))))\n")
           nil legacy nil 'silent)
          (let* ((plan (dape-deep-project-plan
                        (dape-deep-tests--project-spec root)))
                 (item (seq-find (lambda (candidate)
                                   (equal (plist-get candidate :path) legacy))
                                 plan)))
            (should (eq (plist-get item :status) 'delete))
            (dape-deep-apply-project-plan plan)
            (should-not (file-exists-p legacy))
            (should (file-exists-p (expand-file-name ".dir-locals.el" root)))))
      (delete-directory root t))))

(ert-deftest dape-deep-reports-user-owned-legacy-dir-locals-2-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (legacy (expand-file-name ".dir-locals-2.el" root))
         (original "((nil . ((some-user-setting . t))))\n"))
    (unwind-protect
        (progn
          (write-region original nil legacy nil 'silent)
          (let* ((plan (dape-deep-project-plan
                        (dape-deep-tests--project-spec root)))
                 (item (seq-find (lambda (candidate)
                                   (equal (plist-get candidate :path) legacy))
                                 plan)))
            (should (eq (plist-get item :status) 'manual))
            ;; Setup still completes; the user-owned file is reported, not
            ;; touched.
            (dape-deep-apply-project-plan plan)
            (should (equal (dape-deep--read-file legacy) original))
            (should (file-exists-p (expand-file-name ".project" root)))))
      (delete-directory root t))))

(ert-deftest dape-deep-gitignore-created-for-package-artifacts-test ()
  (let ((root (make-temp-file "dape-deep-gitignore-" t)))
    (unwind-protect
        (let* ((spec (dape-deep-tests--local-project-spec root))
               (plan (dape-deep-project-plan spec)))
          (dape-deep-apply-project-plan plan)
          (let ((content (dape-deep--read-file
                          (expand-file-name ".gitignore" root))))
            (dolist (pattern (dape-deep--gitignore-patterns t))
              (should (string-match-p (regexp-quote pattern) content)))
            (should (string-match-p
                     (regexp-quote dape-deep--gitignore-begin) content)))
          ;; A second pass leaves the generated file alone.
          (should (seq-every-p
                   (lambda (item) (eq (plist-get item :status) 'unchanged))
                   (dape-deep-project-plan spec))))
      (delete-directory root t))))

(ert-deftest dape-deep-gitignore-preserves-user-entries-test ()
  (let* ((root (make-temp-file "dape-deep-gitignore-" t))
         (path (expand-file-name ".gitignore" root))
         (original "__pycache__\n*.pth\nwandb\n"))
    (unwind-protect
        (progn
          (write-region original nil path nil 'silent)
          (dape-deep-apply-project-plan
           (dape-deep-project-plan (dape-deep-tests--local-project-spec root)))
          (let ((content (dape-deep--read-file path)))
            (should (string-prefix-p original content))
            (should (string-match-p (regexp-quote "/ty.toml") content)))
          ;; Re-running updates the managed block instead of duplicating it.
          (dape-deep-apply-project-plan
           (dape-deep-project-plan (dape-deep-tests--local-project-spec root)))
          (let ((content (dape-deep--read-file path)))
            (should (= (dape-deep--string-count dape-deep--gitignore-begin
                                               content)
                       1))
            (should (string-prefix-p original content))))
      (delete-directory root t))))

(ert-deftest dape-deep-refuses-stale-project-plan-test ()
  (let* ((root (make-temp-file "dape-deep-project-" t))
         (project-file (expand-file-name ".project" root))
         (plan (dape-deep-project-plan
                (dape-deep-tests--project-spec root))))
    (unwind-protect
        (progn
          (write-region "changed after preview\n" nil project-file nil 'silent)
          (should-error (dape-deep-apply-project-plan plan)
                        :type 'user-error)
          (should-not (file-exists-p (expand-file-name "ty.toml" root))))
      (delete-directory root t))))

(ert-deftest dape-deep-parses-effective-ssh-config-test ()
  (let ((config
         (dape-deep--parse-ssh-g
          (concat "host gpu-box\ncontrolmaster auto\ncontrolpersist 600\n"
                  "controlpath /tmp/cm-hash\nserveraliveinterval 30\n"))))
    (should (equal (cdr (assoc "controlmaster" config)) "auto"))
    (should (equal (cdr (assoc "controlpersist" config)) "600"))))

(defun dape-deep-tests--good-ssh-config (_host)
  "Return a valid fake ssh -G result."
  '(("controlmaster" . "auto")
    ("controlpersist" . "600")
    ("controlpath" . "/tmp/dape-rd-hash")
    ("serveraliveinterval" . "30")
    ("serveralivecountmax" . "3")))

(defun dape-deep-tests--ssh-config-with-connection (host hostname user port)
  "Return a fake ssh -G result for HOST with HOSTNAME, USER, and PORT."
  (append (dape-deep-tests--good-ssh-config host)
          (list (cons "hostname" hostname)
                (cons "user" user)
                (cons "port" port))))

(ert-deftest dape-deep-configures-managed-ssh-files-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "dape-deep.conf" ssh-directory))
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (write-region "Host old-box\n    HostName example.test\n" nil main nil 'silent)
          (cl-letf (((symbol-function 'dape-deep-ssh-effective-config)
                     #'dape-deep-tests--good-ssh-config))
            (dape-deep-ssh-configure "gpu-box" t))
          (should
           (string-prefix-p
            dape-deep--ssh-include-begin
            (dape-deep--read-file main)))
          (should
           (string-match-p
            "Host gpu-box"
            (dape-deep--read-file managed)))
          (should (= (file-modes main) #o600))
          (should (= (file-modes managed) #o600))
          (cl-letf (((symbol-function 'dape-deep-ssh-effective-config)
                     #'dape-deep-tests--good-ssh-config))
            (dape-deep-ssh-configure "gpu-box" t))
          (should (= (with-temp-buffer
                       (insert-file-contents managed)
                       (how-many "begin host gpu-box"))
                     1)))
      (delete-directory root t))))

(ert-deftest dape-deep-rolls-back-invalid-ssh-change-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "managed.conf" ssh-directory))
         (original "Host gpu-box\n    HostName example.test\n")
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (write-region original nil main nil 'silent)
          (cl-letf (((symbol-function 'dape-deep-ssh-effective-config)
                     (lambda (_host) '(("controlmaster" . "no")))))
            (should-error
             (dape-deep-ssh-configure "gpu-box" t)))
          (should (equal (dape-deep--read-file main) original))
          (should-not (file-exists-p managed)))
      (delete-directory root t))))

(ert-deftest dape-deep-refuses-ambiguous-ssh-markers-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "managed.conf" ssh-directory))
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (write-region dape-deep--ssh-include-begin
                        nil main nil 'silent)
          (should-error (dape-deep-ssh-configure "gpu-box" t)
                        :type 'user-error)
          (should-not (file-exists-p managed)))
      (delete-directory root t))))

(ert-deftest dape-deep-ssh-add-writes-connection-and-enhancement-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "dape-deep.conf" ssh-directory))
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed)
         (connection '(:hostname "10.0.0.5" :user "root" :port "2222")))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (cl-letf (((symbol-function 'dape-deep-ssh-effective-config)
                     (lambda (host)
                       (dape-deep-tests--ssh-config-with-connection
                        host "10.0.0.5" "root" "2222")))
                    ((symbol-function 'dape-deep--probe-ssh-login)
                     (lambda (host) 0)))
            (dape-deep-ssh-add "gpu-box" connection t))
          (should
           (string-prefix-p
            dape-deep--ssh-include-begin
            (dape-deep--read-file main)))
          (should
           (string-suffix-p
            (dape-deep--ssh-connection-block
             (dape-deep--ssh-connection-normalize "gpu-box" connection))
            (dape-deep--read-file main)))
          (should
           (string-match-p
            "Host gpu-box"
            (dape-deep--read-file managed)))
          (should (= (file-modes main) #o600))
          (should (= (file-modes managed) #o600)))
      (delete-directory root t))))

(ert-deftest dape-deep-ssh-preview-does-not-outlive-the-answer-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-preview-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "dape-deep.conf" ssh-directory))
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed)
         (connection '(:hostname "10.0.0.5" :user "root" :port "2222")))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (when-let* ((stale (get-buffer "*dape-deep diff*")))
            (kill-buffer stale))
          (cl-letf (((symbol-function 'dape-deep--probe-ssh-login)
                     (lambda (_host) 0)))
            ;; Unattended callers have nothing to confirm, so they are not given
            ;; a preview to clean up either.
            (dape-deep-ssh-add "gpu-box" connection t)
            (should-not (get-buffer "*dape-deep diff*"))
            ;; A declined prompt removes the preview it displayed.
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
              (dape-deep-ssh-add "gpu-box" connection))
            (should-not (get-buffer "*dape-deep diff*"))
            ;; A confirmed prompt removes it before anything is written.
            (let (preview-at-write)
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                        ((symbol-function 'dape-deep--ssh-backup)
                         (lambda (_path)
                           (setq preview-at-write
                                 (get-buffer "*dape-deep diff*")))))
                (dape-deep-ssh-add "gpu-box" connection))
              (should-not preview-at-write)))
          (should-not (get-buffer "*dape-deep diff*")))
      (when-let* ((preview (get-buffer "*dape-deep diff*")))
        (kill-buffer preview))
      (delete-directory root t))))

(ert-deftest dape-deep-ssh-add-is-idempotent-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "managed.conf" ssh-directory))
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed)
         (fake (dape-deep-tests--ssh-config-with-connection
                "gpu-box" "10.0.0.5" "root" "2222")))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (cl-letf (((symbol-function 'dape-deep-ssh-effective-config)
                     (lambda (host) fake))
                    ((symbol-function 'dape-deep--probe-ssh-login)
                     (lambda (host) 0)))
            (dape-deep-ssh-add
             "gpu-box" '(:hostname "10.0.0.5" :user "root" :port "2222") t)
            (setq fake (dape-deep-tests--ssh-config-with-connection
                        "gpu-box" "10.0.0.6" "root" "2222"))
            (dape-deep-ssh-add
             "gpu-box" '(:hostname "10.0.0.6" :user "root" :port "2222") t))
          (let ((main-content (dape-deep--read-file main)))
            (cl-labels ((count (needle)
                          (with-temp-buffer
                            (insert main-content)
                            (goto-char (point-min))
                            (how-many needle))))
              (should (= (count dape-deep--ssh-include-begin) 1))
              (should (= (count "begin connection gpu-box") 1))
              (should (= (count "HostName 10.0.0.6") 1)))
            (should-not (string-match-p "10\\.0\\.0\\.5" main-content))))
      (delete-directory root t))))

(ert-deftest dape-deep-ssh-add-rolls-back-shadowed-connection-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "managed.conf" ssh-directory))
         (original "Host gpu-box\n    HostName shadow.test\n")
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed)
         main-modes)
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (write-region original nil main nil 'silent)
          ;; The package writes its ssh configuration with 0600; a rollback has
          ;; to give the user's file its own mode back, not that one.
          (set-file-modes main #o644)
          (setq main-modes (file-modes main))
          (cl-letf (((symbol-function 'dape-deep-ssh-effective-config)
                     (lambda (host)
                       (append
                        (dape-deep-tests--good-ssh-config host)
                        (list (cons "hostname" "shadow.test"))))))
            (should-error
             (dape-deep-ssh-add
              "gpu-box" '(:hostname "10.0.0.5" :user "root") t)))
          (should (equal (dape-deep--read-file main) original))
          (should (equal (file-modes main) main-modes))
          (should-not (file-exists-p managed)))
      (delete-directory root t))))

(ert-deftest dape-deep-ssh-add-refuses-ambiguous-connection-markers-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "managed.conf" ssh-directory))
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (write-region (dape-deep--ssh-connection-begin "gpu-box")
                        nil main nil 'silent)
          (should-error
           (dape-deep-ssh-add "gpu-box" '(:hostname "10.0.0.5") t)
           :type 'user-error)
          (should-not (file-exists-p managed)))
      (delete-directory root t))))

(ert-deftest dape-deep-ssh-add-rejects-unsafe-values-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (ssh-directory (expand-file-name ".ssh" root))
         (main (expand-file-name "config" ssh-directory))
         (managed (expand-file-name "managed.conf" ssh-directory))
         (dape-deep-ssh-config-file main)
         (dape-deep-ssh-managed-config-file managed))
    (unwind-protect
        (progn
          (make-directory ssh-directory)
          (dolist (connection '((:hostname "a b")
                                (:user "a:b")
                                (:port "0")
                                (:port "70000")
                                (:port "abc")
                                ()))
            (should-error (dape-deep-ssh-add "gpu-box" connection t)
                          :type 'user-error))
          (should-not (file-exists-p main))
          (should-not (file-exists-p managed)))
      (delete-directory root t))))

(ert-deftest dape-deep-ssh-login-probe-command-test ()
  (let* ((root (make-temp-file "dape-deep-ssh-" t))
         (config (expand-file-name "config" root))
         (dape-deep-ssh-config-file config))
    (unwind-protect
        (let ((argv (dape-deep--ssh-login-probe-command "gpu-box")))
          (should (equal (car argv) dape-deep-ssh-program))
          (should (member "-T" argv))
          (should (member "BatchMode=yes" argv))
          (should (member "StrictHostKeyChecking=accept-new" argv))
          (should (seq-find (lambda (argument)
                              (string-prefix-p "ConnectTimeout=" argument))
                            argv))
          (should (member "-F" argv))
          (should (member (dape-deep--ssh-config-path config) argv))
          (should-not (member "-t" argv))
          (should-not (member "-tt" argv))
          (should (member "ExitOnForwardFailure=yes" argv))
          (should (member "-S" argv))
          (should (member "none" argv))
          (should (equal (last argv 3) '("--" "gpu-box" "true"))))
      (delete-directory root t))))

(provide 'dape-deep-tests)

;; A checkout installed as a package is compiled file by file, and the fakes
;; this suite requires sit outside `load-path' for that compiler.  Nothing needs
;; a compiled suite, and `no-byte-compile' also keeps native compilation away.
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dape-deep-tests.el ends here
