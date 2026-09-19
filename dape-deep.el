;;; dape-deep.el --- Local and remote debugpy attach workflows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Di Xiu

;; Author: Di Xiu <dyi.shiou@gmail.com>
;; Assisted-by: Claude Code:claude-fable-5
;; Assisted-by: Claude Code:deepseek-v4.1-flash
;; Assisted-by: Codex:gpt-5.6-sol
;; Maintainer: Di Xiu <dyi.shiou@gmail.com>
;; URL: https://github.com/lemyx/dape-deep
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dape "0.27.1"))
;; Keywords: tools, processes
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
;; `dape-deep' debugs a Python target from Emacs with Dape, whether that target
;; is a process on this machine or a PyTorch training job on an SSH host.
;;
;; Set a project up once with `dape-deep-setup-project'.  It shows the plan
;; first, then writes .project, .gitignore, .dir-locals.el, and a launcher
;; script into the project, and injects a guarded debugpy bootstrap into the
;; entry file's `__main__' guard.  `dape-deep-doctor' reports what still needs
;; attention; `dape-deep-local-lsp' decides whether setup also maintains a
;; ty.toml for the local language server.
;;
;; Then `dape-deep-run-target' synchronizes the source, starts the target, and
;; waits for the debugger.  Attach with `M-x dape' and debug as usual:
;; breakpoints, stepping, variables, and the REPL.
;;
;; The SSH backend keeps the tunnel in the target's process, so stopping the
;; target closes it, and forwards only the debug port to the remote localhost:
;;
;;   local source --rsync--> remote source
;;   local Dape   --ssh -L--> remote debug adapter
;;
;; Source editing, language tooling, and project navigation stay local, and
;; Dape's prefix-local/prefix-remote support translates breakpoint and
;; stack-frame paths in both directions.
;;
;; See https://github.com/lemyx/dape-deep for the full documentation.

;;; Code:

(require 'dape)
(require 'dape-deep-bootstrap)
(require 'dape-deep-config)
(require 'dape-deep-project)
(require 'dape-deep-process)
(require 'dape-deep-ssh)
(require 'subr-x)

(defvar dape-deep-target-command-history nil
  "History for `dape-deep-run-target'.")

;;;###autoload
(defun dape-deep-start (target-command &optional cwd)
  "Start TARGET-COMMAND locally or on `dape-deep-host'.
CWD defaults to `dape-cwd'.  In remote mode, synchronize first when
`dape-deep-sync-before-run' is non-nil."
  (unless (and (stringp target-command)
               (not (string-empty-p (string-trim target-command))))
    (user-error "TARGET-COMMAND must be a non-empty string"))
  (let* ((target-command
         (format (concat "DAPE_DEEP=1 "
                          "DAPE_DEEP_PORT=%d "
                          "DAPE_DEEP_RANK=%d; "
                          "export DAPE_DEEP "
                          "DAPE_DEEP_PORT "
                          "DAPE_DEEP_RANK; %s")
                  dape-deep-port
                  dape-deep-rank
                  target-command))
         (cwd (file-name-as-directory
               (expand-file-name (or cwd (dape-cwd)))))
         (settings (dape-deep--settings))
         (remote-p (eq (plist-get settings :backend) 'ssh))
         (process-command
          (if remote-p
              (dape-deep--ssh-command target-command cwd settings)
            (list shell-file-name shell-command-switch target-command))))
    ;; Capture CWD and PROCESS-COMMAND before rsync.  Its sentinel runs with
    ;; the sync buffer current, where project-local values are unavailable.
    (if (and remote-p dape-deep-sync-before-run)
        (dape-deep-sync
         (lambda ()
           (dape-deep--start-target-process process-command cwd settings))
         settings)
      (dape-deep--start-target-process process-command cwd settings))))

;;;###autoload
(defun dape-deep-run-target ()
  "Prompt for and start a target command from the project root.
The minibuffer is prefilled with a command that runs the project-root
`dape-deep-target-script'."
  (interactive)
  (let* ((root (dape-deep--project-root))
         (script (expand-file-name dape-deep-target-script root))
         (default-command
          (format "bash %s"
                  (shell-quote-argument dape-deep-target-script))))
    (unless (and (file-regular-p script) (file-readable-p script))
      (user-error "Create the debug target script first: %s" script))
    (dape-deep-start
     (read-shell-command
     (if (eq (dape-deep--effective-backend) 'ssh)
          (format "Run target [on %s]: " dape-deep-host)
        "Run target: ")
      default-command
      'dape-deep-target-command-history)
     root)))

;;;###autoload
(defun dape-deep-doctor (&optional root host)
  "Run project checks and, for the SSH backend, host checks for ROOT and HOST.
This command does not connect to HOST or modify any files."
  (interactive)
  (let* ((root (or root (dape-deep--project-root)))
         (backend (dape-deep--effective-backend))
         (host (or host dape-deep-host))
         (checks (append
                  (dape-deep--project-checks root backend)
                  (when (eq backend 'ssh)
                    (dape-deep--ssh-checks host)))))
    (dape-deep--display-checks
     (format "dape-deep checks for %s" (expand-file-name root))
     checks)
    checks))

(defun dape-deep--ensure-dape-config (_config)
  "Validate local or SSH settings for a Dape CONFIG."
  (dape-deep--settings))

(defun dape-deep--configure-attach (config)
  "Add SSH path mapping to Dape CONFIG when the selected backend needs it."
  (let ((settings (dape-deep--settings)))
    (if (eq (plist-get settings :backend) 'ssh)
        (thread-first config
                      (plist-put 'prefix-local
                                 (plist-get settings :local-root))
                      (plist-put 'prefix-remote
                                 (plist-get settings :remote-root)))
      config)))

(defun dape-deep-register-debugpy-config ()
  "Register `dape-deep-attach' unless Dape already has that name.
An existing configuration is preserved so loading this package cannot replace
a working user configuration."
  (let ((config
         '(modes (python-mode python-ts-mode)
           fn dape-deep--configure-attach
           ensure dape-deep--ensure-dape-config
           host "localhost"
           port dape-deep-port
           :request "attach"
           :type "python"
           :justMyCode nil
           :redirectOutput nil)))
    (unless (assq 'dape-deep-attach dape-configs)
      (push (cons 'dape-deep-attach config) dape-configs))))

(dape-deep-register-debugpy-config)

(provide 'dape-deep)

;;; dape-deep.el ends here
