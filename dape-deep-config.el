;;; dape-deep-config.el --- Configuration for dape-deep -*- lexical-binding: t; -*-

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
;; User options, validation, and local-to-remote path mapping for
;; `dape-deep'.

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup dape-deep nil
  "Start local or remote Python targets and attach with Dape."
  :prefix "dape-deep-"
  :group 'tools)

(defcustom dape-deep-backend 'auto
  "Backend used to run the debug target.
`local' runs the target directly, `ssh' synchronizes and runs it through SSH,
and `auto' selects SSH when `dape-deep-host' is non-empty."
  :type '(choice (const :tag "Infer from SSH host" auto)
                 (const :tag "Local process" local)
                 (const :tag "SSH and rsync" ssh))
  :group 'dape-deep)

(defcustom dape-deep-host nil
  "SSH destination used to run the remote debug target.
This can be a Host alias from ssh_config.  With the `auto' backend, a nil value
makes `dape-deep-run-target' run locally."
  :type '(choice (const :tag "Run locally" nil) string)
  :group 'dape-deep)

(defcustom dape-deep-local-root nil
  "Local root corresponding to `dape-deep-remote-root'.
Use the monorepo root when breakpoints may span sibling projects."
  :type '(choice (const :tag "Unset" nil) directory)
  :group 'dape-deep)

(defcustom dape-deep-remote-root nil
  "Absolute source root on `dape-deep-host'."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'dape-deep)

(defconst dape-deep--default-local-python
  (locate-user-emacs-file ".venv/bin/python")
  "Built-in default for `dape-deep-local-python'.
The virtual environment lives below `user-emacs-directory'.  Project setup
reuses this value when the option is unset.")

(defcustom dape-deep-local-python dape-deep--default-local-python
  "Python interpreter used by local targets and local editor tooling.
In SSH mode the remote target instead uses `dape-deep-remote-python'.
The default is the `.venv' interpreter below `user-emacs-directory'."
  :type '(choice (const :tag "Unset" nil) file)
  :group 'dape-deep)

(defcustom dape-deep-remote-python nil
  "Absolute Python interpreter path on `dape-deep-host'.
Project setup writes this value to the generated `dape-deep-target-script'."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'dape-deep)

(defcustom dape-deep-python-version nil
  "Python major/minor version written by project setup, such as \"3.12\"."
  :type '(choice (const :tag "Infer" nil) string)
  :group 'dape-deep)

(defcustom dape-deep-local-lsp 'none
  "Local language server that project setup writes a configuration for.
`none', the default, writes nothing: the package does not depend on a language
server, and debugging works without one.  `ty' maintains the ty.toml that points
ty at `dape-deep-local-python', the eglot and ty combination this package is
developed with.  Any other server is configured outside this package, because
each one names its interpreter in its own file."
  :type '(choice (const :tag "None" none)
                 (const :tag "Eglot with ty" ty))
  :group 'dape-deep)

(defconst dape-deep-target-script "dape-deep.sh"
  "Fixed project-root script generated and used to start the debug target.")

(defcustom dape-deep-port 5678
  "Local and remote debug-adapter port used by the default configuration."
  :type 'natnum
  :group 'dape-deep)

(defcustom dape-deep-forward-ports nil
  "Extra ports forwarded over SSH, in addition to `dape-deep-port'.
Set a list of ports to debug multiple ranks concurrently; the debug-adapter
port itself is always forwarded."
  :type '(repeat natnum)
  :group 'dape-deep)

(defcustom dape-deep-rank 0
  "Distributed rank that an injected debugpy bootstrap should pause."
  :type 'natnum
  :group 'dape-deep)

(defcustom dape-deep-sync-before-run t
  "Whether to synchronize source before starting a remote target."
  :type 'boolean
  :group 'dape-deep)

(defcustom dape-deep-rsync-program "rsync"
  "Rsync executable used by `dape-deep-sync'."
  :type 'string
  :group 'dape-deep)

(defcustom dape-deep-rsync-arguments '("-az" "--info=stats1")
  "Arguments passed to rsync before exclude and source arguments."
  :type '(repeat string)
  :group 'dape-deep)

(defcustom dape-deep-rsync-excludes
  '(".git" ".venv" "ty.toml" "__pycache__" "*.pyc" ".mypy_cache" ".ruff_cache"
    "wandb" "checkpoints" "outputs" "*.egg-info")
  "Patterns excluded by `dape-deep-sync'.
The package deliberately does not pass --delete, so remote checkpoints and
logs are not removed."
  :type '(repeat string)
  :group 'dape-deep)

(defcustom dape-deep-ssh-program "ssh"
  "SSH executable used to start the target and tunnel."
  :type 'string
  :group 'dape-deep)

(defcustom dape-deep-ssh-arguments
  '("-tt" "-o" "ExitOnForwardFailure=yes" "-S" "none")
  "SSH arguments placed before port-forwarding arguments.
The default disables connection sharing for the target SSH process so its
forwarded ports close with the target.  Rsync can still use a persistent SSH
control connection from ssh_config."
  :type '(repeat string)
  :group 'dape-deep)

(defcustom dape-deep-ssh-probe-timeout 5
  "Seconds to wait while detecting the Python interpreter on an SSH host.
Detection is best effort.  Project setup falls back to an existing configured
value or manual input when the probe fails or times out."
  :type 'natnum
  :group 'dape-deep)

(defcustom dape-deep-shell-command '("bash" "-lc")
  "Remote shell and arguments used to run the target script.
The default login shell loads profile.d settings commonly used to restore a
container or GPU image's Python environment.  Environment selection and target
arguments belong in `dape-deep-target-script'."
  :type '(repeat string)
  :group 'dape-deep)

(defcustom dape-deep-target-buffer "*dape-shell*"
  "Buffer that receives target stdout and stderr."
  :type 'string
  :group 'dape-deep)

(defcustom dape-deep-sync-buffer "*dape-sync*"
  "Buffer that receives rsync output."
  :type 'string
  :group 'dape-deep)

(defun dape-deep--port-p (port)
  "Return non-nil when PORT is a valid TCP port."
  (and (integerp port) (<= 1 port) (<= port 65535)))

(defun dape-deep--port-list-p (ports)
  "Return non-nil when PORTS is a list of valid TCP ports."
  (and (listp ports) (seq-every-p #'dape-deep--port-p ports)))

(defun dape-deep--backend-p (backend)
  "Return non-nil when BACKEND is a supported backend symbol."
  (memq backend '(auto local ssh)))

(defun dape-deep--literal-absolute-path-p (path)
  "Return non-nil when PATH is absolute and names no home directory.
A leading \"~\" is expanded by a shell but not by rsync, nor by the generated
launcher, which quotes its arguments, so a path that starts with one would mean
different things on the two sides of an SSH connection."
  (and (stringp path)
       (file-name-absolute-p path)
       (not (string-prefix-p "~" path))))

;; Register these properties from the generated autoloads file, before a
;; project buffer can cause Emacs to validate its .dir-locals.el.  Dape also
;; declares dape-command safe in its own autoloads; repeat it here because this
;; package writes that variable and can be installed or loaded independently.
;;
;; This form runs at startup, before this library is loaded, so its handlers
;; are self-contained; keep them in sync with `dape-deep--backend-p',
;; `dape-deep--port-p', and `dape-deep--port-list-p'.
;;;###autoload
(dolist (entry
         '((dape-command . listp)
           (dape-deep-backend . (lambda (value)
                                     (memq value '(auto local ssh))))
           (dape-deep-host . string-or-null-p)
           (dape-deep-local-root . string-or-null-p)
           (dape-deep-remote-root . string-or-null-p)
           (dape-deep-local-python . string-or-null-p)
           (dape-deep-remote-python . string-or-null-p)
           (dape-deep-python-version . string-or-null-p)
           (dape-deep-local-lsp . (lambda (value)
                                      (memq value '(none ty))))
           (dape-deep-port . (lambda (value)
                                  (and (integerp value)
                                       (<= 1 value)
                                       (<= value 65535))))
           (dape-deep-rank . natnump)
           (dape-deep-sync-before-run . booleanp)
           (dape-deep-forward-ports
            . (lambda (value)
                (and (listp value)
                     (catch 'invalid
                       (dolist (port value t)
                         (unless (and (integerp port)
                                      (<= 1 port)
                                      (<= port 65535))
                           (throw 'invalid nil)))))))))
  (put (car entry) 'safe-local-variable (cdr entry)))

(defun dape-deep--effective-backend (&optional backend)
  "Return the concrete backend selected by BACKEND and current settings."
  (let ((backend (or backend dape-deep-backend)))
    (unless (dape-deep--backend-p backend)
      (user-error "Invalid `dape-deep-backend': %S" backend))
    (if (eq backend 'auto)
        (if (and (stringp dape-deep-host)
                 (not (string-empty-p dape-deep-host)))
            'ssh
          'local)
      backend)))

(defun dape-deep--settings ()
  "Validate and return normalized local or SSH settings as a plist."
  (unless (dape-deep--port-p dape-deep-port)
    (user-error "Invalid `dape-deep-port': %S" dape-deep-port))
  (unless (natnump dape-deep-rank)
    (user-error "Invalid `dape-deep-rank': %S" dape-deep-rank))
  (unless (dape-deep--port-list-p dape-deep-forward-ports)
    (user-error "Invalid `dape-deep-forward-ports': %S"
                dape-deep-forward-ports))
  (let ((backend (dape-deep--effective-backend)))
    (if (eq backend 'local)
        (list :backend 'local
              :port dape-deep-port
              :rank dape-deep-rank)
      (unless (and (stringp dape-deep-host)
                   (not (string-empty-p dape-deep-host)))
        (user-error "Set `dape-deep-host' for the SSH backend"))
      (when (or (string-prefix-p "-" dape-deep-host)
                (string-match-p "[[:space:]]" dape-deep-host))
        (user-error "Invalid SSH destination: %S" dape-deep-host))
      (unless (and (stringp dape-deep-local-root)
                   (not (string-empty-p dape-deep-local-root)))
        (user-error "Set `dape-deep-local-root' for the SSH backend"))
      (unless (and (stringp dape-deep-remote-root)
                   (not (string-empty-p dape-deep-remote-root)))
        (user-error
         "Set `dape-deep-remote-root' for the SSH backend"))
      (unless (dape-deep--literal-absolute-path-p dape-deep-remote-root)
        (user-error
         "`dape-deep-remote-root' must be absolute and without \"~\": %S"
         dape-deep-remote-root))
      (let ((local-root
             (file-name-as-directory
              (expand-file-name dape-deep-local-root))))
        (unless (file-directory-p local-root)
          (user-error "Local source root does not exist: %s" local-root))
        (list :backend 'ssh
              :host dape-deep-host
              :local-root local-root
              :remote-root (file-name-as-directory
                            dape-deep-remote-root)
              :port dape-deep-port
              :rank dape-deep-rank
              :ports (seq-uniq (cons dape-deep-port
                                     dape-deep-forward-ports)))))))

(defun dape-deep--remote-settings ()
  "Return validated SSH settings or signal when the backend is local."
  (let ((settings (dape-deep--settings)))
    (unless (eq (plist-get settings :backend) 'ssh)
      (user-error "The current debug target uses the local backend"))
    settings))

(defun dape-deep--remote-cwd (cwd settings)
  "Translate local CWD to a remote directory using SETTINGS.
Fall back to the remote root when CWD is outside the local mapping root."
  (let ((local-root (plist-get settings :local-root))
        (remote-root (plist-get settings :remote-root))
        (local-cwd (file-name-as-directory (expand-file-name cwd))))
    (if (string-prefix-p local-root local-cwd)
        (concat remote-root (string-remove-prefix local-root local-cwd))
      remote-root)))

(provide 'dape-deep-config)

;;; dape-deep-config.el ends here
