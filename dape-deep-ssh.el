;;; dape-deep-ssh.el --- SSH diagnostics and managed setup -*- lexical-binding: t; -*-

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
;; Inspect effective OpenSSH configuration with ssh -G.  Optional repair is
;; isolated in a package-managed include file, with preview, backups, atomic
;; writes, validation, and rollback.

;;; Code:

(require 'dape-deep-bootstrap)
(require 'dape-deep-config)
(require 'dape-deep-project)
(require 'subr-x)

(defcustom dape-deep-ssh-config-file "~/.ssh/config"
  "Main OpenSSH client configuration file."
  :type 'file
  :group 'dape-deep)

(defcustom dape-deep-ssh-managed-config-file
  "~/.ssh/dape-deep.conf"
  "OpenSSH configuration file managed by this package."
  :type 'file
  :group 'dape-deep)

(defcustom dape-deep-ssh-control-persist "10m"
  "ControlPersist value installed by SSH configuration repair."
  :type 'string
  :group 'dape-deep)

(defcustom dape-deep-ssh-server-alive-interval 30
  "ServerAliveInterval installed by SSH configuration repair."
  :type 'natnum
  :group 'dape-deep)

(defcustom dape-deep-ssh-server-alive-count-max 3
  "ServerAliveCountMax installed by SSH configuration repair."
  :type 'natnum
  :group 'dape-deep)

(defconst dape-deep--ssh-include-begin
  "# dape-deep: begin managed include")

(defconst dape-deep--ssh-include-end
  "# dape-deep: end managed include")

(defconst dape-deep--ssh-managed-header
  "# Generated blocks managed by dape-deep.\n")

(defun dape-deep--ssh-connection-begin (host)
  "Return the begin marker for the managed connection stanza of HOST."
  (format "# dape-deep: begin connection %s" host))

(defun dape-deep--ssh-connection-end (host)
  "Return the end marker for the managed connection stanza of HOST."
  (format "# dape-deep: end connection %s" host))

(defun dape-deep--valid-ssh-alias-p (host)
  "Return non-nil when HOST is an exact, safe SSH alias."
  (and (stringp host)
       (string-match-p "\\`[[:alnum:]_.-]+\\'" host)))

(defun dape-deep--ssh-connection-field (connection key)
  "Return the trimmed non-empty string value of KEY in CONNECTION, or nil."
  (let ((value (plist-get connection key)))
    (when value
      (let ((text (string-trim (format "%s" value))))
        (unless (string-empty-p text) text)))))

(defun dape-deep--ssh-connection-normalize (host connection)
  "Return CONNECTION normalized with HOST and empty fields dropped.
Signal `user-error' when a value cannot be written safely into an OpenSSH
configuration file."
  (unless (dape-deep--valid-ssh-alias-p host)
    (user-error "HOST must be an exact SSH alias: %S" host))
  (when (and connection (not (listp connection)))
    (user-error "CONNECTION must be a plist: %S" connection))
  (let ((hostname (dape-deep--ssh-connection-field connection :hostname))
        (user (dape-deep--ssh-connection-field connection :user))
        (port (dape-deep--ssh-connection-field connection :port)))
    (when (and hostname
               (not (string-match-p "\\`[[:alnum:]_.:-]+\\'" hostname)))
      (user-error "Unsafe HostName value: %S" hostname))
    (when (and user (not (string-match-p "\\`[[:alnum:]_.-]+\\'" user)))
      (user-error "Unsafe User value: %S" user))
    (when port
      (unless (string-match-p "\\`[0-9]+\\'" port)
        (user-error "Unsafe Port value: %S" port))
      (unless (<= 1 (string-to-number port) 65535)
        (user-error "Port must be between 1 and 65535: %S" port)))
    (unless (or hostname user port)
      (user-error "Provide at least one of HostName, User, or Port"))
    (list :host host :hostname hostname :user user :port port)))

(defun dape-deep--ssh-config-path (path)
  "Return an expanded OpenSSH configuration PATH."
  (expand-file-name path))

(defun dape-deep--ssh-complete-markers-p (content begin end)
  "Return non-nil when CONTENT contains both BEGIN and END or neither.
Signal `user-error' when exactly one ownership marker is present."
  (let ((begin-count (if content
                         (dape-deep--string-count begin content)
                       0))
        (end-count (if content
                       (dape-deep--string-count end content)
                     0)))
    (unless (or (and (= begin-count 0) (= end-count 0))
                (and (= begin-count 1) (= end-count 1)))
      (user-error "Ambiguous managed SSH markers: %s / %s" begin end))
    (= begin-count 1)))

(defun dape-deep--ssh-include-block ()
  "Return the package-managed Include block."
  (format "%s\nInclude \"%s\"\n%s\n"
          dape-deep--ssh-include-begin
          (abbreviate-file-name
           (dape-deep--ssh-config-path
            dape-deep-ssh-managed-config-file))
          dape-deep--ssh-include-end))

(defun dape-deep--ssh-remove-managed-region (content begin end)
  "Remove the complete line-delimited BEGIN/END region from CONTENT."
  (let ((begin-pos (string-match
                    (concat "^" (regexp-quote begin) "$") content))
        (end-pos (string-match
                  (concat "^" (regexp-quote end) "$") content)))
    (if (and begin-pos end-pos (< begin-pos end-pos))
        (let ((after-end (or (string-match "\n" content end-pos)
                             (length content))))
          (concat (substring content 0 begin-pos)
                  (substring content
                             (if (< after-end (length content))
                                 (1+ after-end)
                               after-end))))
      content)))

(defun dape-deep--ssh-replace-managed-region
    (content begin end replacement)
  "Replace the complete BEGIN/END region in CONTENT with REPLACEMENT."
  (let ((begin-pos (string-match
                    (concat "^" (regexp-quote begin) "$") content))
        (end-pos (string-match
                  (concat "^" (regexp-quote end) "$") content)))
    (when (and begin-pos end-pos (< begin-pos end-pos))
      (let ((after-end (or (string-match "\n" content end-pos)
                           (length content))))
        (concat (substring content 0 begin-pos)
                replacement
                (substring content
                           (if (< after-end (length content))
                               (1+ after-end)
                             after-end)))))))

(defun dape-deep--ssh-install-include (content)
  "Return CONTENT with the package Include block installed first."
  (let ((without
         (dape-deep--ssh-remove-managed-region
          content dape-deep--ssh-include-begin
          dape-deep--ssh-include-end)))
    (concat (dape-deep--ssh-include-block)
            (unless (or (string-empty-p without)
                        (string-prefix-p "\n" without))
              "\n")
            without)))

(defun dape-deep--ssh-host-begin (host)
  "Return the begin marker for managed HOST."
  (format "# dape-deep: begin host %s" host))

(defun dape-deep--ssh-host-end (host)
  "Return the end marker for managed HOST."
  (format "# dape-deep: end host %s" host))

(defun dape-deep--ssh-host-block (host)
  "Return the managed OpenSSH block for HOST."
  (format (concat "%s\nHost %s\n"
                  "    ControlMaster auto\n"
                  "    ControlPersist %s\n"
                  "    ControlPath ~/.ssh/dape-rd-%%C\n"
                  "    ServerAliveInterval %d\n"
                  "    ServerAliveCountMax %d\n%s\n")
          (dape-deep--ssh-host-begin host)
          host
          dape-deep-ssh-control-persist
          dape-deep-ssh-server-alive-interval
          dape-deep-ssh-server-alive-count-max
          (dape-deep--ssh-host-end host)))

(defun dape-deep--ssh-install-host (content host)
  "Return managed configuration CONTENT with HOST installed or updated."
  (let* ((begin (dape-deep--ssh-host-begin host))
         (end (dape-deep--ssh-host-end host))
         (block (dape-deep--ssh-host-block host))
         (replaced (dape-deep--ssh-replace-managed-region
                    content begin end block)))
    (or replaced
        (concat content
                (unless (or (string-empty-p content)
                            (string-suffix-p "\n" content))
                  "\n")
                (unless (string-empty-p content) "\n")
                block))))

(defun dape-deep--ssh-connection-block (connection)
  "Return the managed OpenSSH connection block for CONNECTION.
CONNECTION is a normalized plist as returned by
`dape-deep--ssh-connection-normalize'.  Fields without a value produce no
line."
  (let* ((host (plist-get connection :host))
         (hostname (plist-get connection :hostname))
         (user (plist-get connection :user))
         (port (plist-get connection :port))
         (lines (delq nil
                      (list (when hostname
                              (format "    HostName %s" hostname))
                            (when user (format "    User %s" user))
                            (when port (format "    Port %s" port))))))
    (concat (dape-deep--ssh-connection-begin host) "\n"
            (format "Host %s\n" host)
            (when lines (concat (mapconcat #'identity lines "\n") "\n"))
            (dape-deep--ssh-connection-end host) "\n")))

(defun dape-deep--ssh-install-connection (content connection)
  "Return main configuration CONTENT with the CONNECTION stanza installed.
The stanza is appended at the end of CONTENT so user-owned Host declarations
keep OpenSSH first-value precedence.  Return CONTENT unchanged when CONNECTION
is nil."
  (if (null connection)
      content
    (let* ((host (plist-get connection :host))
           (begin (dape-deep--ssh-connection-begin host))
           (end (dape-deep--ssh-connection-end host))
           (block (dape-deep--ssh-connection-block connection))
           (replaced (dape-deep--ssh-replace-managed-region
                      content begin end block)))
      (or replaced
          (concat content
                  (unless (or (string-empty-p content)
                              (string-suffix-p "\n" content))
                    "\n")
                  (unless (string-empty-p content) "\n")
                  block)))))

(defun dape-deep--parse-ssh-g (text)
  "Parse ssh -G output TEXT into an alist of lowercase keys."
  (let (result)
    (dolist (line (split-string text "\n" t))
      (when (string-match "\\`\\([^[:space:]]+\\)[[:space:]]+\\(.+\\)\\'" line)
        (let ((key (downcase (match-string 1 line)))
              (value (match-string 2 line)))
          ;; ssh -G can emit repeated keys.  The first value is effective for
          ;; scalar options and is the useful one for our checks.
          (unless (assoc key result)
            (push (cons key value) result)))))
    (nreverse result)))

(defun dape-deep-ssh-effective-config (host)
  "Return the effective OpenSSH configuration alist for HOST using ssh -G."
  (unless (dape-deep--valid-ssh-alias-p host)
    (user-error "HOST must be an exact SSH alias: %S" host))
  (let ((buffer (generate-new-buffer " *dape-deep ssh-g*")))
    (unwind-protect
        (let ((status
               (apply #'process-file dape-deep-ssh-program nil buffer nil
                      (list "-F"
                            (dape-deep--ssh-config-path
                             dape-deep-ssh-config-file)
                            "-G" "--" host))))
          (unless (zerop status)
            (user-error "SSH -G failed for %s: %s"
                        host
                        (string-trim (with-current-buffer buffer
                                       (buffer-string)))))
          (dape-deep--parse-ssh-g
           (with-current-buffer buffer (buffer-string))))
      (kill-buffer buffer))))

(defun dape-deep--positive-ssh-value-p (value)
  "Return non-nil when OpenSSH VALUE represents a positive duration/count."
  (and value
       (or (string-equal value "yes")
           (and (string-match-p "\\`[0-9]+\\'" value)
                (> (string-to-number value) 0)))))

(defun dape-deep--ssh-checks (host)
  "Return diagnostic check plists for effective SSH configuration of HOST."
  (condition-case err
      (let* ((config (dape-deep-ssh-effective-config host))
             (master (cdr (assoc "controlmaster" config)))
             (persist (cdr (assoc "controlpersist" config)))
             (path (cdr (assoc "controlpath" config)))
             (interval (cdr (assoc "serveraliveinterval" config)))
             (count (cdr (assoc "serveralivecountmax" config))))
        (list
         (list :name "ssh -G" :ok t :detail host)
         (list :name "ControlMaster" :ok
               (member master '("auto" "autoask" "yes"))
               :detail (or master "unset"))
         (list :name "ControlPersist" :ok
               (dape-deep--positive-ssh-value-p persist)
               :detail (or persist "unset"))
         (list :name "ControlPath" :ok
               (and path (not (string-equal path "none")) (< (length path) 100))
               :detail (or path "unset"))
         (list :name "ServerAliveInterval" :ok
               (dape-deep--positive-ssh-value-p interval)
               :detail (or interval "unset"))
         (list :name "ServerAliveCountMax" :ok
               (dape-deep--positive-ssh-value-p count)
               :detail (or count "unset"))))
    (error
     (list (list :name "ssh -G" :ok nil
                 :detail (error-message-string err))))))

(defconst dape-deep--ssh-connection-fields
  '(("hostname" "HostName" :hostname)
    ("user" "User" :user)
    ("port" "Port" :port))
  "Mapping of ssh -G key, OpenSSH keyword, and CONNECTION plist key.")

(defun dape-deep--ssh-connection-checks (connection)
  "Return diagnostic check plists for the written CONNECTION stanza.
Only supplied fields are compared because ssh -G reports defaults for the
rest.  HostName is compared case-insensitively and Port numerically because
ssh -G lowercases host names and normalizes ports."
  (condition-case err
      (let* ((host (plist-get connection :host))
             (config (dape-deep-ssh-effective-config host)))
        (cons
         (list :name "ssh -G" :ok t :detail host)
         (mapcar
          (lambda (field)
            (let* ((key (nth 0 field))
                   (label (nth 1 field))
                   (want (plist-get connection (nth 2 field)))
                   (got (cdr (assoc key config)))
                   (ok (and got
                            (cond
                             ((string-equal key "port")
                              (and (string-match-p "\\`[0-9]+\\'" got)
                                   (= (string-to-number want)
                                      (string-to-number got))))
                             ((string-equal key "hostname")
                              (string-equal (downcase want) (downcase got)))
                             (t (string-equal want got))))))
              (list :name label :ok ok
                    :detail (if ok
                                (or got want)
                              (format
                               "wanted %s, effective %s (an earlier Host stanza wins)"
                               want (or got "unset"))))))
          (seq-filter (lambda (field)
                        (plist-get connection (nth 2 field)))
                      dape-deep--ssh-connection-fields))))
    (error
     (list (list :name "ssh -G" :ok nil
                 :detail (error-message-string err))))))

(defun dape-deep--display-checks (title checks)
  "Display diagnostic CHECKS under TITLE and return their buffer."
  (let ((buffer (get-buffer-create "*dape-deep doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert title "\n\n")
        (dolist (check checks)
          (insert (format "[%s] %-28s %s\n"
                          (if (plist-get check :ok) "OK" "FAIL")
                          (plist-get check :name)
                          (plist-get check :detail))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun dape-deep-ssh-doctor (&optional host)
  "Inspect the effective SSH configuration for HOST without connecting."
  (interactive)
  (let* ((host (or host dape-deep-host
                   (read-string "SSH Host alias: ")))
         (checks (dape-deep--ssh-checks host)))
    (dape-deep--display-checks
     (format "SSH checks for %s" host) checks)
    checks))

(defun dape-deep--ssh-backup (path)
  "Create a timestamped backup of PATH and return it, or nil if absent."
  (when (file-exists-p path)
    (let ((backup (format "%s.dape-deep.%s.bak"
                          path (format-time-string "%Y%m%d%H%M%S%N"))))
      (copy-file path backup nil t t)
      (set-file-modes backup #o600)
      backup)))

(defun dape-deep--restore-file (path content &optional mode)
  "Restore PATH to CONTENT, removing a newly created file when CONTENT is nil.
MODE is the mode PATH had before the change.  Rolling back has to leave the
file as the user had it, not as this package would have written it."
  (if content
      (dape-deep--atomic-write path content (or mode #o600))
    (when (file-exists-p path)
      (delete-file path))))

(defun dape-deep--ssh-login-probe-command (host)
  "Return the SSH argv used to probe login for HOST.
The managed configuration file is passed explicitly so the probe exercises the
same file that ssh -G validation inspects.  Pseudo-terminal allocation and
interactive authentication are disabled, so the probe can never prompt."
  (let ((timeout (max 1 dape-deep-ssh-probe-timeout))
        (ssh-arguments (seq-remove (lambda (argument)
                                     (member argument '("-t" "-tt")))
                                   dape-deep-ssh-arguments)))
    (append (list dape-deep-ssh-program
                  "-T"
                  "-o" "BatchMode=yes"
                  "-o" "StrictHostKeyChecking=accept-new"
                  "-o" (format "ConnectTimeout=%d" timeout)
                  "-F" (dape-deep--ssh-config-path
                        dape-deep-ssh-config-file))
            ssh-arguments
            (list "--" host "true"))))

(defun dape-deep--probe-ssh-login (host)
  "Best-effort login probe for HOST.
Return the process exit status, or the symbol `timeout'.  Never signal."
  (when (and (stringp host)
             (not (string-empty-p host))
             (not (string-prefix-p "-" host))
             (not (string-match-p "[[:space:]]" host)))
    (let ((buffer (generate-new-buffer " *dape-deep ssh probe*"))
          (deadline (+ (float-time) (max 1 dape-deep-ssh-probe-timeout)))
          process status)
      (unwind-protect
          (condition-case nil
              (progn
                (setq process
                      (make-process
                       :name "dape-deep ssh probe"
                       :buffer buffer
                       :stderr buffer
                       :command (dape-deep--ssh-login-probe-command host)
                       :connection-type 'pipe
                       :coding 'utf-8-unix
                       :noquery t))
                (while (and (process-live-p process)
                            (< (float-time) deadline))
                  (accept-process-output process 0.05))
                (setq status (if (process-live-p process)
                                 'timeout
                               (process-exit-status process))))
            (error nil))
        (when (process-live-p process)
          (delete-process process))
        (kill-buffer buffer))
      status)))

(defun dape-deep--report-ssh-login-probe (host)
  "Probe login for HOST and report the outcome as a message."
  (let ((status (dape-deep--probe-ssh-login host)))
    (when status
      (cond
       ((eq status 'timeout)
        (message "dape-deep: configured %s; login probe timed out after %d seconds"
                 host (max 1 dape-deep-ssh-probe-timeout)))
       ((zerop status)
        (message "dape-deep: configured %s; login probe succeeded" host))
       (t
        (message (concat "dape-deep: configured %s; login probe failed "
                         "(check the address, the port, and ~/.ssh keys)")
                 host))))))

;;;###autoload
(defun dape-deep-ssh-configure (host &optional no-confirm)
  "Install and validate package-managed multiplexing settings for HOST.
The main config receives only an Include block; per-host settings live in
`dape-deep-ssh-managed-config-file'.  Existing files are backed up.
On validation failure both files are rolled back.  Programmatic NO-CONFIRM
skips the confirmation prompt."
  (interactive (list (read-string "Exact SSH Host alias: "
                                  dape-deep-host)))
  (dape-deep--ssh-configure-host host nil no-confirm))

(defun dape-deep--ssh-configure-host (host connection &optional no-confirm)
  "Install and validate package-managed SSH settings for HOST.
CONNECTION is a normalized connection plist as returned by
`dape-deep--ssh-connection-normalize', or nil to install only the
multiplexing settings.  The main config receives the managed Include block
and, when CONNECTION is non-nil, a package-owned connection stanza.  Per-host
settings live in `dape-deep-ssh-managed-config-file'.  Existing files are
backed up.  On validation failure both files are rolled back.  When CONNECTION
is non-nil, a best-effort login probe reports its outcome after a successful
write.  Programmatic NO-CONFIRM skips the confirmation prompt."
  (unless (dape-deep--valid-ssh-alias-p host)
    (user-error "HOST must be an exact SSH alias: %S" host))
  (let* ((main (dape-deep--ssh-config-path
                dape-deep-ssh-config-file))
         (managed (dape-deep--ssh-config-path
                   dape-deep-ssh-managed-config-file))
         (main-before (dape-deep--read-file main))
         (main-modes (and main-before (file-modes main)))
         (managed-before (dape-deep--read-file managed))
         (managed-modes (and managed-before (file-modes managed)))
         (_include-markers
          (dape-deep--ssh-complete-markers-p
           main-before dape-deep--ssh-include-begin
           dape-deep--ssh-include-end))
         (_connection-markers
          (when connection
            (dape-deep--ssh-complete-markers-p
             main-before (dape-deep--ssh-connection-begin host)
             (dape-deep--ssh-connection-end host))))
         (_host-markers
          (dape-deep--ssh-complete-markers-p
           managed-before (dape-deep--ssh-host-begin host)
           (dape-deep--ssh-host-end host)))
         (main-after
          (dape-deep--ssh-install-connection
           (dape-deep--ssh-install-include (or main-before ""))
           connection))
         (managed-after (dape-deep--ssh-install-host
                         (or managed-before
                             dape-deep--ssh-managed-header)
                         host)))
    (when (and managed-before
               (not (string-prefix-p dape-deep--ssh-managed-header
                                     managed-before)))
      (user-error "Refusing to overwrite user-owned SSH file: %s" managed))
    (let* ((preview
            (unless no-confirm
              (dape-deep--preview-change
               (concat "### " main "\n" (or main-before "")
                       "\n### " managed "\n" (or managed-before ""))
               (concat "### " main "\n" main-after
                       "\n### " managed "\n" managed-after))))
           (confirmed
            (unwind-protect
                (or no-confirm
                    (y-or-n-p "Write and validate managed SSH settings? "))
              ;; The preview describes a change that is pending only until this
              ;; question is answered, so it must not outlive the answer: a
              ;; declined prompt leaves nothing to write, and a confirmed one
              ;; makes the displayed diff stale.
              (dape-deep--close-preview preview))))
      (when confirmed
        (dape-deep--ssh-backup main)
        (dape-deep--ssh-backup managed)
        (let ((result
               (condition-case err
                   (progn
                     (dape-deep--atomic-write managed managed-after #o600)
                     (dape-deep--atomic-write main main-after #o600)
                     (let ((failed
                            (seq-find
                             (lambda (check) (not (plist-get check :ok)))
                             (append
                              (dape-deep--ssh-checks host)
                              (when connection
                                (dape-deep--ssh-connection-checks
                                 connection))))))
                       (when failed
                         (error "SSH validation failed: %s (%s)"
                                (plist-get failed :name)
                                (plist-get failed :detail))))
                     (message "Configured and validated SSH alias %s" host)
                     (list main managed))
                 (error
                  (dape-deep--restore-file main main-before main-modes)
                  (dape-deep--restore-file managed managed-before managed-modes)
                  (signal (car err)
                          (append (cdr err)
                                  '("Changes were rolled back")))))))
          (when connection
            (dape-deep--report-ssh-login-probe host))
          result)))))

;;;###autoload
(defun dape-deep-ssh-add (host connection &optional no-confirm)
  "Add or update a complete SSH alias and install package-managed settings.
HOST is the exact SSH alias.  CONNECTION is a plist with any of `:hostname',
`:user', and `:port'; absent or empty values are omitted from the stanza.  The
connection stanza is written into a package-owned region at the end of
`dape-deep-ssh-config-file', so an earlier user-owned Host stanza keeps
OpenSSH precedence and causes validation to fail rather than be overridden.
Multiplexing and keepalive options go into
`dape-deep-ssh-managed-config-file'.  Existing files are backed up, the
result is validated with ssh -G, and both files are rolled back on failure.
NO-CONFIRM skips the confirmation prompt.  After a successful write a
best-effort login probe runs and only reports a message."
  (interactive
   (let* ((host (string-trim (read-string "SSH Host alias: "
                                          dape-deep-host)))
          (known (and (dape-deep--valid-ssh-alias-p host)
                      (condition-case nil
                          (dape-deep-ssh-effective-config host)
                        (error nil))))
          (effective-hostname (cdr (assoc "hostname" known)))
          (defined (and effective-hostname
                        (not (string-equal (downcase effective-hostname)
                                           (downcase host)))))
          (hostname (read-string "HostName (empty: connect to alias): "
                                 (and defined effective-hostname)))
          (user (read-string "User (empty: omit): "
                             (and defined (cdr (assoc "user" known)))))
          (port (read-string "Port (empty: omit): "
                             (and defined (cdr (assoc "port" known))))))
     (list host
           (list :hostname (string-trim hostname)
                 :user (string-trim user)
                 :port (string-trim port)))))
  (let ((connection (dape-deep--ssh-connection-normalize host connection)))
    (dape-deep--ssh-configure-host
     (plist-get connection :host) connection no-confirm)))

(provide 'dape-deep-ssh)

;;; dape-deep-ssh.el ends here
