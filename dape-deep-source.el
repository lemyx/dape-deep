;;; dape-deep-source.el --- On-demand remote sources for Dape -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Di Xiu

;; Author: Di Xiu <dyi.shiou@gmail.com>
;; Assisted-by: Claude Code:deepseek-flash
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
;; Read library sources from an SSH target while debugging, without
;; synchronizing the dependencies they belong to.  A library that exists only on
;; the target has no path this machine can open, so the adapter answers with a
;; source reference and Dape fetches the file on demand into a read-only buffer.
;;
;; Three things are missing between that mechanism and a usable buffer: debugpy
;; wants the original path whenever breakpoints are set in one, it does not
;; always name the source it sends, and it leaves the buffer in
;; `fundamental-mode'.  This module supplies the three, and none of it is active
;; unless `dape-deep-source-reference' is enabled.

;;; Code:

(require 'dape)
(require 'dape-deep-config)
(require 'subr-x)

(declare-function python-mode "python")

(defcustom dape-deep-source-reference nil
  "Whether Dape reads library sources from the target while debugging.
When enabled, a new SSH attach session asks the adapter for a source reference
for every library outside the project, and the buffers Dape fetches for them
can be stepped into and broken in.  The buffers are read-only and belong to the
session; they do not visit a file on disk.  This is runtime debugging, not a
language server: see the README for what it does and does not reach."
  :type 'boolean
  :group 'dape-deep)

(defvar dape-deep-source--paths (make-hash-table :test #'eq :weakness 'key)
  "Remote path of a source reference by session.
Keys are sessions, as `dape-deep-source--session' returns them, and values are
hash tables mapping a source reference to the path the adapter named it by.  A
session's entry goes away with its connection rather than outliving it.")

(defun dape-deep-source--session (conn)
  "Return the session that CONN belongs to.
Dape keys its own source buffers on the root connection, so advice about those
buffers keys its state the same way."
  (dape--root-of conn))

(defun dape-deep-source--remember (conn reference path)
  "Remember that REFERENCE names PATH in CONN's session."
  (let* ((session (dape-deep-source--session conn))
         (paths (or (gethash session dape-deep-source--paths)
                    (puthash session (make-hash-table :test #'eql)
                             dape-deep-source--paths))))
    (puthash reference path paths)))

(defun dape-deep-source--path (conn reference)
  "Return the remote path of REFERENCE in CONN's session, if it is known."
  (when (numberp reference)
    (when-let* ((paths (gethash (dape-deep-source--session conn)
                                dape-deep-source--paths)))
      (gethash reference paths))))

(defun dape-deep-source--remember-source (conn source)
  "Remember the remote path that SOURCE names for CONN.

Dape asks for an on-demand source with the frame's own source object, which
debugpy fills with the file's server path beside its reference.  That request is
the only place the path appears: the response carries the content and nothing
else, because debugpy 1.8.21 answers with a `SourceResponseBody', whose fields
are `content' and `mimeType'."
  (let ((path (plist-get source :path))
        (reference (plist-get source :sourceReference)))
    (when (and (stringp path) (numberp reference))
      (dape-deep-source--remember conn reference path))))

(defun dape-deep-source--complete (conn arguments)
  "Return ARGUMENTS with the remote path of its source reference restored.

`dape--set-breakpoints-in-source' describes the file either by path or by
source reference, and a buffer Dape made from a reference has no path of its
own.  debugpy 1.8.21 wants the path of the file it breaks in, so the path
remembered from the `source' response goes back in beside the reference.

ARGUMENTS is returned unchanged when it already names a path or when the
reference was never fetched through this module.  The caller's plists are
copied rather than modified."
  (let* ((source (plist-get arguments :source))
         (path (plist-get source :path))
         (reference (plist-get source :sourceReference))
         (remote (and (not (and (stringp path) (not (string-empty-p path))))
                      (dape-deep-source--path conn reference))))
    (if remote
        (let ((completed (copy-sequence arguments)))
          (plist-put completed :source
                     (plist-put (copy-sequence source) :path remote)))
      arguments)))

(defun dape-deep-source--reference-buffer (conn reference)
  "Return the buffer Dape made for REFERENCE in CONN's session."
  (plist-get (dape--source-buffers (dape-deep-source--session conn))
             reference))

(defun dape-deep-source--highlight (buffer path)
  "Give the Python source in BUFFER a major mode when nothing else chose one.

An adapter whose MIME type is not in `dape-mime-mode-alist' leaves the buffer in
`fundamental-mode', without highlighting or code navigation.  Only that mode is
replaced, so a mode the adapter or the user did pick stands.

BUFFER is given no file name: it belongs to the debug session and does not visit
a file on disk, which is also what keeps a language server from attaching to it.
Python mode resets `buffer-read-only', so the flag is saved and put back."
  (when (and (buffer-live-p buffer)
             (stringp path)
             (string-match-p "\\.pyi?\\'" path))
    (with-current-buffer buffer
      (when (eq major-mode 'fundamental-mode)
        (let ((read-only buffer-read-only))
          (python-mode)
          (font-lock-mode 1)
          (setq buffer-read-only read-only))))))

(defun dape-deep-source--make-buffer-advice (make-buffer conn name reference
                                                         content mime-type)
  "Make and finish the on-demand source buffer for REFERENCE.

MAKE-BUFFER is `dape--source-make-buffer', and CONN, NAME, REFERENCE, CONTENT,
and MIME-TYPE are its arguments.  The adapter does not always name the source
it sends, and the file name of the remembered path is a better name than none."
  (let* ((path (dape-deep-source--path conn reference))
         (name (or (and (stringp name) (not (string-empty-p name)) name)
                   (and path (file-name-nondirectory path))
                   name)))
    (funcall make-buffer conn name reference content mime-type)
    (dape-deep-source--highlight
     (dape-deep-source--reference-buffer conn reference) path)))

(defun dape-deep-source--request-advice (request conn command arguments
                                                 &optional cb)
  "Advise `dape-request' REQUEST for CONN, COMMAND, ARGUMENTS, and CB.
The request for an on-demand source names the file behind its reference, so it
is recorded there.  A `setBreakpoints' request is where that path is needed
back."
  (cond
   ((not dape-deep-source-reference)
    (funcall request conn command arguments cb))
   ((eq command :source)
    (dape-deep-source--remember-source conn (plist-get arguments :source))
    (funcall request conn command arguments cb))
   ((eq command :setBreakpoints)
    (funcall request conn command
             (dape-deep-source--complete conn arguments) cb))
   (t
    (funcall request conn command arguments cb))))

(defun dape-deep-source--configure (config settings)
  "Add the identity `:pathMappings' entry to CONFIG for SETTINGS.

Dape already translates project paths with `prefix-local' and `prefix-remote',
so the adapter needs no translation of its own, and an entry whose two roots are
the remote root translates nothing.  What it buys is the boundary: the adapter
reads it as \"the client has this project and nothing else\", and answers with a
source reference for every library outside it, which is what makes stepping into
a dependency possible.

Nothing is added while `dape-deep-source-reference' is disabled, and a
`:pathMappings' the user configured is kept as it is.  The value is a vector of
keyword plists because `dape--launch-or-attach-arguments' reads a plain list as
one argument plist and drops what is not a keyword."
  (if (and dape-deep-source-reference
           (not (plist-member config :pathMappings)))
      (plist-put config :pathMappings
                 (vector (list :localRoot (plist-get settings :remote-root)
                               :remoteRoot (plist-get settings :remote-root))))
    config))

(advice-add 'dape-request :around #'dape-deep-source--request-advice)
(advice-add 'dape--source-make-buffer :around
            #'dape-deep-source--make-buffer-advice)

(provide 'dape-deep-source)

;;; dape-deep-source.el ends here
