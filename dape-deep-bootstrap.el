;;; dape-deep-bootstrap.el --- Reversible debugpy injection -*- lexical-binding: t; -*-

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
;; Indentation-aware insertion and removal of a guarded debugpy bootstrap.
;; Source changes happen directly in the local buffer so the synchronized
;; remote file retains identical line numbers.

;;; Code:

(require 'diff)
(require 'subr-x)

(defconst dape-deep--bootstrap-begin
  "# dape-deep: begin")

(defconst dape-deep--bootstrap-end
  "# dape-deep: end")

(defun dape-deep--bootstrap-lines ()
  "Return the canonical debugpy bootstrap as unindented lines.
The lines are kept inside 88 columns so a formatter such as black or ruff
format leaves the block alone."
  (list
   dape-deep--bootstrap-begin
   "# Managed by dape-deep; pauses the target until Emacs attaches."
   "import os"
   ""
   "_rank = os.getenv(\"DAPE_DEEP_RANK\", \"0\")"
   "if os.getenv(\"DAPE_DEEP\") == \"1\" and os.getenv(\"RANK\", \"0\") == _rank:"
   "    import debugpy  # noqa: T100"
   ""
   "    _port = int(os.getenv(\"DAPE_DEEP_PORT\", \"5678\"))"
   "    debugpy.listen((\"127.0.0.1\", _port))  # noqa: T100"
   "    print(f\"[dape-deep] waiting for Dape on 127.0.0.1:{_port}\", flush=True)"
   "    debugpy.wait_for_client()  # noqa: T100"
   dape-deep--bootstrap-end))

(defun dape-deep--indent-body (line step)
  "Return LINE with each leading four-space level replaced by STEP."
  (let ((level 0))
    (while (string-prefix-p "    " line)
      (setq level (1+ level))
      (setq line (substring line 4)))
    (concat (mapconcat #'identity (make-list level step) "") line)))

(defun dape-deep--bootstrap-block (indent)
  "Return a bootstrap block with every line prefixed by INDENT.
The canonical body indents each level with four spaces; an INDENT that holds a
tab switches the nested levels to tabs as well, so a file indented with tabs
keeps its own style instead of mixing spaces into it."
  (let ((step (if (string-match-p "\t" indent) "\t" "    ")))
    (concat
     (mapconcat (lambda (line)
                  (if (string-empty-p line)
                      ""
                    (concat indent (dape-deep--indent-body line step))))
                (dape-deep--bootstrap-lines)
                "\n")
     "\n\n")))

(defun dape-deep--python-buffer-p ()
  "Return non-nil when the current buffer contains Python source."
  (or (derived-mode-p 'python-base-mode 'python-mode)
      (and buffer-file-name
           (string-equal (file-name-extension buffer-file-name) "py"))))

(defun dape-deep--bootstrap-bounds ()
  "Return bounds of the managed bootstrap, or nil when it is absent.
Signal `user-error' when the markers are incomplete or out of order, or when
the block appears more than once, which a merge can leave behind."
  (let ((begins (how-many (regexp-quote dape-deep--bootstrap-begin)
                          (point-min) (point-max)))
        (ends (how-many (regexp-quote dape-deep--bootstrap-end)
                        (point-min) (point-max))))
    (when (and (or (> begins 0) (> ends 0))
               (not (and (= begins 1) (= ends 1))))
      (user-error "Expected one dape-deep bootstrap, found %d begin and %d end markers"
                  begins ends))
    (save-excursion
      (goto-char (point-min))
      (let ((begin (when (search-forward dape-deep--bootstrap-begin nil t)
                     (line-beginning-position)))
            end)
        (goto-char (point-min))
        (setq end (when (search-forward dape-deep--bootstrap-end nil t)
                    (min (point-max) (1+ (line-end-position)))))
        ;; The canonical block owns its trailing separator line, so removal
        ;; restores the source exactly as it was before insertion.
        (when (and end (< end (point-max)))
          (goto-char end)
          (when (looking-at "[[:blank:]]*\n")
            (setq end (match-end 0))))
        (cond
         ((and begin end (< begin end)) (cons begin end))
         ((or begin end)
          (user-error "Incomplete dape-deep bootstrap markers"))
         (t nil))))))

(defun dape-deep--preview-change (before after)
  "Display a unified diff from BEFORE to AFTER.
Return the preview buffer so callers can remove it once the change it
describes stops being pending."
  (let ((before-buffer (generate-new-buffer " *dape-deep-before*"))
        (after-buffer (generate-new-buffer " *dape-deep-after*"))
        (preview (get-buffer-create "*dape-deep diff*")))
    (unwind-protect
        (progn
          (with-current-buffer before-buffer (insert before))
          (with-current-buffer after-buffer (insert after))
          (diff-no-select before-buffer after-buffer "-u" t preview)
          (display-buffer preview))
      (kill-buffer before-buffer)
      (kill-buffer after-buffer))
    preview))

(defun dape-deep--close-preview (preview)
  "Remove PREVIEW and restore the window it was displayed in.
A preview describes a change that is still pending, so it must not outlive the
confirmation it belongs to."
  (when (buffer-live-p preview)
    (let ((window (get-buffer-window preview t)))
      (when (window-live-p window)
        (with-selected-window window
          (quit-window))))
    (when (buffer-live-p preview)
      (kill-buffer preview))))

(defun dape-deep--line-indent ()
  "Return the literal whitespace that indents the current line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[[:blank:]]*")
        (match-string-no-properties 0)
      "")))

(defun dape-deep--inject (position indent)
  "Insert the managed block at POSITION, prefixed by the INDENT string.
INDENT is copied from the surrounding source, so tabs stay tabs."
  (save-excursion
    (goto-char position)
    (when (re-search-forward
           "^from[[:space:]]+__future__[[:space:]]+import[[:space:]]" nil t)
      (user-error "Insert after all Python __future__ imports")))
  (let ((block (dape-deep--bootstrap-block indent)))
    (atomic-change-group
      (goto-char position)
      (insert block))
    (message "Inserted dape-deep bootstrap")
    t))

;;;###autoload
(defun dape-deep-inject-at-point (&optional _no-confirm)
  "Insert a guarded debugpy bootstrap at the current line.
The block follows the current indentation and is inserted immediately.
The optional argument is retained for compatibility and ignored."
  (interactive)
  (unless (dape-deep--python-buffer-p)
    (user-error "The current buffer is not Python source"))
  (when (dape-deep--bootstrap-bounds)
    (user-error "A dape-deep bootstrap already exists"))
  (dape-deep--inject
   (line-beginning-position) (dape-deep--line-indent)))

(defun dape-deep--main-body-position ()
  "Return the insertion position and indentation for a Python main guard.
The indentation is the literal whitespace of the guard's first body line, so a
file indented with tabs keeps its own style.  A guard that only appears inside
a string or comment, such as a usage example in a docstring, is skipped; the
block would otherwise be inserted into that literal and never run."
  (save-excursion
    (goto-char (point-min))
    (let (guard)
      (while (and (not guard)
                  (re-search-forward
                   "^if[[:space:]]+__name__[[:space:]]*==[[:space:]]*['\"]__main__['\"][[:space:]]*:"
                   nil t))
        ;; `syntax-ppss' moves point, so the match end is kept explicitly.
        (let ((end (point)))
          (unless (save-excursion
                    (nth 8 (syntax-ppss (match-beginning 0))))
            (setq guard end))))
      (unless guard
        (user-error "No top-level Python __main__ guard found")))
    (let* ((guard-column (current-indentation))
           (guard-indent (dape-deep--line-indent)))
      (forward-line 1)
      (while (and (not (eobp)) (looking-at-p "^[[:space:]]*$"))
        (forward-line 1))
      (let ((body-indent
             (if (and (not (eobp)) (> (current-indentation) guard-column))
                 (dape-deep--line-indent)
               (concat guard-indent
                       (make-string (if (boundp 'python-indent-offset)
                                        python-indent-offset
                                      4)
                                    ?\s)))))
        (cons (line-beginning-position) body-indent)))))

;;;###autoload
(defun dape-deep-inject-main (&optional _no-confirm)
  "Insert a guarded debugpy bootstrap at the start of a Python main guard.
The change is applied immediately.  The optional argument is retained for
compatibility and ignored."
  (interactive)
  (unless (dape-deep--python-buffer-p)
    (user-error "The current buffer is not Python source"))
  (when (dape-deep--bootstrap-bounds)
    (user-error "A dape-deep bootstrap already exists"))
  (pcase-let ((`(,position . ,indent)
               (dape-deep--main-body-position)))
    (save-excursion
      (dape-deep--inject position indent))))

;;;###autoload
(defun dape-deep-remove-injection (&optional force _no-confirm)
  "Remove the managed debugpy bootstrap from the current buffer.
Refuse to remove an edited block unless FORCE is non-nil, otherwise remove it
immediately.  Interactively, a prefix argument supplies FORCE.  The second
optional argument is retained for compatibility and ignored."
  (interactive "P")
  (let ((bounds (or (dape-deep--bootstrap-bounds)
                    (user-error "No dape-deep bootstrap found"))))
    (pcase-let* ((`(,begin . ,end) bounds)
                 (indent (save-excursion
                           (goto-char begin)
                           (dape-deep--line-indent)))
                 (actual (buffer-substring-no-properties begin end))
                 (expected (dape-deep--bootstrap-block indent)))
      (unless (or force (string-equal actual expected))
        (user-error "Bootstrap was edited; use a prefix argument to remove it"))
      (atomic-change-group
        (delete-region begin end))
      (message "Removed dape-deep bootstrap"))))

(provide 'dape-deep-bootstrap)

;;; dape-deep-bootstrap.el ends here
