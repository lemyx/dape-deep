;;; dape-deep-fakes.el --- Test fakes for dape-deep -*- lexical-binding: t; -*-

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
;; Deterministic process fakes shared by the ERT suite.

;;; Code:

(defun dape-deep-tests--write-fake-ssh (directory)
  "Write a fake ssh executable under DIRECTORY and return its path.
The executable consumes the options emitted by `dape-deep' and executes the
remote command locally."
  (let ((file (expand-file-name "fake-ssh" directory)))
    (with-temp-file file
      (insert "#!/bin/sh\n"
              "while [ \"$#\" -gt 0 ]; do\n"
              "  case \"$1\" in\n"
              "    --) shift; break ;;\n"
              "    -L|-o) shift 2 ;;\n"
              "    *) shift ;;\n"
              "  esac\n"
              "done\n"
              "shift\n"
              "exec /bin/sh -c \"$1\"\n"))
    (set-file-modes file #o700)
    file))

(provide 'dape-deep-fakes)

;; The suite beside this file is not compiled; see dape-deep-tests.el.  Skip
;; this file too, so a checkout never carries a stale test .elc.
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dape-deep-fakes.el ends here
