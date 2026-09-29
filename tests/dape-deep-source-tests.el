;;; dape-deep-source-tests.el --- Tests for dape-deep-source -*- lexical-binding: t; -*-

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
;; Unit tests for the on-demand sources of `dape-deep-source'.  A debug
;; connection needs a process and an adapter, so these tests stand a session key
;; in for the connection and drive the advice with the arguments Dape passes.

;;; Code:

(require 'cl-lib)
(require 'dape-deep)
(require 'ert)

(defmacro dape-deep-source-tests--with-session (session &rest body)
  "Run BODY with SESSION standing in for the connection it is given."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'dape-deep-source--session)
              (lambda (_conn) ,session)))
     ,@body))

(defun dape-deep-source-tests--fetch (session reference path)
  "Record PATH as the file behind REFERENCE in SESSION.
Dape asks for a source with the frame's own source object, which names both."
  (dape-deep-source-tests--with-session session
    (dape-deep-source--request-advice
     (lambda (&rest _) nil)
     'conn :source
     (list :source (list :path path :sourceReference reference)
           :sourceReference reference)
     #'ignore)))

(ert-deftest dape-deep-source-reference-defaults-to-nil-test ()
  (should-not (default-value 'dape-deep-source-reference)))

(ert-deftest dape-deep-source-advises-dape-test ()
  (should (advice-member-p #'dape-deep-source--request-advice 'dape-request))
  (should (advice-member-p #'dape-deep-source--make-buffer-advice
                           'dape--source-make-buffer)))

(ert-deftest dape-deep-source-adds-identity-path-mappings-test ()
  "Both roots are the remote root: Dape already translated the paths."
  (let* ((dape-deep-source-reference t)
         (settings (list :backend 'ssh :remote-root "/workspace/project/"))
         (config (dape-deep-source--configure (list :request "attach")
                                              settings)))
    (should (equal (plist-get config :pathMappings)
                   [(:localRoot "/workspace/project/"
                     :remoteRoot "/workspace/project/")]))
    (should (equal (plist-get config :request) "attach"))))

(ert-deftest dape-deep-source-keeps-configured-path-mappings-test ()
  (let* ((dape-deep-source-reference t)
         (settings (list :backend 'ssh :remote-root "/workspace/project/"))
         (configured [(:localRoot "/local" :remoteRoot "/remote")])
         (config (list :pathMappings configured)))
    (should (eq (plist-get (dape-deep-source--configure config settings)
                           :pathMappings)
                configured))))

(ert-deftest dape-deep-source-adds-no-mapping-while-disabled-test ()
  (let* ((dape-deep-source-reference nil)
         (settings (list :backend 'ssh :remote-root "/workspace/project/"))
         (config (dape-deep-source--configure (list :request "attach")
                                              settings)))
    (should-not (plist-member config :pathMappings))))

(ert-deftest dape-deep-source-remembers-the-path-of-a-fetched-source-test ()
  (let ((dape-deep-source-reference t)
        (session (list :session)))
    (dape-deep-source-tests--fetch session 12
                                   "/remote/site-packages/torch/module.py")
    (should (equal (dape-deep-source-tests--with-session session
                     (dape-deep-source--path 'conn 12))
                   "/remote/site-packages/torch/module.py"))))

(ert-deftest dape-deep-source-ignores-a-source-without-a-path-test ()
  "A source object that names no file, such as a module event, records nothing."
  (let ((dape-deep-source-reference t)
        (session (list :session)))
    (dape-deep-source-tests--with-session session
      (dape-deep-source--request-advice
       (lambda (&rest _) nil)
       'conn :source
       (list :source (list :name "torch") :sourceReference 12)
       #'ignore)
      (should-not (dape-deep-source--path 'conn 12)))))

(ert-deftest dape-deep-source-keeps-sessions-apart-test ()
  "Two debug sessions must not see each other's references."
  (let ((dape-deep-source-reference t)
        (first (list :first))
        (second (list :second)))
    (dape-deep-source-tests--fetch first 12 "/remote/site-packages/torch/a.py")
    (should (dape-deep-source-tests--with-session first
              (dape-deep-source--path 'conn 12)))
    (should-not (dape-deep-source-tests--with-session second
                  (dape-deep-source--path 'conn 12)))))

(ert-deftest dape-deep-source-restores-the-path-when-setting-breakpoints-test ()
  (let ((dape-deep-source-reference t)
        (session (list :session))
        (arguments (list :breakpoints [(:line 3)]
                         :source (list :sourceReference 12))))
    (dape-deep-source-tests--with-session session
      (dape-deep-source--remember 'conn 12 "/remote/site-packages/torch/a.py")
      (let* ((completed (dape-deep-source--complete 'conn arguments))
             (source (plist-get completed :source)))
        (should (equal (plist-get source :path) "/remote/site-packages/torch/a.py"))
        (should (equal (plist-get source :sourceReference) 12))
        (should (equal (plist-get completed :breakpoints) [(:line 3)]))))))

(ert-deftest dape-deep-source-copies-the-arguments-it-completes-test ()
  "The plists Dape built are read, never modified."
  (let ((dape-deep-source-reference t)
        (session (list :session))
        (arguments (list :source (list :sourceReference 12))))
    (dape-deep-source-tests--with-session session
      (dape-deep-source--remember 'conn 12 "/remote/site-packages/torch/a.py")
      (dape-deep-source--complete 'conn arguments)
      (should (equal arguments (list :source (list :sourceReference 12)))))))

(ert-deftest dape-deep-source-leaves-a-complete-source-alone-test ()
  (let ((dape-deep-source-reference t)
        (session (list :session))
        (arguments (list :source (list :path "/remote/project/train.py"
                                       :sourceReference 12))))
    (dape-deep-source-tests--with-session session
      (dape-deep-source--remember 'conn 12 "/remote/site-packages/torch/a.py")
      (should (eq (dape-deep-source--complete 'conn arguments) arguments)))))

(ert-deftest dape-deep-source-leaves-unknown-references-alone-test ()
  (let ((dape-deep-source-reference t)
        (session (list :session))
        (arguments (list :source (list :sourceReference 99))))
    (dape-deep-source-tests--with-session session
      (should (eq (dape-deep-source--complete 'conn arguments) arguments)))))

(ert-deftest dape-deep-source-names-an-unnamed-source-test ()
  "debugpy can send a source without a name; its file name is a better one."
  (let ((dape-deep-source-reference t)
        (session (list :session))
        asked)
    (dape-deep-source-tests--with-session session
      (cl-letf (((symbol-function 'dape-deep-source--reference-buffer)
                 (lambda (&rest _) nil)))
        (dape-deep-source--remember 'conn 12 "/remote/site-packages/torch/a.py")
        (dape-deep-source--make-buffer-advice
         (lambda (_conn name _reference _content _mime) (setq asked name))
         'conn nil 12 "content" "text/x-python")
        (should (equal asked "a.py"))))))

(ert-deftest dape-deep-source-keeps-a-name-the-adapter-sent-test ()
  (let ((dape-deep-source-reference t)
        (session (list :session))
        asked)
    (dape-deep-source-tests--with-session session
      (cl-letf (((symbol-function 'dape-deep-source--reference-buffer)
                 (lambda (&rest _) nil)))
        (dape-deep-source--remember 'conn 12 "/remote/site-packages/torch/a.py")
        (dape-deep-source--make-buffer-advice
         (lambda (_conn name _reference _content _mime) (setq asked name))
         'conn "module.py" 12 "content" "text/x-python")
        (should (equal asked "module.py"))))))

(defmacro dape-deep-source-tests--with-buffer (content &rest body)
  "Run BODY in a read-only buffer holding CONTENT, and kill it after.
BODY runs inside a `let' that binds `buffer' to that buffer."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer "*dape-source test*")))
     (unwind-protect
         (progn
           (with-current-buffer buffer
             (insert ,content)
             (setq buffer-read-only t))
           ,@body)
       (kill-buffer buffer))))

(ert-deftest dape-deep-source-fontifies-a-python-source-test ()
  ;; font-lock-mode cannot be observed from batch, where it stays off by
  ;; design; the mode that carries font-lock-defaults is what a test can see.
  (dape-deep-source-tests--with-buffer "import torch\n"
    (dape-deep-source--highlight buffer "/remote/site-packages/torch/a.py")
    (with-current-buffer buffer
      (should (eq major-mode 'python-mode))
      (should buffer-read-only)
      (should-not buffer-file-name))))

(ert-deftest dape-deep-source-fontifies-a-stub-source-test ()
  (dape-deep-source-tests--with-buffer "def forward(x: int) -> int: ...\n"
    (dape-deep-source--highlight buffer "/remote/site-packages/torch/a.pyi")
    (with-current-buffer buffer
      (should (eq major-mode 'python-mode)))))

(ert-deftest dape-deep-source-keeps-the-mode-the-adapter-chose-test ()
  "A MIME type Dape maps keeps its mode, and its read-only flag."
  (dape-deep-source-tests--with-buffer "import torch\n"
    (with-current-buffer buffer (text-mode) (setq buffer-read-only t))
    (dape-deep-source--highlight buffer "/remote/site-packages/torch/a.py")
    (with-current-buffer buffer
      (should (eq major-mode 'text-mode))
      (should buffer-read-only))))

(ert-deftest dape-deep-source-leaves-other-sources-alone-test ()
  (dape-deep-source-tests--with-buffer "weights\n"
    (dape-deep-source--highlight buffer "/remote/data/names.txt")
    (with-current-buffer buffer
      (should (eq major-mode 'fundamental-mode))
      (should buffer-read-only))))

(ert-deftest dape-deep-source-highlights-what-it-fetched-test ()
  "The advice gives the buffer Dape made the mode of its file."
  (let ((dape-deep-source-reference t)
        (session (list :session))
        (buffer (generate-new-buffer "*dape-source fetched*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "import torch\n")
            (setq buffer-read-only t))
          (dape-deep-source-tests--with-session session
            (cl-letf (((symbol-function 'dape-deep-source--reference-buffer)
                       (lambda (&rest _) buffer)))
              (dape-deep-source--remember 'conn 12
                                          "/remote/site-packages/torch/a.py")
              (dape-deep-source--make-buffer-advice
               (lambda (&rest _) nil) 'conn "a.py" 12 "import torch\n"
               "text/x-python")))
          (with-current-buffer buffer
            (should (eq major-mode 'python-mode))
            (should buffer-read-only)))
      (kill-buffer buffer))))

(ert-deftest dape-deep-source-requests-without-the-feature-test ()
  "While disabled, requests pass through untouched and nothing is recorded."
  (let ((dape-deep-source-reference nil)
        (session (list :session))
        (cb #'ignore)
        seen)
    (dape-deep-source-tests--with-session session
      (dape-deep-source--request-advice
       (lambda (_conn command arguments callback)
         (setq seen (list command arguments callback)))
       'conn :source (list :sourceReference 12) cb)
      (should (equal (car seen) :source))
      (should (eq (caddr seen) cb))
      (should-not (dape-deep-source--path 'conn 12)))))

(provide 'dape-deep-source-tests)

;; A checkout installed as a package is compiled file by file, and the fakes
;; this suite requires sit outside `load-path' for that compiler.  Nothing needs
;; a compiled suite, and `no-byte-compile' also keeps native compilation away.
;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dape-deep-source-tests.el ends here
