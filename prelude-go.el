;;; prelude-go.el --- Emacs Prelude: A nice setup for Go devs.
;;
;; Author: Doug MacEachern
;; URL: https://github.com/dougm/go-prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Prelude configuration for Go

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'prelude-programming)

(defvar prelude-go-smartparens t
  "Enable smartparens enhancements.")

(prelude-require-packages '(go-mode
                            company-go
                            go-eldoc
                            go-errcheck
                            go-projectile
                            golint
                            gotest))

(require 'go-projectile)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

(defun prelude-go-open-pair (id action context)
  "Open a new pair with newline and indent.
ID is used to look-up the pair close.  ACTION and CONTEXT are ignored."
  (let ((c (string-to-char (plist-get (sp-get-pair id) :close))))
    (when (eq (following-char) c)
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))))

(eval-after-load 'go-mode
  '(progn
     (defun prelude-go-mode-defaults ()
       ;; Default key bindings for gotest.el
       (let ((map go-mode-map))
         (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
         (define-key map (kbd "C-c m") 'go-test-current-file)
         (define-key map (kbd "C-c .") 'go-test-current-test)
         (define-key map (kbd "C-c b") 'go-run))

       ;; Prefer goimports to gofmt if installed
       (let ((goimports (executable-find "goimports")))
         (when goimports
           (setq gofmt-command goimports)))

       ;; gofmt on save
       (add-hook 'before-save-hook 'gofmt-before-save nil t)

       ;; stop whitespace being highlighted
       (whitespace-toggle-options '(tabs))

       ;; Company mode settings
       (set (make-local-variable 'company-backends) '(company-go))

       ;; El-doc for Go
       (go-eldoc-setup)

       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq prelude-go-mode-hook 'prelude-go-mode-defaults)

     (add-hook 'go-mode-hook (lambda ()
                               (run-hooks 'prelude-go-mode-hook)))

     ;; smartparens enhancements
     (when prelude-go-smartparens
       (dolist (key '("{" "("))
        (sp-local-pair 'go-mode key nil :post-handlers
                       '((prelude-go-open-pair "RET")))))

     ;; Enable go-oracle-mode if available
     (let ((oracle (executable-find "oracle")))
       (when oracle
         (setq go-oracle-command oracle)
         (autoload 'go-oracle-mode "oracle")
         (add-hook 'go-mode-hook 'go-oracle-mode)))))

(provide 'prelude-go)
;;; prelude-go.el ends here
