;;; lsp-metta.el --- LSP client for Metta -*- lexical-binding: t; -*-

;; Author: Douglas R. Miles
;; Keywords: lsp, metta

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This Emacs package provides a Metta language major mode and integrates
;; an LSP client for enhanced language features. To use this package,
;; simply place it in your Emacs directory, typically ~/.emacs.d/,
;; and add the following line to your ~/.emacs or init.el file:
;;
;; (load "path/to/lsp-metta.el")
;;
;; Ensure you adjust "path/to/" to the actual path where you saved this file.
;; This will set up the major mode and LSP client whenever you open a Metta
;; file with the .metta extension.
;;

;; Example using use-package:
;;
;; (use-package metta-mode
;;   :load-path "path/to/lsp-metta"  ; Adjust the path as needed
;;   :config
;;   (setq some-metta-config-var 'value)
;;   (add-hook 'metta-mode-hook #'lsp))
;;
;;; Code:

;; Ensure lsp-mode is loaded for LSP client configuration
(require 'lsp-mode)

;; Define the mode's keymap
(defvar metta-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Define keybindings here if needed
    map)
  "Keymap for `metta-mode'.")

;; Define syntax table
(defvar metta-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; MeTTa comments start with ';'
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `metta-mode'.")

;; Define the list of keywords
(defvar metta-font-lock-keywords
  (let* (
         (x-keywords '("=" "if" "superpose" "!" "let" "let*" "car-atom" "cdr-atom"))
         (x-constants '("True" "False" "Type"))
         ;; Regex patterns to match any keyword starting with '@' or '&'
         (at-constants "@\\w+")
         (amp-constants "&\\w+")
         (x-keywords-regexp (regexp-opt x-keywords 'words))
         (x-constants-regexp (regexp-opt x-constants 'words)))
    `((,x-keywords-regexp . font-lock-keyword-face)
      (,x-constants-regexp . font-lock-constant-face)
      (,at-constants . font-lock-constant-face)  ;; Apply constant face to @ prefixed words
      (,amp-constants . font-lock-constant-face) ;; Apply constant face to & prefixed words
      )))

(define-derived-mode metta-mode prog-mode "Metta"
  "A major mode for editing Metta language code."
  (set-syntax-table metta-mode-syntax-table)
  (setq font-lock-defaults '(metta-font-lock-keywords)))

;; Register .metta files with metta-mode
(add-to-list 'auto-mode-alist '("\\.metta\\'" . metta-mode))

;; LSP client configuration for Metta language
(with-eval-after-load 'lsp-mode
  ;; Register the language ID
  (add-to-list 'lsp-language-id-configuration '(metta-mode . "metta"))
  (defgroup lsp-metta nil
    "LSP support for Metta."
    :group 'lsp-mode
    :link '(url-link "https://github.com/trueagi-io/metta-wam"))
  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection (list "swipl"
                                                "-g" "use_module(library(lsp_server_metta))."
                                                "-g" "lsp_server_metta:main"
                                                "-t" "halt"
                                                "--" "stdio"))
                    :activation-fn (lsp-activate-on "metta")
                    ;; :major-modes '(metta-mode)
                    :priority -1
                    :multi-root t
                    :server-id 'metta-lsp))
      (add-hook 'metta-mode-hook #'lsp))

(require 'ffap)

(defun my-find-file-at-line (path)
  "Open the specified file at a given line number or range extracted from PATH."
  (let ((components (split-string path "#L")))
    (if (> (length components) 1)
        (let* ((file-range (split-string (cadr components) "-"))
               (file (url-unhex-string (car components)))
               (start-line (string-to-number (car file-range)))
               (end-line (if (> (length file-range) 1) (string-to-number (cadr file-range)) start-line)))
          (if (file-exists-p file)
              (progn
                (find-file file)
                (goto-line start-line)
                (when (> end-line start-line)
                  (push-mark (point) nil t)
                  (goto-line (1+ end-line)) ;; Move slightly past to include the end line fully
                  (end-of-line))
                (message "Opened %s from line %d to %d" file start-line end-line))
            (error "File does not exist: %s" file)))
      (find-file (url-unhex-string path)))))

(with-eval-after-load 'ffap
  (ffap-url-unwrap-remote "file")
  (setq ffap-url-fetcher 'my-find-file-at-line))

(defun my-md-follow-link ()
  "Custom follow link function to handle file links with line numbers in Markdown."
  (interactive)
  (let ((link (thing-at-point 'url)))
    (if (and link (string-match "^file://" link))
        (my-find-file-at-line link)
      (markdown-follow-link-at-point))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "<mouse-1>") 'my-md-follow-link))

;; Provide the Metta mode feature
(provide 'metta-mode)
;;; lsp-metta.el ends here
