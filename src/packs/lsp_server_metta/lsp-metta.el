;;; lsp-metta.el --- lsp client for metta -*- lexical-binding: t; -*-

;; Author: Douglas R. Miles
;; Keywords: lsp, metta

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; LSP client for metta language.
;;
;;; Code:


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(fundamental-mode . ("swipl" "-g" "use_module(library(lsp_server_metta))."
                                                 "-g" "lsp_server_metta:main"
                                                 "-t" "halt"
                                                 "--" "stdio"))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(metta-mode . ("swipl" "-g" "use_module(library(lsp_server_metta))."
                                                 "-g" "lsp_server_metta:main"
                                                 "-t" "halt"
                                                 "--" "stdio"))))




(require 'lsp-mode)
(with-eval-after-load 'lsp-mode
  ;; Add to lsp language configurations
  (add-to-list 'lsp-language-id-configuration '(".*\\.metta$" . "metta"))

  ;; Define a new lsp client group
  (defgroup lsp-metta nil
    "LSP support for MeTTa."
    :group 'lsp-mode
    :link '(url-link "https://github.com/trueagi-io/metta-wam"))

  ;; Register the lsp client
  (lsp-register-client
    (make-lsp-client :new-connection
                     (lsp-stdio-connection (list "swipl"
                                                 "-g" "use_module(library(lsp_server_metta))."
                                                 "-g" "lsp_server_metta:main"
                                                 "-t" "halt"
                                                 "--" "stdio"))
                     :activation-fn (lsp-activate-on "metta")
                     :priority -1                    
                     :server-id 'metta-lsp)))

;; swipl "-g" "use_module(library(lsp_server_metta))." "-g" "lsp_server_metta:main" "-t" "halt" "--" "stdio"
(use-package lsp-mode
  :ensure t
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.metta$" . "metta"))

  (defgroup lsp-metta nil
    "LSP support for MeTTa."
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
                     :priority -1
                     :server-id 'metta-lsp)))

