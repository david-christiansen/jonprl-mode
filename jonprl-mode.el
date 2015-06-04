;;; jonprl-mode.el --- A major mode for editing JonPRL files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Version: 0.0.1
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing JonPRL files. Right now, it's just
;; simple syntax highlighting.

;;; Code:
(require 'compile)
(require 'cl-lib)

(defgroup jonprl nil "Customization options for JonPRL"
  :prefix 'jonprl- :group 'languages)

(defcustom jonprl-path "jonprl" "The path to the jonprl executable."
  :type 'file
  :group 'jonprl)

(defcustom jonprl-mode-hook () "The hook to run when initializing JonPRL mode."
  :type 'hook
  :group 'jonprl)

(defcustom jonprl-pre-check-buffer-hook '(save-buffer)
  "A hook to run prior to checking the buffer."
  :type 'hook
  :group 'jonprl
  :options '(save-buffer))

(defface jonprl-keyword-face '((t (:inherit font-lock-keyword-face)))
  "The face used to highlight JonPRL keywords."
  :group 'jonprl)

(defface jonprl-tactic-face '((t (:inherit font-lock-function-name-face)))
  "The face used to highlight JonPRL tactics."
  :group 'jonprl)

(defface jonprl-name-face '((t (:inherit font-lock-variable-name-face)))
  "The face used to highlight JonPRL names."
  :group 'jonprl)

(defface jonprl-comment-face '((t (:inherit font-lock-comment-face)))
  "The face used to highlight JonPRL comments."
  :group 'jonprl)

(defconst jonprl-keywords '("Theorem" "Tactic" "=def=")
  "Keywords for `jonprl-mode'.")

(defconst jonprl-tactics
  '("cum" "unit-elim" "prod-eq" "prod-intro" "prod-elim" "pair-eq"
    "spread-eq" "fun-eq" "fun-intro" "fun-elim" "lam-eq" "ap-eq"
    "isect-eq" "isect-intro" "isect-elim" "isect-member-eq"
    "isect-member-case-eq" "witness" "hypothesis" "subst" "lemma"
    "unfold" "refine" "subset-eq" "subset-intro" "subset-elim"
    "subset-member-eq" "eq-eq" "auto" "univ-eq" "void-eq"
    "void-elim" "unit-eq" "unit-intro" "ax-eq" "mem-unfold"
    "assumption" "symmetry" "hyp-eq")
  "A list of the tactics to be highlighted in JonPRL mode.")

(defun jonprl-font-lock-defaults ()
  "Calculate the font-lock defaults for `jonprl-mode'."
  `('((,(regexp-opt jonprl-keywords 'words) 0 'jonprl-keyword-face)
      (,(regexp-opt jonprl-tactics 'words) 0 'jonprl-tactic-face)
      ("<\\(\\w+\\)>" 1 'jonprl-name-face)
      ("^\\s-*\\(|||.*\\)$" 1 'jonprl-comment-face))))


(defun jonprl--compilation-buffer-name-function (_mode)
  "Compute a buffer name for the jonprl-mode compilation buffer."
  "*JonPRL*")

(defun jonprl-check-buffer ()
  "Load the current file into JonPRL."
  (interactive)
  (run-hooks 'jonprl-pre-check-buffer-hook)
  (let* ((filename (buffer-file-name))
         (dir (file-name-directory filename))
         (file (file-name-nondirectory filename))
         (command (concat jonprl-path " " file))

         ;; Emacs compile config stuff - these are special vars
         (compilation-buffer-name-function
          'jonprl--compilation-buffer-name-function)
         (default-directory dir))
    (compile command)))

(defvar jonprl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'jonprl-check-buffer)
    map))

(defvar jonprl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?' "w" table)
    table))

(defun jonprl-complete-at-point ()
  "Attempt to complete at point for JonPRL keywords and tactics."
  (when (looking-back "\\w+" nil t)
    (let* ((match (match-string-no-properties 0))
           (start (match-beginning 0))
           (end (match-end 0))
           (candidates (cl-remove-if-not (apply-partially #'string-prefix-p match)
                                         (append jonprl-keywords
                                                 jonprl-tactics))))
      (if (null candidates) () (list start end candidates)))))

;;;###autoload
(define-derived-mode jonprl-mode prog-mode "JonPRL"
  "A major mode for JonPRL files.
     \\{jonprl-mode-map}
Invokes `jonprl-mode-hook'."
  (setq-local font-lock-defaults (jonprl-font-lock-defaults))
  (setq-local imenu-generic-expression
              '(("Definitions" "^\\s-*\\(\\w+\\)\\s-+=def=" 1)
                ("Theorems" "Theorem\\s-+\\(\\w+\\)" 1)))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local completion-at-point-functions '(jonprl-complete-at-point))
  (set-input-method "TeX"))

;;;###autoload
(push '("\\.jonprl$" . jonprl-mode) auto-mode-alist)

(provide 'jonprl-mode)
;;; jonprl-mode.el ends here
