;;; jonprl-mode.el --- A major mode for editing JonPRL files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5") (yasnippet "0.8.0"))
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
(require 'yasnippet)

;; subr-x is allowed to fail because the relevant functions are also
;; defined in jonprl-compat under a feature gate. subr-x was
;; introduced in Emacs 24.4, and it would be nice to support 24.3 for now.
(require 'subr-x nil t)

(require 'jonprl-compat)


;;; Customization options and settings

(defgroup jonprl nil "Customization options for JonPRL"
  :prefix 'jonprl- :group 'languages)

(defcustom jonprl-path "jonprl"
  "The path to the jonprl executable."
  :type 'file
  :group 'jonprl)

(defcustom jonprl-pretty-symbols
  '(("*" . ?×)
   ("->" . ?→)
   (":=" . ?≔)
   ("<-" . ?←)
   ("<>" . ?★)
   ("=>" . ?⇒)
   ("=def=" . ?≝)
   ("base" . ?𝔻)
   ("lam" . ?λ)
   ("member" . ?∈)
   ("nat" . ?ℕ)
   ("unit" . ?𝟙)
   ("void" . ?𝟘)
   ("^" . ?⌢)
   ("<:" . ?◃)
   ("bot" . ?⊥)
   ("|-" . ?⊢))
  "Pretty replacement symbols for JonPRL syntax."
  :type '(alist :value-type string :key-type character)
  :group 'jonprl)

(defcustom jonprl-mode-hook '(jonprl-update-operators eldoc-mode)
  "The hook to run when initializing JonPRL mode."
  :type 'hook
  :group 'jonprl
  :options '(yas-minor-mode jonprl-update-operators eldoc-mode))

(defcustom jonprl-mode-after-save-hook '(jonprl-update-operators)
  "The hook to run when saving JonPRL files.
By default, this updates the snippets.  If that gets too slow,
remove `jonprl-update-operators' from this hook and invoke it
manually."
  :type 'hook
  :group 'jonprl
  :options '(jonprl-update-operators))

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

(defface jonprl-operator-face '((t (:inherit font-lock-constant-face)))
  "The face used to highlight JonPRL operators."
  :group 'jonprl)

(defface jonprl-name-face '((t (:inherit font-lock-variable-name-face)))
  "The face used to highlight JonPRL names."
  :group 'jonprl)

(defface jonprl-comment-face '((t (:inherit font-lock-comment-face)))
  "The face used to highlight JonPRL comments."
  :group 'jonprl)

;;; Syntax highlighting

(defconst jonprl-keywords '("Theorem" "Tactic" "Operator" "=def=" "Print" "Eval" "Search" "Infix" "Postfix" "Prefix" "Resource" "Declare")
  "Keywords for `jonprl-mode'.")

(defconst jonprl-vernacular-snippets
   (list
     (list "-thm" "Theorem $1 : [$2] {\n  $3\n}.\n" nil nil nil '((yas-indent-line 'fixed) (yas-wrap-around-region 'nil)) nil nil "JonPRL-Vernacular-Theorem")
     (list "-op" "Operator $1 : ($2).\n" nil nil nil nil nil nil "JonPRL-Verncular-Operator")
     (list "-def" "[$1] =def= [$2].\n" nil nil nil nil nil nil "JonPRL-Vernacular-Definition")
     (list "-wf" "Resource wf += { wf-lemma <$1> }.\n" nil nil nil nil nil nil "JonPRL-Vernacular-WF-Resource"))
   "yasnippet snippet defs for JonPRL's vernacular language")

(defvar jonprl-tactics ()
  "A list of the tactics to be highlighted in JonPRL mode.
This list is constructed from JonPRL's output.")

(defvar jonprl-operators ()
  "A list of the operators to be highlighted in JonPRL mode.
This list is constructed from JonPRL's output.")
(make-variable-buffer-local 'jonprl-operators)

(defvar jonprl-configuration-file ()
  "A configuration file for a JonPRL development.
If set, this is passed to the JonPRL compilation command instead of the current file.")
(make-variable-buffer-local 'jonprl-configuration-file)

(defun jonprl-highlight-operators (limit)
  "Search from point to LIMIT after an operator, setting the match data."
  (re-search-forward (regexp-opt (mapcar #'car jonprl-operators) 'words) limit t))

(defun jonprl-highlight-tactics (limit)
  "Search from point to LIMIT after a tactic, setting the match data."
  (re-search-forward (regexp-opt (mapcar #'car jonprl-tactics) 'words) limit t))

(defun jonprl-font-lock-defaults ()
  "Calculate the font-lock defaults for `jonprl-mode'."
  `('((,(regexp-opt jonprl-keywords 'words) 0 'jonprl-keyword-face)
      (jonprl-highlight-tactics 1 'jonprl-tactic-face)
      ("<\\(\\w+\\(\\s-+\\w+\\)*\\)>" 1 'jonprl-name-face)
      ("^\\s-*\\(|||.*\\)$" 1 'jonprl-comment-face)
      (jonprl-highlight-operators 1 'jonprl-operator-face))))

(defun jonprl--compilation-buffer-name-function (_mode)
  "Compute a buffer name for the jonprl-mode compilation buffer."
  "*JonPRL*")

(defun jonprl-update-available-tactics ()
  "Update the `jonprl-tactics' by asking JonPRL."
  (interactive)
  ;; Right now, JonPRL only lists the built-in tactics, so we don't
  ;; pass it a file name. This should result in maximum speed.
  (let ((tactics-list
         (condition-case nil
             (with-temp-buffer
               (call-process jonprl-path nil t nil " --list-tactics")
               (let ((tacs ()))
                 (goto-char (point-min))
                 (while (re-search-forward "^[ 	]*\\(?1:[a-zA-Z-_]+\\)\\(?:[ 	]+\\(?2:.*\\)\\)?$" nil t)
                   (let ((tac-name (match-string 1))
                         (tac-help (match-string 2)))
                     (push (list tac-name tac-help) tacs)))
                 tacs))
           ;; If JonPRL couldn't be run, fall back to something not
           ;; totally insane - either a previously-read tactic list
           ;; or some hard-coded tactics
           (error (if (null jonprl-tactics)
                      '(("cum" "@NUM?")
                        ("trace" "\"MESSAGE\"")
                        ("id" "fail")
                        ("hyp-subst" "(←|→) #NUM [TERM] @NUM?")
                        ("hypothesis" "#NUM")
                        ("witness" "[TERM]")
                        ("refine" "<NAME>")
                        ("unfold" "<(NAME @NUM)+>")
                        ("cut-lemma" "<NAME>")
                        ("lemma" "<NAME>")
                        ("reduce" "NUM?")
                        ("mem-cd" "auto")
                        ("assert" "[TERM] <NAME>?")
                        ("cstruct" "assumption")
                        ("csymmetry" "step")
                        ("symmetry" "creflexivty")
                        ("ext" "<NAME>? @LEVEL?")
                        ("eq-cd" "[TERM*]? <NAME*>? @LEVEL?")
                        ("elim" "#NUM [TERM]? <NAME*>?")
                        ("intro" "[TERM]? #NUM? <NAME*>?"))
                    jonprl-tactics)))))
    (setq jonprl-tactics tactics-list)))


;;; Invoking JonPRL

;; Compilation mode regexps, to be used for compilation mode
(defconst jonprl-parse-error-regexp
  "\\(Fail: Parse error at \\)\\([^:]+\\):\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)\\.\\([0-9]+\\):"
  "Regexp matching JonPRL parse errors.")

(defconst jonprl-tactic-fail-regexp
  "\\[\\(?1:\\(?2:[^:]+\\):\\(?3:[0-9]+\\)\\.\\(?4:[0-9]+\\)-\\(?5:[0-9]+\\)\\.\\(?6:[0-9]+\\)\\)\\]: tactic '\\(?7:.+\\)' failed with goal:"
  "Regexp matching JonPRL tactic failures.")

;; The following code is cribbed from idris-ipkg-mode:
;; Based on http://www.emacswiki.org/emacs/EmacsTags section "Finding tags files"
;; That page is GPL, so this is OK to include
(defun jonprl-find-file-upwards (suffix &optional allow-hidden)
  "Recursively searches each parent directory starting from the
directory of the current buffer filename or from
`default-directory' if that's not found, looking for a file with
name ending in SUFFIX.  Returns the paths to the matching files,
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (matching (if parent
                                         (ignore-errors (directory-files parent t (concat suffix "$")))
                                       nil)))
                      (cond
                       (matching matching)
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (let* ((file (buffer-file-name (current-buffer)))
           (dir (if file (file-name-directory file) default-directory)))
      (when dir
        (cl-remove-if #'(lambda (f)
                          (and (not allow-hidden)
                               (string-prefix-p "." (file-name-nondirectory f))))
                      (find-file-r dir))))))

(defun jonprl-command-args (should-print)
  "Compute the arguments to be passed to JonPRL, based on the (optional)
  configuration tile. The development will be checked silently unless
  'should-print' is true."
  (let* ((filename (buffer-file-name))
         (file (file-name-nondirectory filename))
         (config-file jonprl-configuration-file))
    (concat
     (if (not should-print) "--check " "")
       (if (or (not config-file) (equal config-file 'shut-up)) file config-file))))

(defun jonprl-forget-configuration ()
  "Forget the configuration file"
  (interactive)
  (setq jonprl-configuration-file nil)
  (message "Configuration file unset"))

(defun jonprl-read-configuration-file ()
  "Prompt the user for a configuration file, prepopulating with a .cfg file in
  the current directory or above, if possible."
  (let ((cfg-prompt "Set configuration file:")
        (cfg-default (jonprl-find-file-upwards "cfg")))
    (expand-file-name
      (if cfg-default
        (read-file-name cfg-prompt
                        (file-name-directory (car cfg-default))
                        (car cfg-default)
                        t
                        (file-name-nondirectory (car cfg-default)))
        (read-file-name cfg-prompt nil nil nil t)))))

(defun jonprl-set-configuration-file (filename)
  "Choose a configuration file for your JonPRL development"
  (interactive (list (jonprl-read-configuration-file)))
  (setq jonprl-configuration-file filename)
  (message (concat "Configuration file set to " filename)))

(defun jonprl-initialize-configuration ()
  "Check if a configuration file is loaded, and if not, prompt the user to choose one."
  (interactive)
  (unless jonprl-configuration-file
    (if (y-or-n-p "Do you want to choose a .cfg file?")
      (jonprl-set-configuration-file (jonprl-read-configuration-file))
      (setq jonprl-configuration-file 'shut-up))))

(defun jonprl-check-buffer ()
  "Load the current file into JonPRL."
  (interactive)
  (run-hooks 'jonprl-pre-check-buffer-hook)
  (jonprl-initialize-configuration)
  (let* ((filename (buffer-file-name))
         (dir (file-name-directory filename))
         (command (concat jonprl-path " " (jonprl-command-args nil)))

         ;; Emacs compile config stuff - these are special vars
         (compilation-buffer-name-function
          'jonprl--compilation-buffer-name-function)
         (default-directory dir))
    (compile command)))

(defconst jonprl-development-buffer-name "*JonPRL Development*"
  "The name for the JonPRL development output buffer.")

(defconst jonprl-development-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'jonprl-development-quit)
    map))

(define-derived-mode jonprl-development-mode fundamental-mode "JonPRL Dev"
  "Mode for JonPRL development output.
\\{jonprl-development-mode-map}")

(defun jonprl-print-development ()
  "Print the explicit form of the current buffer as a JonPRL development."
  (interactive)
  (let ((command-args (jonprl-command-args t))
        (view-read-only t)
        (old-buffer (current-buffer))
        (buffer (get-buffer-create jonprl-development-buffer-name)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (jonprl-development-mode)
      (read-only-mode 1)
      (message "Press 'q' to close the development.")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process jonprl-path nil t t command-args)))))

(defun jonprl-development-quit ()
  "Exit the development buffer."
  (interactive)
  (let ((buffer (get-buffer jonprl-development-buffer-name)))
    (when (and buffer (buffer-live-p buffer)) (kill-buffer buffer))))


;;; ElDoc

(defun jonprl-eldoc-function ()
  "Display the arity of the operator at point in the modeline."
  (let* ((op (thing-at-point 'symbol))
         (arity (assoc op jonprl-operators))
         (tac (assoc op jonprl-tactics)))
    (cond (arity
           (let ((op-name (car arity))
                 (valences (cdr arity)))
             (concat (propertize op-name 'face 'jonprl-operator-face)
                     "(" (string-join (mapcar #'(lambda (arg) (format "%d" arg)) valences) ";") ")")))
          (tac (concat (propertize (car tac) 'face 'jonprl-tactic-face)
                       (if (cadr tac)
                           (concat " " (cadr tac))
                         "")))
          (t nil))))



;;; yasnippet integration

(defun jonprl-snippet-escape (string)
  "Replace the yasnippet special characters in STRING with their escaped forms."
  (replace-regexp-in-string "\\([`$\\]\\)" "\\\\\\1" string))

(defun jonprl-arity-to-snippet (arity)
  "Convert an operator ARITY to a yasnippet definition.
The `car' of ARITY should be a string, naming the operator, and
the `cdr' should be a potentially-empty list of natural
numbers."
  (let ((op-name (car arity))
        (arg-valences (cdr arity))
        (uniquifier 0))
    (concat
     (jonprl-snippet-escape op-name)
     "("
     (string-join
      (mapcar #'(lambda (v)
                  (concat
                   (string-join
                    (cl-loop for i from 0 to (- v 1)
                             collecting (format "${%d:var}."
                                                (incf uniquifier))))
                   (format "${%d:term}" (incf uniquifier))))
              arg-valences) "; ")
     ")")))

(defun jonprl-parse-arities (buffer)
  "Take a BUFFER containing operator arities and convert them to the list representation."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((ops ()))
      (while (re-search-forward "^[ 	]*\\(?1:[^ ()	]+\\)[ 	]*(\\(?2:[^)]*\\))[ 	]*$" nil t)
        (let ((op (match-string 1))
              (pre-arity (match-string 2)))
          (push (cons op (mapcar #'string-to-number
                                 (split-string pre-arity ";" t "[ \\t]*")))
                ops)))
      ops)))

(defun jonprl-get-arities (&optional file)
  "Get the operator arities from FILE.
If FILE is nil, use the current buffer's file."
  (unless file
    (setq file (buffer-file-name (current-buffer))))
  (with-temp-buffer
    (call-process jonprl-path nil t t file "--list-operators")
    (jonprl-parse-arities (current-buffer))))

(defun jonprl-define-snippets (operators)
  "Define a collection of yasnippet snippets for OPERATORS.
OPERATORS must be a list of operator arity definitions.  An
operator arity definition is a list whose `car' is an operator
name and whose `cdr' is a list of valences.  A valence is a
natural number."
  (let ((snippet-defs
         (cl-loop for op in operators
                  when (consp (cdr op)) ;; non-nullary
                  collect (list (car op) ;; key
                                (jonprl-arity-to-snippet op) ;; template
                                op ;; name
                                nil ;; condition
                                nil ;; group
                                nil ;; expand-env
                                nil ;; file
                                nil ;; keybinding
                                (concat "JonPRL-" (car op)) ;; uuid - we want to replace old definitions
                                ))))
    (yas-define-snippets 'jonprl-mode snippet-defs)))

(defun jonprl-update-operators ()
  "Update the JonPRL snippets for the current buffer."
  (interactive)
  (let ((operators (jonprl-get-arities)))
    (when operators ;; don't throw away operators if buffer doesn't parse
      (jonprl-define-snippets operators)
      (setq jonprl-operators operators))))


;;; Means of invoking commands: tool bar and keybindings

(defvar jonprl-mode-path nil
  "Directory containing the `jonprl-mode' package.
This is used to load resource files such as images.  The default
value is automatically computed from the location of the Emacs
Lisp package.")
(when load-file-name
  (setq jonprl-mode-path (file-name-directory load-file-name)))


(defvar jonprl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'jonprl-check-buffer)
    (define-key map (kbd "C-c C-c") 'jonprl-print-development)
    map))

;; When the icon is supported, build a toolbar. This avoids problems
;; on console versions of Emacs that don't have PNG support.
(when (image-type-available-p 'png)
  (define-key jonprl-mode-map [tool-bar sep] '(menu-item "--"))
  (define-key-after jonprl-mode-map [tool-bar check-buffer]
    `(menu-item "Check" jonprl-check-buffer
                :enable t
                :visible t
                :help "Check in JonPRL"
                :image ,(create-image (concat jonprl-mode-path "jonprl-icon.png")))
    t)
  (define-key-after jonprl-mode-map [tool-bar print-development]
    `(menu-item "Print" jonprl-print-development
                :enable t
                :visible t
                :help "Print Development"
                :image ,(create-image (concat jonprl-mode-path "jonprl-icon.png")))
    t))


;;; Syntax table

(defvar jonprl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?' "w" table)
    (modify-syntax-entry ?∈ "w" table)
    (modify-syntax-entry ?⋂ "w" table)
    (modify-syntax-entry ?\( "()1n" table)
    (modify-syntax-entry ?*  ". 23n" table)
    (modify-syntax-entry ?\) ")(4n" table)
    table))


;;; Standard completion

(defun jonprl-complete-at-point ()
  "Attempt to complete at point for JonPRL keywords and tactics."
  (when (looking-back "\\w+" nil t)
    (let* ((match (match-string-no-properties 0))
           (start (match-beginning 0))
           (end (match-end 0))
           (candidates (cl-remove-if-not (apply-partially #'string-prefix-p match)
                                         (append jonprl-keywords
                                                 (mapcar #'car jonprl-tactics)
                                                 (mapcar #'car jonprl-operators)))))
      (if (null candidates) () (list start end candidates)))))


;;; Menus
(easy-menu-define jonprl-mode-menu jonprl-mode-map
  "Menu for JonPRL major mode"
  `("JonPRL"
    ["Check" jonprl-check-buffer t]
    ["Print development" jonprl-print-development t]))


;;; The mode itself

(defun jonprl-mode-run-after-save-hook ()
  "Run JonPRL-specific operations when saving a file."
  (run-hooks 'jonprl-mode-after-save-hook))

;;;###autoload
(define-derived-mode jonprl-mode prog-mode "JonPRL"
  "A major mode for JonPRL files.
     \\{jonprl-mode-map}
Invokes `jonprl-mode-hook'."
  (setq-local comment-start "|||")
  (jonprl-update-available-tactics)
  (setq-local font-lock-defaults (jonprl-font-lock-defaults))
  (setq-local prettify-symbols-alist jonprl-pretty-symbols)
  (setq-local imenu-generic-expression
              '(("Definitions" "^\\s-*\\\[\\(.+\\)\\\]\\s-+=def=" 1)
                ("Theorems" "Theorem\\s-+\\(\\w+\\)" 1)
                ("Operators" "Operator\\s-+\\(\\w+\\)" 1)))
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local completion-at-point-functions '(jonprl-complete-at-point))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               (".*\\(|\\)||.+\\(
\\)"
                (1 "<")
                (2 ">"))))
  (set-input-method "TeX")
  (add-to-list 'compilation-error-regexp-alist-alist
               `(jonprl-parse-error
                 ,jonprl-parse-error-regexp
                 2 (3 . 5) (4 . 6) 2))
  (add-to-list 'compilation-error-regexp-alist-alist
               `(jonprl-tactic-fail
                 ,jonprl-tactic-fail-regexp
                 2 (3 . 5) (4 . 6) 2 1 (7 'jonprl-tactic-face)))
  (cl-pushnew 'jonprl-parse-error compilation-error-regexp-alist)
  (cl-pushnew 'jonprl-tactic-fail compilation-error-regexp-alist)

  ;; Eldoc
  (setq-local eldoc-documentation-function 'jonprl-eldoc-function)

  ;; Enable Yasnippet integration
  ;; This is a customizable hook, hence the indirection.
  (add-hook 'after-save-hook 'jonprl-mode-run-after-save-hook t t)
  (yas-define-snippets 'jonprl-mode jonprl-vernacular-snippets)
  (jonprl-update-operators))

;;;###autoload
(push '("\\.jonprl\\'" . jonprl-mode) auto-mode-alist)


(provide 'jonprl-mode)
;;; jonprl-mode.el ends here
