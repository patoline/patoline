;; emacs mode for patoline

(provide 'patoline-mode)
(require 'patoline-input)

(add-to-list 'auto-mode-alist '("\\.txp\\'" . patoline-mode))


(defconst patoline-font-lock-keywords
  (list (cons "\\\\[a-zA-Z][a-zA-Z0-9]*\\>" 'font-lock-keyword-face)
        (cons "^[ \t]*===*\\([^=]+=\\)*[^=]+===*" 'font-lock-doc-face)
        (cons "^[ \t]*---*\\(-[^-]+\\)*[^-]+---*" 'font-lock-doc-face)
        (cons "^[ \t]*===========*" 'font-lock-doc-face)
        (cons "^[ \t]*-----------*" 'font-lock-doc-face)
        (cons "^[ \t]*[=-]>[^\n]*\n[\r \t]*\n" 'font-lock-doc-face)
        (cons "^[ \t]*[=-]<\\([ \t]*[=-]<\\)*" 'font-lock-doc-face)
  )
  "Minimal highlighting expressions for Patoline mode.")

(defvar patoline-mode-hook nil)
(autoload 'patoline-mode "patoline-mode" "Major mode for editing Patoline documents." t)

(defvar patoline-program-buffer
  nil)

(defvar patoline-view-buffer
  nil)

(defvar patoline-error-regexp 
    '(("^[^\"]+\"\\([^\"]*\\)\", line \\([0-9]+\\), character \\([0-9]+\\):$" 1 2 3 1)
      ("^[^\"]+\"\\([^\"]+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):$" 1 2 (3 . 4) 1)))

(defun select-patoline-program-buffer ()
  (if (and patoline-program-buffer (buffer-live-p patoline-program-buffer))
      (set-buffer patoline-program-buffer)
    (progn
      (setq patoline-program-buffer (get-buffer-create "*patoline-interaction*"))
      (set-buffer patoline-program-buffer)
      )))

(defun select-patoline-view-buffer ()
  (if (and patoline-view-buffer (buffer-live-p patoline-view-buffer))
      (set-buffer patoline-view-buffer)
    (progn
      (setq patoline-view-buffer (get-buffer-create "*patoline-view*"))
      (set-buffer patoline-view-buffer)
      )))

(defun patoline-process-sentinel (p m)
  (if (not (string-match "finished\\.*" m))
      (display-buffer patoline-program-buffer t 'visible)
    (select-patoline-program-buffer)
    (if patoline-view-process 
	(signal-process patoline-view-process 1)))
  (message m))

(defun display-program-buffer ()
  "display patoline compilation buffer"
  (interactive)
  (display-buffer patoline-program-buffer t 'visible))

(defvar patoline-compile-format
  "patoline --edit-link --driver Bin \"%s\""
  "What to do to compile patoline document. Examples [patoline \"%s\"], [make]")

(defun patoline-compile ()
  "compile the current buffer with patoline"
  (interactive)
  (save-buffer)
  (save-excursion
    (let ((cmd-format (read-from-minibuffer "compile: " patoline-compile-format)))
      (setq patoline-compile-format cmd-format)
      (let ((cmd (format cmd-format buffer-file-name))
	    (dir-name (file-name-directory buffer-file-name)))
	(select-patoline-program-buffer)
	(erase-buffer)
	(cd dir-name)
	(compilation-start cmd t (lambda (name) "*patoline-interaction*") t)
	(set-process-sentinel (get-buffer-process patoline-program-buffer) 'patoline-process-sentinel)))))

(defvar patoline-default-env
  "center"
  "Patoline default environment")

(defun patoline-env ()
  "insert environment"
  (interactive)
  (let ((env (read-from-minibuffer "insert environment: " patoline-default-env)))
    (setq patoline-default-env env)
    (insert "\\begin{" env "}\n") 
    (save-excursion (insert "\n\\end{" env "}"))))

(defvar patoline-view-format
  "embedded"
  "What to do to view patoline document. Examples [embedded], [xpdf \"%s\"]")

(defvar patoline-view-process
  nil)

(defun patoline-view ()
  "view the pdf corresponding to the current buffer"
  (interactive)
  (let ((file-name 
	 (concat (file-name-sans-extension (buffer-file-name (current-buffer))) ".pdf"))
	(cmd-format (read-from-minibuffer "view: " patoline-view-format)))
    (setq patoline-view-format cmd-format)
    (if (string-equal cmd-format "embedded")
	(progn
	  (find-file-other-window file-name)
	  (save-excursion
	    (set-buffer (find-buffer-visiting file-name))
	    (auto-revert-mode)))
      (let ((cmd (split-string-and-unquote (format cmd-format file-name))))
	(save-excursion
	  (select-patoline-view-buffer)
	  (cd (file-name-directory file-name))
	  (setq patoline-view-process
		(apply 'start-process "patoline-view" nil (car cmd) (cdr cmd))))))))

(defun patoline-glview ()
  "view the pdf corresponding to the current buffer"
  (interactive)
  (let ((file-name 
	 (concat (file-name-sans-extension (buffer-file-name (current-buffer))) ".bin")))
      (let ((cmd (split-string-and-unquote (format "patolineGL \"%s\"" file-name))))
	(save-excursion
	  (select-patoline-view-buffer)
	  (cd (file-name-directory file-name))
	  (setq patoline-view-process
		(get-buffer-process
		 (apply 'make-comint "patoline-view" (car cmd) nil (cdr cmd))))))))

(defun patoline-goto (file line col)
  (find-file file)
  (goto-line line)
  (let ((bol (position-bytes (point))))
    (let ((dest (+ bol col)))
      (while (< (position-bytes (point)) dest)
	(forward-char)))))

(defvar patoline-mode-map
  (let ((patoline-mode-map (make-keymap)))
    (progn
      (define-key patoline-mode-map (kbd "C-c C-c") 'patoline-compile)
      (define-key patoline-mode-map (kbd "C-c C-e") 'patoline-make)
      (define-key patoline-mode-map (kbd "C-c C-a") 'patoline-env)
      (define-key patoline-mode-map (kbd "C-c C-v") 'patoline-glview)
      (define-key patoline-mode-map (kbd "C-c C-p") 'patoline-view)
      (define-key patoline-mode-map (kbd "C-c C-s") 'patoline-forward-search)
      (define-key patoline-mode-map (kbd "C-c C-l") 'display-program-buffer)
    patoline-mode-map))
  "Keymap for PATOLINE major mode")

(add-hook 'patoline-mode-hook (lambda () (flyspell-mode t)))

(defvar patoline-mode-syntax-table
  (let ((patoline-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" patoline-mode-syntax-table)
	(modify-syntax-entry ?' "w" patoline-mode-syntax-table)
	(modify-syntax-entry ?\$ "_" patoline-mode-syntax-table)
	
	; Comment styles are same as C++
	(modify-syntax-entry ?\) ")(4" patoline-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" patoline-mode-syntax-table)
	(modify-syntax-entry ?\( "()1" patoline-mode-syntax-table)
;	(modify-syntax-entry ?\n "> b" patoline-mode-syntax-table)
	patoline-mode-syntax-table)
  "Syntax table for patoline-mode")

(defun patoline-forward-search ()
  (interactive)
  (let ((line (line-number-at-pos))
	(col (- (position-bytes (point)) (position-bytes (line-beginning-position)))))
    (message  (format "e %d %d\n" line col))
    (save-excursion
      (select-patoline-view-buffer)
      (if patoline-view-process
	  (process-send-string patoline-view-process (format "e %d %d\n" line col))))))

(define-derived-mode patoline-mode fundamental-mode "WPDL"
  "Major mode for editing Patoline documents."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'patoline-mode)
  (line-number-mode t)
  (column-number-mode t)
  (setq mode-name "Patoline")
  (make-local-variable 'compilation-error-regexp-alist)
  (setq-default compilation-error-regexp-alist patoline-error-regexp)
  (use-local-map patoline-mode-map)
  (set-syntax-table patoline-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(patoline-font-lock-keywords))
  ;; Register our indentation function
;  (set (make-local-variable 'indent-line-function) 'patoline-indent-line)  
  (set-input-method "Patoline")
  (make-local-variable 'paren-mode)
  (make-local-variable 'patoline-compile-format)
  (make-local-variable 'patoline-view-format)
  (make-local-variable 'patoline-view-buffer)
  (make-local-variable 'patoline-view-process)
  (make-local-variable 'patoline-default-env)
  (if (featurep 'xemacs)
      (progn (require 'paren)
	     (paren-set-mode 'blink-paren t))
    (show-paren-mode 'mixed))
  (setq case-fold-search nil)
  (run-hooks 'patoline-mode-hook)
)

(require 'mmm-mode nil t)
(require 'tuareg nil t)

(if (and (featurep 'mmm-mode) (featurep 'tuareg))
    (progn 
      (setq mmm-global-mode 'maybe)
      (mmm-add-mode-ext-class nil "\\.txp" 'patoline-tuareg)
      (mmm-add-classes
       '((patoline-tuareg
	  :submode tuareg-mode
	  :front "\\\\\\([Cc]aml\\)\\|\\(diagram\\)("
	  :back "^)"
	  :insert ((?c tuareg-mode nil @ "\\Caml(\n"  @ " " _ " " @ "\n)" @)
		   (?d tuareg-mode nil @ "\\diagram(\n"  @ " " _ " " @ "\n)" @)))
	 ))))

(setq comment-start "(*")
(setq comment-end "*)")