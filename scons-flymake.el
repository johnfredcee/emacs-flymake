(require 'flymake)
(require 'eproject)
(require 'auto-complete)
(require 'traverselisp)

;; THIS IS A LOAD OF DINGOES KIDNEYS

(defun clang-location-argument ()
  (format "%s:%d:%d " 
		  (buffer-file-name)
		  (line-number-at-pos)
		  (1+ (- (point) (line-beginning-position)))))

(defun clang-completion-pattern ()
  (format "^COMPLETION: \\(%s[^\s\n:]*\\)\\(?: : \\)*\\(.*$\\)" ac-prefix))

(defun clang-eproject-file-completion ()
  (when (buffer-modified-p)
	(basic-save-buffer))
  (let* ((matches nil)
		 (clang-command (concat "clang -w -fsyntax-only "
								(eproject-attribute :clang-extra-flags)
								" -Xclang -code-completion-at="
								(clang-location-argument)
								(buffer-file-name)))
		 (completions (split-string (shell-command-to-string clang-command) "[\n\r]+")))
	(message "Clang command %s " clang-command)
	(let* ((completion-index 0)
		   (completion-pattern  (clang-completion-pattern)))
	  (while (< completion-index (length completions))
		(let ((completion (elt completions completion-index)))
		  (when (string-match completion-pattern completion)
			(push (match-string 1 completion) matches))
		  (setq completion-index (1+ completion-index)))))
	(message "%d matches" (length matches))
	matches))

(defun ac-clang-eproject-prefix ()
  (or (ac-prefix-symbol)
	  (let ((c (char-before)))
		(when (or (eq ?\. c)
				  ;; ->
				  (and (eq ?> c)
					   (eq ?- (char-before (1- (point)))))
				  ;; ::
				  (and (eq ?: c)
					   (eq ?: (char-before (1- (point))))))
		  (point)))))

(defun ac-clang-eproject-candidate ()
  (save-restriction
    (widen)
	(clang-eproject-file-completion)))

(defvar ac-source-clang-eproject
  '((candidates . ac-clang-eproject-candidate)
	(prefix     . ac-clang-eproject-prefix)))

;; END DINGOES KIDNEYS



;; making .h files --------------------

(defun flymake-get-scons-cmdline (source base-dir)
  (list "scons"
		(list "-suC"
			  (expand-file-name base-dir)
			  "SYNTAX=1"
			  (concat (file-name-sans-extension source) ".o"))))

(defun flymake-find-scons-possible-master-files (file-name master-file-dirs masks)
  "Find (by name and location) all possible master files.
Master files include .cpp and .c for .h.  Files are searched for
starting from the .h directory and max max-level parent dirs.
File contents are not checked."
  (flymake-log 3 "Find possible master files %s in %s" file-name master-file-dirs)
  (let* ((dirs (mapcar (lambda (x) (expand-file-name x (eproject-root))) master-file-dirs))
         (files  nil)
         (done   nil))
	(flymake-log 3 "Expanded to %s " dirs)
    (while (and (not done) dirs)
      (let* ((dir (car dirs))
             (masks masks))
		(flymake-log 3 "Looking in %s " dir)
        (while (and (file-exists-p dir) (not done) masks)
          (let* ((mask        (car masks))
                 (dir-files   (directory-files dir t mask)))
            (flymake-log 3 "dir %s, %d file(s) for mask %s"
                         dir (length dir-files) mask)
            (while (and (not done) dir-files)
              (when (not (file-directory-p (car dir-files)))
                (setq files (cons (car dir-files) files))
                (when (>= (length files) flymake-master-file-count-limit)
                  (flymake-log 3 "master file count limit (%d) reached" flymake-master-file-count-limit)
                  (setq done t)))
              (setq dir-files (cdr dir-files))))
          (setq masks (cdr masks))))
      (setq dirs (cdr dirs)))
    (when files
      (let ((flymake-included-file-name (file-name-nondirectory file-name)))
        (setq files (sort files 'flymake-master-file-compare))))
    (flymake-log 3 "found %d possible master file(s)" (length files))
    files))

(defun flymake-create-scons-master-file (source-file-name patched-source-file-name get-incl-dirs-f create-temp-f masks include-regexp)
  "Save SOURCE-FILE-NAME with a different name.
Find master file, patch and save it."
  (let* ((possible-master-files     (flymake-find-scons-possible-master-files source-file-name (eproject-attribute :source-dirs) masks))
         (master-file-count         (length possible-master-files))
         (idx                       0)
         (temp-buffer               nil)
         (master-file-name          nil)
         (patched-master-file-name  nil)
         (found                     nil))
	(flymake-log 3 "flymake create master file %s %s " source-file-name patched-source-file-name)
    (while (and (not found) (< idx master-file-count))
      (setq master-file-name (nth idx possible-master-files))
      (setq patched-master-file-name (funcall create-temp-f master-file-name "flymake_master"))
      (if (flymake-find-buffer-for-file master-file-name)
          (setq temp-buffer (flymake-copy-buffer-to-temp-buffer (flymake-find-buffer-for-file master-file-name)))
        (setq temp-buffer (flymake-read-file-to-temp-buffer master-file-name)))
      (setq found
            (flymake-check-patch-master-file-buffer
             temp-buffer
             master-file-name
             patched-master-file-name
             source-file-name
             patched-source-file-name
             (funcall get-incl-dirs-f (file-name-directory master-file-name))
             include-regexp))
      (setq idx (1+ idx)))
    (if found
        (list master-file-name patched-master-file-name)
      (progn
        (flymake-log 3 "none of %d master file(s) checked includes %s" master-file-count
                     (file-name-nondirectory source-file-name))
        nil))))



(defun flymake-init-scons-create-temp-source-and-master-buffer-copy (get-incl-dirs-f create-temp-f master-file-masks include-regexp)
  "Find master file (or buffer), create its copy along with a copy of the source file."
  (let* ((source-file-name       buffer-file-name)
         (temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f))
         (master-and-temp-master (flymake-create-scons-master-file
                                  source-file-name temp-source-file-name
                                  get-incl-dirs-f create-temp-f
                                  master-file-masks include-regexp)))

    (if (not master-and-temp-master)
        (progn
          (flymake-log 1 "cannot find master file for %s" source-file-name)
          (flymake-report-status "!" "")        ; NOMASTER
          nil)
      (setq flymake-master-file-name (nth 0 master-and-temp-master))
      (setq flymake-temp-master-file-name (nth 1 master-and-temp-master)))))

(defun flymake-master-scons-init (get-incl-dirs-f master-file-masks include-regexp)
  "Create make command line for a source file checked via master file compilation."
  (let* ((scons-args nil)
		 (temp-master-file-name (flymake-init-scons-create-temp-source-and-master-buffer-copy
								 get-incl-dirs-f 'flymake-create-temp-inplace
								 master-file-masks include-regexp)))
	(when temp-master-file-name
	  (let* ((buildfile-dir (eproject-root)))
		(if  buildfile-dir
			(setq scons-args (flymake-get-syntax-check-program-args
							  temp-master-file-name buildfile-dir nil nil 'flymake-get-scons-cmdline)))))
	scons-args))


(defun flymake-get-scons-include-dirs-imp (basedir)
  (flymake-add-project-include-dirs-to-cache (eproject-root) (eproject-attribute :include-dirs)))

(defun flymake-master-scons-header-init ()
  (flymake-master-scons-init
   'flymake-get-scons-include-dirs-imp
   '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'")
   "[ \t]*#[ \t]*include[ \t]*\"\\([[:word:]0-9/\\_.]*%s\\)\""))

;; making .cpp files ---------------------


(defun flymake-simple-scons-init-impl (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
         (source-file-name   buffer-file-name)
         (buildfile-dir      (eproject-root)))
    (if buildfile-dir
        (let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
          (setq args (flymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
                                                            use-relative-base-dir use-relative-source
                                                            get-cmdline-f))))
    args))

(defun flymake-simple-scons-init ()
  (flymake-simple-scons-init-impl 'flymake-create-temp-inplace t t "SConstruct" '
								  flymake-get-scons-cmdline))


;; An SCons project has an SConstruct file in it's root directory
;; c-sources in its src/ directory and headers in it's include/ directory

(define-project-type scons (generic) (look-for "SConstruct")
  :relevant-files ("\.cpp$" "\.h$" "\.glsl$" "\.cg$" "\.material$" "\.log$")
  :include-dirs   ("." "./include")
  :source-dirs    ("." "./src"))

(add-hook 'scons-project-file-visit-hook 
		  '(lambda ()
			 ;; binary is same as project name - ignore it when searching
			 (add-to-list 'traverse-ignore-files (eproject-name))
			 (add-to-list 'traverse-ignore-files "SConstruct")
			 (set (make-local-variable 'sourcepair-header-path)
				  (quote ("." ".." "../include" "./include")))
			 (set (make-local-variable 'sourcepair-source-path)
				  (quote ("." ".." "./src" "../src")))
			 (set (make-local-variable 'flymake-master-file-dirs)
				  (quote ("." "./src" ".." "../src")))
			 (setq ac-sources '(ac-source-etags ac-source-words-in-buffer ac-source-words-in-same-mode-buffers))
			 (set (make-local-variable 'compile-command)
				  (format "scons -C %s %s " (eproject-root) (eproject-name)))
			 (set (make-local-variable 'flymake-allowed-file-name-masks)
				  '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-scons-init)
					("\\.\\(?:h\\(?:pp\\|xx\\|\\+\\+\\)?\\)\\'" flymake-master-scons-header-init flymake-master-cleanup)))
			 (when (file-exists-p (concat (eproject-root) "TAGS"))
			   (visit-tags-table (concat (eproject-root) "TAGS") t))))


(defun eproject-search (regexp &optional only)
  (interactive
   (list (traverse-read-regexp (if (fboundp 'read-regexp) "Regexp" "Regexp: "))
		 (read-string "CheckOnly: ")))
  (traverse-deep-rfind (eproject-root) regexp only))
  

(provide 'scons-flymake)