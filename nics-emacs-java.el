;;; enterprise Java help -*- lexical-binding: t -*-

(require 'cc-mode)

(defun nj-dissam (buffer)
  (interactive
   (list (current-buffer)))
  (with-current-buffer buffer
    (let ((coding-system-for-write 'no-conversion))
      (let ((class-file-name (make-temp-file "class-file" nil ".class")))
        (write-region (point-min) (point-max) class-file-name)
        (erase-buffer)
        (let* ((command (concat "javap -constants -p -c " class-file-name))
               (javap-out (shell-command-to-string command))
               (lines (split-string javap-out "\n"))
               (tail-lines (cdr lines))
               (output (mapconcat 'identity tail-lines "\n")))
          (insert output)
          (java-mode)
          (not-modified)
          (setq buffer-read-only 't)
          (beginning-of-buffer))))))


;;; requirements

(defvar nj-list-of-poms nil)

(defun nj-pom-dir ()
  (locate-dominating-file default-directory "pom.xml"))

(defun nj-find-pom ()
  (expand-file-name "pom.xml" (nj-pom-dir)))

(defun nj-pom-things ()
  (let ((pom (nj-find-pom)))
    (xml-parse-file pom)))

(defun nj-java-file-init ()
  (let ((pom (nj-find-pom)))
    (unless (assoc pom nj-list-of-poms)
      (setq nj-list-of-poms (cons (cons pom pom) nj-list-of-poms)))))

(add-hook 'java-mode 'nj-java-file-init)


;;; Indexing a Java project

(defun nj--make-index-procs (dir finished-cont)
  (let ((all-data "") ; used as a buffer between calls
        results)
    (list
     :filter (lambda (process data)
               (when data
                 (setq all-data (concat all-data data))
                 (let ((lines (reverse (split-string all-data "\n"))))
                   (seq results (append results (reverse (cdr lines))))
                   (setq all-data (car lines)))))
     :sentinel (lambda (process event)
                 (if (and (stringp event)
                          (equal event "finished\n"))
                     (funcall finished-cont results))))))

(defun nj--index (finished-cont)
  "Do an index of the current project.

Pass the results as a list of lines to the lambda FINISHED-CONT."
  (let* ((dir (nj-pom-dir))
         (default-directory dir)
         (proc-name (format "*nics-emacs-java-index-%s*" dir))
         (find-args (list "-name" "*.java" "!" "-name" ".#*"))
         (proc (start-process proc-name proc-name find-program find-args))
         (proc-funs (nj--make-index-procs dir finished-cont)))
    (set-process-filter proc (plist-get proc-funs :filter))
    (set-process-sentinel proc (plist-get proc-funs :sentinel))))

(defun nj-file-name-directory (file-name)
  "Like `file-name-directory` but capable of recursion.

 (nj-file-name-directory \"/home/nicferrier/.profile\")
 => \"/home/nicferrier\"
"
  (let ((directory (file-name-directory file-name)))
    (if directory
        (substring directory 0 (- length directory) 1)
      file-name)))

(defun nj-pom-project-name (pom-file-name)
  (let ((dir (nj-pom-dir)))
    (file-name-nondirectory dir)))

(defun nj-idle-index-handle-pom (pom-file-pair &optional completion)
  (let* ((pom-file-name (car pom-file-pair))
         (project (nj-pom-project-name pom-file-name)))
    (message "nics-emacs-java indexing project %s" project)
    (nj--index
     (lambda (results)
       (setcdr pom-file-pair results)
       (when (functionp completion)
         (funcall completion results))))))

(defun nj-idle-indexer ()
  (mapcar 'nj-idle-index-handle-pom nj-list-of-poms))

(defvar nj-file-indexer nil)

(defun nj-start-indexer ()
  (interactive)
  (setq nj-file-indexer (run-with-idle-timer 20 t 'nj-idle-indexer)))

(nj-start-indexer)

(defun nj-stop-indexer ()
  (interactive)
  (cancel-timer nj-file-indexer))


;;; Extract things from the indexed project

(defun nj-project-file-list (completion)
  (let* ((pom (nj-find-pom))
         (pom-entry (cdr (assoc pom nj-list-of-poms))))
    (if (listp pom-entry)
        (funcall completion pom-entry)
      (nj-idle-index-handle-pom pom completion))))

(defun nj-complete (completion)
  (nj-project-file-list
   (lambda (file-list)
     (let* ((file-names (mapcar
                         (lambda (file-name)
                           (cons (file-name-base file-name) file-name))
                         file-list))
            (completed (completing-read "find file: " file-names nil t))
            (pair (assoc completed file-names))
            (completed-file-name (cdr pair)))
       (funcall completion completed-file-name)))))

(defmacro nj-await (function-name)
  (let ((symv (make-symbol "await-value")))
    `(let* (,symv)
       (funcall ,function-name (lambda (result) (setq ,symv result)))
       (while (eq ,symv nil) (sleep-for 0 100))
       ,symv)))

(defun nj-open-file-in-project (filename)
  (interactive
   (list (let ((value (nj-await 'nj-complete))) value)))
  (message "nic's emacs java opening java file: %s" filename)
  (let ((default-directory (nj-pom-dir)))
    (find-file filename)))

(defun nj-open-file-in-project-other-window (filename)
  (interactive
   (list (let ((value (nj-await 'nj-complete))) value)))
  (message "nic's emacs java opening java file: %s" filename)
  (let ((default-directory (nj-pom-dir)))
    (find-file-other-window filename)))

(defun nj-open-shell ()
  (interactive)
  (let ((default-directory (nj-pom-dir)))
    (eshell)))

(define-key java-mode-map (kbd "C-c f") 'nj-open-file-in-project)
(define-key java-mode-map (kbd "C-c 4 f") 'nj-open-file-in-project-other-window)
(define-key java-mode-map (kbd "C-c #") 'nj-open-shell)

;;; nics-emacs-java.el ends here
