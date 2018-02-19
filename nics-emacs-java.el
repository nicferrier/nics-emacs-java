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

(defvar nj-list-of-poms nil
  "An a-list of Java projects with their source files.

CAR is the pom file-name and the CDR is the list of source files,
or before indexing, the pom file-name again.")

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

(defun nj-maven-buffer-init ()
  "Ensure we set the `compile-command` to maven."
  (make-variable-buffer-local 'compile-command)
  (let ((pom-dir (nj-pom-dir)))
    (setq compile-command (format"cd %s ; mvn -q test" pom-dir))))

(defun nj-init ()
  "Initialize Nic's Java Emacs helpers - add to a mode hook."
  (nj-java-file-init)
  (nj-maven-buffer-init))

(add-hook 'java-mode-hook 'nj-init)


;;; Indexing a Java project

(defun nj--make-index-procs (dir finished-cont)
  (let ((all-data "") ; used as a buffer between calls
        results)
    (list
     :filter (lambda (process data)
               (when data
                 (setq all-data (concat all-data data))
                 (let ((lines (reverse (split-string all-data "\n"))))
                   (setq results (append results (reverse (cdr lines))))
                   (setq all-data (car lines)))))
     :sentinel (lambda (process event)
                 (if (and (stringp event)
                          (equal event "finished\n"))
                     (funcall finished-cont results))))))

(defun nj--index (directory finished-cont)
  "Do an index of the project at DIRECTORY.

Pass the results as a list of lines to the lambda FINISHED-CONT."
  (let* ((default-directory directory)
         (proc-name (format "*nics-emacs-java-index-%s*" directory))
         (find-args (list "-name" "*.java" "!" "-name" ".#*"))
         (proc-args (list proc-name proc-name find-program))
         (proc (apply 'start-process (append proc-args find-args)))
         (proc-funs (nj--make-index-procs directory finished-cont)))
    (set-process-filter proc (plist-get proc-funs :filter))
    (set-process-sentinel proc (plist-get proc-funs :sentinel))))

(defun nj-file-name-directory (file-name)
  "Like `file-name-directory` but capable of recursion.

 (nj-file-name-directory \"/home/nicferrier/.profile\")
 => \"/home/nicferrier\"

Argument FILE-NAME the file name to directoryize."
  (let ((directory (file-name-directory file-name)))
    (if directory
        (substring directory 0 (- (length directory) 1))
      file-name)))

(defun nj-pom-project-name (pom-file-name)
  "Get the project name indicated by POM-FILE-NAME."
  (let ((dir (nj-file-name-directory pom-file-name)))
    (file-name-nondirectory dir)))


;;; Indexer control

(defun nj--idle-index-result-handler (pom-file-pair results)
  "Worker function for `nj-idle-index-handle-pom`.
Argument POM-FILE-PAIR the pair to alter.
Argument RESULTS the results."
  (let* ((directory (nj-file-name-directory
                     (nj-file-name-directory
                      (car pom-file-pair))))
         (abs-func (lambda (f) (expand-file-name f directory)))
         (file-list (mapcar abs-func results)))
    (setcdr pom-file-pair file-list)))

(defun nj-idle-index-handle-pom (pom-file-pair &optional completion)
  "Called by `nj-idle-indexer` to handle each POM-FILE-PAIR.

COMPLETION is an optional function to call when we're done."
  (let* ((pom-file-name (car pom-file-pair))
         (project (nj-pom-project-name pom-file-name))
         (directory (nj-file-name-directory pom-file-name)))
    (message "nics-emacs-java indexing project %s" project)
    (nj--index
     directory
     (lambda (results)
       (nj--idle-index-result-handler pom-file-pair results)
       (when (functionp completion)
         (funcall completion results))))))

(defun nj-index-this (directory)
  "Index the current (or specified) DIRECTORY.

There must be a pom.xml in the current directory or an error will
be raised."
  (interactive (list default-directory))
  (let ((pom-file (expand-file-name "pom.xml" directory)))
    (if (file-exists-p pom-file)
        (let ((pom-file-pair (cons pom-file pom-file)))
          (setq nj-list-of-poms (cons pom-file-pair nj-list-of-poms))
          (nj-idle-index-handle-pom pom-file-pair))
      ;; else
      (error "nic's emacs java: no pom project to index here."))))

(defun nj-idle-indexer ()
  (mapcar 'nj-idle-index-handle-pom nj-list-of-poms))

(defvar nj-file-indexer nil)

(defun nj-start-indexer ()
  (interactive)
  (setq nj-file-indexer (run-with-idle-timer 20 t 'nj-idle-indexer)))

(unless nj-file-indexer
  (nj-start-indexer))

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

(defun nics-java-init (groupid artifactid)
  "Initialize a new Maven project.

GROUPID and ARTIFACTID are passed to Maven."
  (interactive
   (list
    (read-from-minibuffer "new project maven groupid: ")
    (read-from-minibuffer "new project maven artifactid: ")))
  (let* ((maven-buffer-name
          (format "*nics-emacs-java-new-project-log-%s/%s*" groupid artifactid))
         (maven-output (prog1
                           (get-buffer-create maven-buffer-name)
                         (with-current-buffer (get-buffer maven-buffer-name)
                           (erase-buffer))))
         (command (format
                   "mvn -B archetype:generate -DarchetypeGroupId=org.apache.maven.archetypes -DgroupId=%s -DartifactId=%s"
                   groupid artifactid)))
    (switch-to-buffer-other-window maven-output)
    (shell-command command maven-output)))

(provide 'nj)

;;; nics-emacs-java.el ends here
