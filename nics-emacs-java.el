;;; enterprise Java help -*- lexical-binding: t -*-

(require 'hl-line)
(require 'cc-mode)
(require 'ansi-color)

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

;;; import finding

(defun nj-import-list ()
  "Find the list of imports in a java program.

This function proceeds by finding the first import statement from
the start of the file and then scanning every line going forward
for an import statement until it hits a line which is not either
blank or empty or a comment.

A list of the class names imported is returned."
  (save-excursion
    (goto-char (point-min))
    (goto-char (+ 1 (line-end-position)) )
    (let (imports)
      (re-search-forward "^import \\(.*\\);$" nil t)
      (while (progn
               (when (save-match-data
                       (string-match "^import .*" (match-string 0)))
                 (setq imports (cons (match-string-no-properties 1) imports)))
               (goto-char (+ 1 (line-end-position)))
               (or (looking-at "^//.*")
                   (looking-at "^[:space:]+$")
                   (looking-at "^$")
                   (looking-at "^import \\(.*\\);$"))))
      imports)))



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

    ;; to handle term output ones needs to:
    ;;
    ;;   (ansi-color-apply-on-region (point-min) (point-max))
    ;;
    ;; on the compiled region
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
         (find-args (list directory "-name" "*.java" "!" "-name" ".#*"))
         (proc-args (list proc-name proc-name find-program))
         (proc (apply 'start-file-process (append proc-args find-args)))
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

(defun nj-index-this (directory &optional completion)
  "Index the current (or specified) DIRECTORY.

There must be a pom.xml in the current directory or an error will
be raised.

COMPLETION can be supplied to call with the result when it's
done."
  (interactive (list default-directory))
  (let ((pom-file (expand-file-name "pom.xml" directory)))
    (if (file-exists-p pom-file)
        (let ((pom-file-pair (cons pom-file pom-file)))
          (setq nj-list-of-poms (cons pom-file-pair nj-list-of-poms))
          (nj-idle-index-handle-pom pom-file-pair completion))
      ;; else
      (error "Nic's Emacs java: no pom project to index here"))))

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
  "Return the cached file-list or call `nj-idle-index-handle-pom`.

COMPLETION is called with the result."
  (let* ((pom (nj-find-pom))
         (pom-entry (cdr (assoc pom nj-list-of-poms))))
    (if (and pom-entry (listp pom-entry))
        (funcall completion pom-entry)
      (let ((pom-entry (cons pom pom)))
        (setq nj-list-of-poms (cons pom-entry nj-list-of-poms))
        (nj-idle-index-handle-pom pom-entry completion)))))

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
(define-key java-mode-map (kbd "C-c .") 'nj-open-project)

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

(defconst nj-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'nj-list-open)
    (define-key map (kbd "q") 'nj-list-quit)
    map)
  "Keymap for nj-list major mode.")

(defun nj-list-mode ()
  "Major mode for the file list.

\\{nj-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map nj-list-mode-map)
  (setq major-mode 'nj-list-mode)
  (setq mode-name "nj-list-mode")
  (setq buffer-read-only 't)
  (hl-line-mode)
  (run-hooks 'nj-list-mode))

(defun nj--list-project (completion)
  (nj-project-file-list
   (lambda (file-list)
     (funcall completion file-list))))

(defun nj-list-project (directory)
  "List the project in DIRECTORY.

Use the `nj-list-of-poms` to get the list of Java files (or
regenerate it) and display them in a buffer list."
  (interactive
   (list
    (if (and (not current-prefix-arg)
             (file-exists-p (expand-file-name "pom.xml" default-directory)))
        default-directory
      ;; Else read a project
      (read-file-name "Project: " "~/javawork"))))
  (let* ((project-dir-name (substring directory 0 (- (length directory) 1)))
         (buffer-name (format "*java-%s*" (file-name-base project-dir-name)))
         (pom-file (expand-file-name "pom.xml" directory))
         (default-directory directory)
         (file-list (nj-await 'nj--list-project))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
        (erase-buffer)
        (goto-char (point-min))
        (mapcar (lambda (file-name)
                  (insert file-name)
                  (newline))
                file-list))
      (nj-list-mode))
    (switch-to-buffer buffer)))

(defun nj-open-project (dir)
  "Open the project in DIR.

See `nj-list-mode`, the mode the buffer is opened in."
  (interactive (list (nj-pom-dir)))
  (nj-list-project dir))

(defun nj-list-open (file)
  "Open FILE from the list of Java's."
  (interactive
   (list
    (buffer-substring (line-beginning-position) (line-end-position))))
  (unless (equal "" file)
    (find-file file)))

(defun nj-list-quit ()
  "Quit the Java file list."
  (interactive)
  (when (equal major-mode 'nj-list-mode)
    (kill-buffer (current-buffer))))

(provide 'nj)

;;; nics-emacs-java.el ends here
