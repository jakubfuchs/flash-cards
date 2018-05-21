;; DATABASE init
(defvar *db* nil)
(defvar *loaded-db-file* nil)

(defun make-flash-card (&key function my-docs (learned? nil))
  (list
   :function function :my-docs my-docs :learned? learned?))

(defun add-db-record (make-fn)
  (push make-fn *db*))

;; DUMPs
;; Database
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; Functions
(defun dump-functions ()
  (dolist (cd *db*)
    (format t "~a~%" (getf cd :function))))

;; Functions--not-learned

;; Functions--learned

;; PROMPTs
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-flash-card ()
  (make-flash-card
   :function (prompt-read "Function")
   :my-docs (prompt-read "My Description")
   :learned? (y-or-n-p "Learned?")))

;; MAIN METHODS
(defun add-flash-card ()
  (loop (add-db-record (prompt-for-flash-card))
     (if (not (y-or-n-p "Add another? ")) (return)))
  (if (y-or-n-p "Save Database? ") (save-db *loaded-db-file*)))

(defun update-flash-cards ())

(defun delete-flash-cards ())

;; SAVE & LOADS
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in))
      (setf *loaded-db-file* filename))))

;; QUERING
(defun select-by-function (function)
  (if (not *db*) (format t "No database loaded"))
  (remove-if-not
   #'(lambda (card) (equal (getf card :function) function))
   *db*))
