;; Dynamic VARIABLES
(defvar *db* nil)
(defvar *loaded-db-file* nil)
(defvar *ex-db* nil)
(defvar db-file-lisp "./flash-lisp.log")

;; DATABASE init
(defun make-flash-card (&key function my-docs (learned? nil))
  (list
   :function function :my-docs my-docs :learned? learned?))

(defun add-db-record (make-fn)
  (push make-fn *db*))

(defun add-key-to-make ())

;; DUMP database
(defun dump->db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; DUMP Functions
(defun dump->function-list ()
  (dolist (cd *db*)
    (format t "~a~%" (getf cd :function))))

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

;; ADDING new
(defun new-flash-card ()
  (loop (add-db-record (prompt-for-flash-card))
     (if (not (y-or-n-p "Add another? ")) (return)))
  (if (y-or-n-p "Save Database? ") (save-db *loaded-db-file*)))

;; SAVE & LOADS
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (&Optional filename)
  (let ((source (if filename filename db-file-lisp)))
    (with-open-file (in source)
      (with-standard-io-syntax
        (setf *db* (read in))
        (setf *loaded-db-file* source)))))

;; QUERING
;; simple function select
(defun select-function (function)
  (if (not *db*) (format t "No database loaded"))
  (remove-if-not
   #'(lambda (card) (equal (getf card :function) function))
   *db*))

;; more advanced quering
;; designet as (select (where :function "GETF"))
(defun select (selector-fn)
  (if (not *db*) (format t "No database loaded"))
  (remove-if-not selector-fn *db*))

;; selector-function
(defun where (&key function my-docs learned?)
  #'(lambda (flash-card)
      (and
       (if function (equal (getf flash-card :function) function) t)
       (if my-docs (equal (getf flash-card :my-docs) my-docs) t)
       (if learned? (equal (getf flash-card :learned?) learned?) t))))

;; select-with-selector-fn wraped for formated printing
(defun dump-select (selector-fn)
  (dolist (f (select selector-fn))
    (format t "~{~a:~10t~a~%~}~%" f)))

;; PREDEFINEG Queries
(defun dump->nonlearned ()
  (dump-select (where :learned? nil)))

(defun dump->learned ()
  (dump-select (where :learned? t)))

(defun dump->nondocumented ()
  (dump-select (where :my-docs "")))

(defun dump->function (fun)
  (dump-select (where :function (string-upcase fun))))

;; UPDATING
(defun update-flash-card ())

(defun delete-flash-card ())

;; MAIN learn-it function
(defun start ()
  "nth flash-card based on random number
dump function name
wait for input/write description (not evaluated0/:quit
dump description from my-docs
prompt user if it is learned now
  (if it means updated prop -> silently update and save database-to-file)
remove function from running excersise list
print another from remaining (nth rand to be ajusted by length of remaining)
repeat step 1....
with :quit (rather as set of commands) to exit and :help to help the excersise at any prompt")
