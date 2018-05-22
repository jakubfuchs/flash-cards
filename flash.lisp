;; Dynamic VARIABLES
(defvar *db* nil)
(defvar *loaded-db-file* nil)
(defvar *ex-db* nil)

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

;; UPDATING
(defun update-flash-card ())

(defun delete-flash-card ())

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

(defun dump->documented ()
  (dump-select (where :my-docs (not ""))))

;; MAIN learn-it function
(defun start ()
  "nth flash-card based on random number
dump function name
wait for user input/let him write description
dump description
prompt user if it is learned now
  (if it means updated prop -> silent update and save database)
remove function from running excersise
print another from remaining (nth rand to be ajusted by length of remaining)
:quit to exit excersise at any prompt")
