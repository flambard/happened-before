(in-package :lamport-timestamp)

;;;
;;; Lamport timestamp
;;;

(defstruct (stamp (:constructor make-stamp (counter))
                  (:constructor make-seed-stamp ()))
  (counter 0 :read-only t :type integer))


(defun fork (stamp)
  (values (copy-stamp stamp) (copy-stamp stamp)))

(defun join (stamp-a stamp-b)
  (make-stamp (max (stamp-counter stamp-a) (stamp-counter stamp-b))))

(defun event (stamp)
  (make-stamp (1+ (stamp-counter stamp))))


(defun send (stamp)
  (event stamp))

(defun receive (stamp-a stamp-b)
  (event (join stamp-a stamp-b)))

(defun sync (stamp-a stamp-b)
  (fork (join stamp-a stamp-b)))


(defun compare (stamp-a stamp-b)
  (<= (stamp-counter stamp-a) (stamp-counter stamp-b)))

(defun happened-before (stamp-a stamp-b)
  (< (stamp-counter stamp-a) (stamp-counter stamp-b)))
