(defpackage :interval-tree-clock
  (:documentation "Implementation of Interval Tree Clocks.")
  (:nicknames :hb-itc)
  (:use :cl)
  (:export

   #:stamp
   #:make-seed-stamp

   #:fork
   #:peek
   #:join
   #:event
   #:send
   #:receive
   #:sync
   #:compare
   #:happened-before

   ))


(defpackage :lamport-timestamp
  (:documentation "Implementation of Lamport timestamps.")
  (:nicknames :hb-lt)
  (:use :cl)
  (:export

   #:stamp
   #:make-seed-stamp

   #:fork
   #:join
   #:event
   #:send
   #:receive
   #:sync
   #:compare
   #:happened-before

   ))
