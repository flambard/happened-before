(in-package :interval-tree-clock)

;;;
;;; Stamp
;;;

(defstruct (stamp (:constructor make-stamp (id-tree event-tree))
                  (:constructor make-seed-stamp ()))
  (id-tree    1 :read-only t :type id-tree)
  (event-tree 0 :read-only t :type event-tree))


(defun fork (stamp)
  (let ((event-tree (stamp-event-tree stamp)))
    (multiple-value-bind (left right) (split-id-tree (stamp-id-tree stamp))
      (values (make-stamp left  event-tree)
              (make-stamp right event-tree)))))

(defun peek (stamp)
  (values (make-stamp 0 (stamp-event-tree stamp)) stamp))

(defun join (stamp-a stamp-b)
  (make-stamp (sum-id-trees (stamp-id-tree stamp-a)
                            (stamp-id-tree stamp-b))
              (join-event-trees (stamp-event-tree stamp-a)
                                (stamp-event-tree stamp-b)) ))

(defun event (stamp)
  (let ((i-tree (stamp-id-tree stamp))
        (e-tree (stamp-event-tree stamp)))
    (multiple-value-bind (filled-e-tree tree-modified) (inflate i-tree e-tree)
      (if tree-modified
          (make-stamp i-tree filled-e-tree)
          (make-stamp i-tree (grow i-tree e-tree)) ))))


(defun send (stamp)
  (peek (event stamp)))

(defun receive (stamp-a stamp-b)
  (event (join stamp-a stamp-b)))

(defun sync (stamp-a stamp-b)
  (fork (join stamp-a stamp-b)))


(defun compare (stamp-a stamp-b)
  (compare-event-trees (stamp-event-tree stamp-a) (stamp-event-tree stamp-b)))

(defun happened-before (stamp-a stamp-b)
  (not (compare stamp-b stamp-a)))


(defun inflate (i-tree e-tree)
  (cond ((integerp i-tree)
         (if (= 0 i-tree)
             (values e-tree nil)
             (values (event-tree-maximum e-tree) (not (integerp e-tree))) ) )
        ((integerp e-tree)
         (values e-tree nil))
        ((let ((l (car i-tree)))
           (and (integerp l) (= 1 l)))
         (let ((filled (inflate (cdr i-tree) (event-tree-node-right e-tree))))
           (values
            (normalize-event-tree
             (make-event-tree-node
              :base  (event-tree-node-base e-tree)
              :left  (max (event-tree-maximum (event-tree-node-left e-tree))
                          (event-tree-minimum filled))
              :right filled))
            t)) )
        ((let ((r (cdr i-tree)))
           (and (integerp r) (= 1 r)))
         (let ((filled (inflate (car i-tree) (event-tree-node-left e-tree))))
           (values
            (normalize-event-tree
             (make-event-tree-node
              :base  (event-tree-node-base e-tree)
              :left  filled
              :right (max
                      (event-tree-node-maximum (event-tree-node-right e-tree))
                      (event-tree-node-minimum filled)) ))
            t)) )
        (t
         (values
          (normalize-event-tree
           (make-event-tree-node
            :base  (event-tree-node-base e-tree)
            :left  (inflate (car i-tree) (event-tree-node-left  e-tree))
            :right (inflate (cdr i-tree) (event-tree-node-right e-tree))))
          t) ) ))


(defun grow (i-tree e-tree)
  (if (integerp e-tree)
      (if (and (integerp i-tree) (= 1 i-tree))
          (values (1+ e-tree) 0)
          (multiple-value-bind (grown c)
              (grow i-tree (make-event-tree-node :base e-tree))
            (values grown (+ c 65536))) )
      (destructuring-bind (left-i-tree . right-i-tree) i-tree
        (cond ((and (integerp left-i-tree) (= 0 left-i-tree))
               (multiple-value-bind (grown c)
                   (grow right-i-tree (event-tree-node-right e-tree))
                 (values
                  (make-event-tree-node :base  (event-tree-node-base e-tree)
                                        :left  (event-tree-node-left e-tree)
                                        :right grown)
                  (1+ c))) )
              ((and (integerp right-i-tree) (= 0 right-i-tree))
               (multiple-value-bind (grown c)
                   (grow left-i-tree (event-tree-node-left e-tree))
                 (values
                  (make-event-tree-node :base  (event-tree-node-base e-tree)
                                        :left  grown
                                        :right (event-tree-node-right e-tree))
                  (1+ c))) )
              (t
               (multiple-value-bind (grown-left cl)
                   (grow left-i-tree (event-tree-node-left e-tree))
                 (multiple-value-bind (grown-right cr)
                     (grow right-i-tree (event-tree-node-right e-tree))
                   (if (< cl cr)
                       (values
                        (make-event-tree-node :base  (event-tree-node-base e-tree)
                                              :left  grown-left
                                              :right (event-tree-node-right e-tree))
                        (1+ cl))
                       (values
                        (make-event-tree-node :base  (event-tree-node-base e-tree)
                                              :left  (event-tree-node-left e-tree)
                                              :right grown-right)
                        (1+ cr)) ))) ) )) ))


;;;
;;; ID tree
;;;

(defun id-tree-p (tree)
  (or (typep tree '(integer 0 1))
      (and (consp tree)
           (id-tree-p (car tree))
           (id-tree-p (cdr tree)))))

(deftype id-tree ()
  '(satisfies id-tree-p))


(defun normalize-id-tree (tree)
  (if (integerp tree)
      (values tree nil)
      (multiple-value-bind (right right-updated) (normalize-id-tree (cdr tree))
        (multiple-value-bind (left left-updated) (normalize-id-tree (car tree))
          (cond ((and (integerp left)
                      (integerp right)
                      (= left right))
                 (values left t))
                ((or left-updated right-updated)
                 (values (cons left right) t))
                (t
                 (values tree nil)) ))) ))

(defun split-id-tree (tree)
  (if (integerp tree)
      (if (= 0 tree)
          (values 0 0)
          (values (cons 1 0) (cons 0 1)))
      (destructuring-bind (left . right) tree
        (cond ((= 0 left)
               (multiple-value-bind (a b) (split-id-tree right)
                 (values (cons 0 a) (cons 0 b))))
              ((= 0 right)
               (multiple-value-bind (a b) (split-id-tree left)
                 (values (cons a 0) (cons b 0))))
              (t
               (values (cons left 0) (cons 0 right))) )) ))

(defun sum-id-trees (tree-a tree-b)
  (cond ((integerp tree-a)
         (if (= 0 tree-a) tree-b 1))
        ((integerp tree-b)
         (if (= 0 tree-b) tree-a 1))
        (t
         (normalize-id-tree
          (cons (sum-id-trees (car tree-a) (car tree-b))
                (sum-id-trees (cdr tree-a) (cdr tree-b)))) ) ))


;;;
;;; Event tree
;;;

(defstruct event-tree-node
  (base  0 :read-only t :type (integer 0))
  (left  0 :read-only t :type event-tree)
  (right 0 :read-only t :type event-tree))

(defun event-tree-p (tree)
  (typep tree 'event-tree))

(deftype event-tree ()
  '(or (integer 0) event-tree-node))


(defun lift-event-tree (tree amount)
  (if (integerp tree)
      (+ tree amount)
      (make-event-tree-node :base (+ (event-tree-node-base tree) amount)
                            :left (event-tree-node-left tree)
                            :right (event-tree-node-right tree)) ))

(defun sink-event-tree (tree amount)
  (if (integerp tree)
      (- tree amount)
      (make-event-tree-node :base (- (event-tree-node-base tree) amount)
                            :left (event-tree-node-left tree)
                            :right (event-tree-node-right tree)) ))


(defun normalize-event-tree (tree)
  (if (integerp tree)
      (values tree nil)
      (multiple-value-bind (left left-updated)
          (normalize-event-tree (event-tree-node-left tree))
        (multiple-value-bind (right right-updated)
            (normalize-event-tree (event-tree-node-right tree))
          (if (and (integerp left)
                   (integerp right)
                   (= left right))
              (values (+ left (event-tree-node-base tree)) t)
              (let ((m (min (event-tree-minimum left)
                            (event-tree-minimum right))))
                (if (and (= 0 m) (not left-updated) (not right-updated))
                    (values tree nil)
                    (values (make-event-tree-node
                             :base (+ m (event-tree-node-base tree))
                             :left (sink-event-tree left m)
                             :right (sink-event-tree right m))
                            t) )) ))) ))


(defun event-tree-minimum (normalized-tree)
  (if (integerp normalized-tree)
      normalized-tree
      (event-tree-node-base normalized-tree)))

(defun event-tree-maximum (tree)
  (if (integerp tree)
      tree
      (+ (event-tree-node-base tree)
         (max (event-tree-maximum (event-tree-node-left  tree))
              (event-tree-maximum (event-tree-node-right tree)))) ))

(defun join-event-trees (tree1 tree2)
  (if (and (integerp tree1) (integerp tree2))
      (max tree1 tree2)
      (progn
        (when (integerp tree1)
          (setf tree1 (make-event-tree-node :base tree1)))
        (when (integerp tree2)
          (setf tree2 (make-event-tree-node :base tree2)))
        (when (> (event-tree-node-base tree1) (event-tree-node-base tree2))
          (rotatef tree1 tree2))
        (let ((b1 (event-tree-node-base  tree1))
              (l1 (event-tree-node-left  tree1))
              (r1 (event-tree-node-right tree1))
              (b2 (event-tree-node-base  tree2))
              (l2 (event-tree-node-left  tree2))
              (r2 (event-tree-node-right tree2)))
          (normalize-event-tree
           (make-event-tree-node
            :base  b1
            :left  (join-event-trees l1 (lift-event-tree l2 (- b2 b1)))
            :right (join-event-trees r1 (lift-event-tree r2 (- b2 b1))))) )) ))

(defun compare-event-trees (tree-a tree-b)
  (cond ((integerp tree-a)
         (<= tree-a (if (integerp tree-b)
                        tree-b
                        (event-tree-node-base tree-b))))
        ((integerp tree-b)
         (let ((a-base (event-tree-node-base tree-a)))
           (and (<= a-base tree-b)
                (compare-event-trees
                 (lift-event-tree (event-tree-node-left tree-a) a-base)
                 tree-b)
                (compare-event-trees
                 (lift-event-tree (event-tree-node-right tree-a) a-base)
                 tree-b))) )
        (t
         (let ((a-base (event-tree-node-base tree-a))
               (b-base (event-tree-node-base tree-b)))
           (and (<= a-base b-base)
                (compare-event-trees
                 (lift-event-tree (event-tree-node-left tree-a) a-base)
                 (lift-event-tree (event-tree-node-left tree-b) b-base))
                (compare-event-trees
                 (lift-event-tree (event-tree-node-right tree-a) a-base)
                 (lift-event-tree (event-tree-node-right tree-b) b-base))) )) ))
