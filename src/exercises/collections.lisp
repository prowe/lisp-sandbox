(load "lisp-unit")
(use-package :lisp-unit)

(defun reduce2 (lst reducer initial-value)
    "Executes a reduce operation by applying the reducer to each element in the list left to right"
    (if (null lst)
        initial-value
        (reduce2
            (rest lst)
            reducer
            (funcall reducer initial-value (first lst))
        )
    )
)

(define-test reduce-tests
    (assert-equal 3 (reduce2 `() nil 3))
    (assert-equal 6 (reduce2 `(1 2 3) (lambda (acc x) (+ acc x)) 0))
)

(defun map2 (lst mapper)
    "Returns a new list by applying mapper to each element of the list"
    (if (null lst)
        `()
        (cons
            (funcall mapper (first lst))
            (map2 (rest lst) mapper)
        )
    )
)

(define-test map-tests
    (assert-equal `() (map2 `() nil))
    (assert-equal `(1 2) (map2 `(0 1) (lambda (x) (+ 1 x))))
)

(defun filter2 (lst predicate)
    "Returns a new list containing only items that match predicate"
    (cond
        ((null lst) `())
        ((funcall predicate (first lst))
            (cons
                (first lst)
                (filter2 (rest lst) predicate)
            ))
        (t (filter2 (rest lst) predicate))
    )
)

(defun slice (lst start end)
    "Returns a new list from element index start to (but not including) element at index end"
    (cond
        ((null lst) `())
        (
            (cons
                (first lst)
                (slice (rest lst) (- start 1) (- end 1)))
        )
        (t )
    )
)

(define-test slice-tests
    (assert-equal `(2 3) (slice `(1 2 3 4 5) 1 3))
    (assert-equal `(2 3) (slice `(1 2 3) 1 5))
)

(define-test filter-tests
    (assert-equal `(1 3) (filter2 `(1 2 3) (lambda (x) (= 1 (mod x 2)))))
    (assert-equal `() (filter2 `() nil))
)

(let (
    (*print-failures* t)
    (*print-errors* t))
    (run-tests :all)
)