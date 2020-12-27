(load "lisp-unit")
(use-package :lisp-unit)

(defun emphasize3 (x)
    (cond
        ((equal (first x) `good) (cons `great (rest x)))
        ((equal (first x) `bad) (cons `awful (rest x)))
        (t (cons `very x))
    )
)

(define-test e4.8
    (assert-equal `(great day) (emphasize3 `(good day)))
    (assert-equal `(awful day) (emphasize3 `(bad day)))
    (assert-equal `(very long day) (emphasize3 `(long day)))
)

(defun constrain (x min max)
    (cond
        ((< x min) min)
        ((> x max) max)
        (t x)
    )
)
(defun constrain-v2 (x min max)
    (if (< x min) min
        (if (> x max) max x)
    )
)

(define-test e4.10v1
    (assert-equal 10 (constrain 3 10 50))
    (assert-equal 3 (constrain 3 -50 50))
    (assert-equal 50 (constrain 92 -50 50))
)
(define-test e4.10v2
    (assert-equal 10 (constrain-v2 3 10 50))
    (assert-equal 3 (constrain-v2 3 -50 50))
    (assert-equal 50 (constrain-v2 92 -50 50))
)

(defun firstzero (lst)
    (cond
        ((= 0 (nth 0 lst)) `first)
        ((= 0 (nth 1 lst)) `second)
        ((= 0 (nth 2 lst)) `third)
        (t `none)
    )
)

(define-test e4.11
    (assert-equal `second (firstzero `(3 0 4)))
    (assert-equal `first (firstzero `(0 0 4)))
    (assert-equal `third (firstzero `(3 5 0)))
    (assert-equal `none (firstzero `(3 2 4)))
    ;; (assert-equal nil (firstzero 3 0 4))
)


(defun cycle (x)
    (if (= 99 x) 1 (+ x 1))
)
(define-test e4.12
    (assert-equal 2 (cycle 1))
    (assert-equal 99 (cycle 98))
    (assert-equal 1 (cycle 99))
)

(let (
    (*print-failures* t)
    (*print-errors* t))
    (run-tests :all)
)

