
(defun pop-end (lst)
    "returns all but the last element of a list"
    (if (null (cdr lst))
        ()
        (cons (car lst) (pop-end (cdr lst)))
    )
)

(print (pop-end `(1 2 3 4)))