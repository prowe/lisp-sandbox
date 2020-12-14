(defun onlyIf (test ifTrue)
    (if (eval test) (eval ifTrue))
)

(onlyIf `t `(print "true"))