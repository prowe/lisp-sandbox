; this calculates 8 and returns it (just like a function)
(defmacro eight ()
    (+ 3 5)
)
(print (eight))

; this returns the expression `(+ 3 5)` which is then evaluated after being returned
(defmacro eight2 ()
    `(+ 3 5)
)
(print (eight2))
