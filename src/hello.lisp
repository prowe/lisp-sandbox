(defun sayHello (name)
    "Says hello"
    (let ((message (concatenate 'string "Hello " name)))
        (print message)
    )
)
(sayHello "You")