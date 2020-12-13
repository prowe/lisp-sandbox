(defun sayTimes (times)
    (loop
        (write-line (format nil "Saying ~D" times))
        (setq times (- times 1))
        (when (<= times 0) (return nil))
    )
)
(sayTimes 2)