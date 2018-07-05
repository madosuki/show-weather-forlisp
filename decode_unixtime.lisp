(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro show-clock (year month date hour min)
  `(format nil "~d年~d月~2,'0d日 ~d時~2,'0d分" ,year ,month ,date ,hour ,min))


(defun decode-unixtime (n &optional (gmt nil))
  ;; dow is ommision "day of the week".
  (multiple-value-bind (sec min hour date month year weeknum daylight-p zone)
      (decode-universal-time n -9)
    (declare (ignore daylight-p zone sec weeknum))
    (if gmt
        (show-clock year month date hour min)
        (let1 tmp (+ year 70)
              (show-clock tmp month date hour min)))))

;; (defun main ()
;;   (decode-unixtime (get-universal-time)))
;; (main)
