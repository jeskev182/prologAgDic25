(defun suma(a b)
 (+ a b)
)

(defun areacuadro(a b)
 (* a b)
)

(defun factorial(x)
 (if (= x 0)
  1
  (* x (factorial(- x 1)))
 )
)

(defun fibonacci (n &optional (a 0) (b 1))
 (if (zerop n)
  nil
  (cons a (fibonacci (1- n) b (+ a b)))
 )