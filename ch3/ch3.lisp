; Special forms for definitions.

(defstruct name
  first
  (middle nil)
  last)

; Note that the above definition automatically creates make-name, name-p and 
; accessor function name-first, name-middle, and name-last.
; Create one in the REPL using:
; (setf b (make-name :first 'Barney :last 'Rubble))

; Special forms for conditionals.

(defun tax-bracket (income)
  "Determine what percent tax should be paid for this income."
  (cond ((< income 10000.00) 0.00)
        ((< income 30000.00) 0.20)
        ((< income 50000.00) 0.20)
        ((< income 70000.00) 0.30)
        (t                   0.35)))
