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

; Special forms for dealing with variables and places

; Ex 3.1

(defun lam ()
  "Wrapper to allow lambda to be called."
  ((lambda (x)
     (+ x ((lambda (y)
             (* y y))
           x)))
   6))

(defstruct player (score 0) (wins 0))

(defun determine-winner (players)
  "Increment the wins for the player with the highest score."
  (incf (player-wins (first (sort players #'> :Key #'player-score)))))
