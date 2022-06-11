; Special forms for definitions.

(defstruct name
  first
  (middle nil)
  last)

; Note that the above definition automatically creates make-name, name-p and 
; accessor function name-first, name-middle, and name-last.

