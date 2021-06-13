(defun last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (first name))

(defun test-names ()
  '((John Q Public) (Malcolm X) (Admiral Grace Murray Hopper) (Spot)
    (Aristotle) (A A Milne) (Z Z Top) (Sir Larry Olivier) (Miss Scarlet)))

(defun first-names (names)
  (mapcar #' first-name names))

(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defparameter *quals*
  '(MD Jr)
  "A list of qualifications that can appear at the end of a name.")

(defun first-name2 (name)
  "Select the first name from a name  represented as a list. Skip titles."
  (if (member (first name) *titles*)
    (first-name2 (rest name))
    (first name)))
