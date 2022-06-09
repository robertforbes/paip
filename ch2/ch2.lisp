(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun one-of (set)
  "Pick one element of set and make a list of it."
  (list (random-elt set)))

(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))

(defun sentence ()    (append (noun-phrase) (verb-phrase)))

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is *simple-grammar*, but we
  can switch to other grammars.")

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate2 (phrase)
  "Generate a randcom sentence or phrase"
  (if (listp phrase)
    (mappend #'generate2 phrase)
    (let ((choices (rewrites phrase)))
      (if (null choices)
        (list phrase)
        (generate2 (random-elt choices))))))

; Exercise 2.1: Implement a version of generate that uses cond but avoids 
; calling rewrites twice.

(defun generate3 (phrase)
  "Generate a random sentence or phrase"
  (let ((choices (rewrites phrase)))
    (cond ((listp phrase)
           (mappend #'generate3 phrase))
          (choices
           (generate3 (random-elt choices)))
          (t (list phrase)))))

; Exercise 2.2: write a version of generate which explicitly differentiates
; between terminal and non-terminal symbols.

(defun non-terminal-p (category)
  "True if this is a category in the grammar."
  (not (null (rewrites category))))

(defun generate4 (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate4 phrase))
        ((non-terminal-p phrase)
         (generate4 (random-elt (rewrites phrase))))
        (t (list phrase))))


