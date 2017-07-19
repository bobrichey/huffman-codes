;;; FILENAME: frequency-list.lisp

;;; DESCRIPTION: Provides functions for creating a frequency list of the symbols
;;; contained in a list. Code is adapted from functions provided by Dr. Richard Wyatt.


(defun frequency-list (mess)
  "Returns a list of the symbols and their frequencies in a message, mess"
  (freq-list-helper mess nil))

(defun freq-list-helper (mess fl)
  "Builds a frequency list, fl, from a given message, mess"
  (if (null mess) fl
      (freq-list-helper (rest mess) (update (first mess) fl))))

(defun update (symbol fl)
  "Appends symbol to a frequency list, fl, if it is not present.
   Otherwise, increments its frequency."
  (cond ((null fl) (append-symbol symbol))
	((equal symbol (first-symbol fl)) (increment-symbol fl))
	(t (cons (first fl) (update symbol (rest fl))))))

(defun append-symbol (symbol)
  "Creates a new symbol/frequency pair with a frequency of 1"
  (list (list (list symbol) 1)))

(defun first-symbol (fl)
  "Returns the first symbol in a frequency list"
  (first (first (first fl))))

(defun increment-symbol (fl)
  "Increments the frequency of the first symbol in a frequency list"
  (cons (incpair (first fl)) (rest fl)))

(defun incpair (fp) 
  "Increments the frequency in a symbol/frequency pair"
  (list (first fp) (1+ (second fp))))
