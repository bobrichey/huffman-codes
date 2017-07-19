;;; FILENAME: huffman-codes.lisp

;;; DESCRIPTION: Provides functions for creating and traversing Huffman trees from given messages,
;;; as well as encoding and decoding messages using the created trees.


; Load additional files

(load "frequency-list.lisp")


; Main functions

(defun make-huffman-tree (mess)
  "Returns the Huffman tree from a message, mess"
  (make-htree-helper (make-leaves nil (frequency-list mess))))

(defun encode (mess htree)
  "Encodes a message, mess, which is a list of symbols, using the Huffman tree htree"
  (if (null mess) nil
      (append (encode-symbol (first mess) htree) (encode (rest mess) htree))))

(defun decode (bin htree)
  "Decode a list of binary, bin, using the Huffman tree htree"
  (decode-helper bin htree htree))


; Helpers for main functions

(defun make-htree-helper (htrees)
  "Takes a list of htrees and successively merges the first two, adds the new tree to the list,
   and sorts the list by weight until the list contains a single htree"
  (if (= (length htrees) 1) (first htrees)
      (make-htree-helper 
       (htree-sort (cons (htree-merge (first htrees) (second htrees)) (rest (rest htrees)))))))

(defun make-leaves (leaves fl)
  "Returns a sorted list of huffman tree leaves, leaves, from a frequency list, fl"
  (if (null fl) (htree-sort leaves) 
      (make-leaves (cons (list (first fl)) leaves) (rest fl))))

(defun encode-symbol (symbol htree)
  "Encodes a symbol into binary by traversing a htree until a leaf is reached, appending a 0 for
   each left subtree visited and a 1 for each right subtree visited"
  (cond ((leaf-p htree) nil)
	((null (member symbol (htree-symbols (left-subhtree htree)))) 
	 (cons 1 (encode-symbol symbol (right-subhtree htree))))
	(t (cons 0 (encode-symbol symbol (left-subhtree htree))))))

(defun decode-helper (bin subhtree htree)
  "Takes a list of binary, bin, and two htrees. The subhtree is traversed according to the binary
   until a leaf is reached, at which point the symbol of the leaf is returned and decoding proceeds
   from the root of htree."
  (cond ((null bin) 
	 (append (htree-symbols subhtree)))
	((leaf-p subhtree) 
	 (append (htree-symbols subhtree) (decode-helper bin htree htree)))
	((= (first bin) 0) 
	 (decode-helper (rest bin) (left-subhtree subhtree) htree))
	(t (decode-helper (rest bin) (right-subhtree subhtree) htree))))


; Huffman tree ADT

(defun htree-less (htree1 htree2)
  "Returns T if weight of htree1 is less than htree2, else NIL"
  (< (htree-weight htree1) (htree-weight htree2)))

(defun htree-symbols (htree)
  "Returns the list of symbols stored in the root of htree"
  (first (root htree)))
	
(defun htree-weight (htree)
  "Returns the weight of a htree"
  (second (root htree)))
	
(defun root (htree)
  "Returns the root of a htree"
  (first htree))

(defun htree-sort (htrees)
  "Returns the list of htrees sorted by their weight"
  (sort htrees #'< :key #'htree-weight))

(defun htree-merge (htree1 htree2)
  "Returns the Huffman tree that results from merging the Huffman trees
   htree1 and htree2 into a single Huffman tree"
  (list (list (append (htree-symbols htree1) (htree-symbols htree2)) 
	(+ (htree-weight htree1) (htree-weight htree2)))
	htree1 htree2))

(defun leaf-p (htree)
  "Returns T if htree is a leaf, else NIL"
  (= (length (htree-symbols htree)) 1))

(defun left-subhtree (htree)
  "Returns the left sub-tree of a htree"
  (second htree))

(defun right-subhtree (htree)
  "Returns the right sub-tree of a htree"
  (third htree))
