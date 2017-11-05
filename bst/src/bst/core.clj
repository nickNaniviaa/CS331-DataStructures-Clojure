(ns bst.core)

;; # Introduction
;;
;; In this lab you get to write a BST like the one we did in class, only
;; this time it is a dictionary structure and not a set.
;; As such, the "data" element from before will have a key and value instead.

(defrecord BST [root size])
(defrecord BNode [left key value right])

(defn make-node
  ([key value]  (make-node nil key value nil))
  ([left key value right] (BNode. left key value right))
  )

(defn make-tree []
  (BST. nil 0))

;; # Size
;;
;; A warmup function.

(defn size "Return the size of the tree."
  [t]
  (:size t))



;; # Find
;;
;; We need two versions of find.  The first one takes a key and returns the
;; value.  The second takes a value and returns the key.  Note that the second
;; version of the function must search the entire tree!  If the search item is not
;; there, return nil.

(defn find "Look for a key and return the corresponding value."
  	[bst look-key] 
  	(loop [x (:root bst)]
  		(if(nil? x) nil
  		(cond (= (compare (:key x) look-key) 0) (:value x)
  			  (< (compare (:key x) look-key) 0) (recur (:right x))
  			  (> (compare (:key x) look-key) 0) (recur (:left x))
  		)))) 

(defn nodefindkey
	[x look-value]
	(cond (nil? x) nil
		(= (compare (:value x) look-value) 0)(:key x)
		:else (or (nodefindkey (:right x) look-value) (nodefindkey (:left x) look-value))))

(defn find-key "Look for a value and return the corresponding key."
  [bst look-value]
   (nodefindkey (:root bst) look-value))


;; # Add
;;
;; The nodes will be entered into the tree on the basis of their key.
;; If someone tries to add a key that is already there, we replace the value
;; with the new entry.
(defn nodeadd
	[bn nkey nval]
	(cond (= (compare(:key bn) nkey) 0) (BNode. (:left bn) nkey nval (:right bn))
		(nil? bn) (BNode. nil nkey nval nil)
		(< (compare (:key bn) nkey) 0) (BNode. (:left bn) (:key bn) (:value bn) (nodeadd (:right bn) nkey nval))
		:else (BNode. (nodeadd (:left bn) nkey nval) (:key bn) (:value bn) (:right bn))))

(defn add "Add a key and value to the BST."
  [bst nu-key nu-val]
  (if (find bst nu-key) (BST. (nodeadd (:root bst) nu-key nu-val) (:size bst))
  	(BST. (nodeadd (:root bst) nu-key nu-val) (inc (:size bst)))))



;; # Delete
;;
;; Similiarly, we have two versions of delete.  Please use the predecessor node if
;; you need to delete a child with two elements.

(defn predecessor [t]
	(loop [x (:left t)]
		(if (nil? (:right x)) x
		(recur (:right x)))))
;;(cond (and (not (nil? (:right dnode))) (not (nil? (:right dnode))))
(defn deletenode [t vic]
	(cond (= (compare (:key t) vic) 0) 
			(cond (and (not (nil? (:left t))) (not (nil? (:right t))))
				(BNode. (deletenode (:left t) (:key (predecessor t))) (:key (predecessor t))(:value  (predecessor t)) (:right t))
			(and (nil? (:left t)) (not (nil? (:right t)))) (BNode. (:left (:right t)) (:key (:right t)) (:value (:right t)) (:right (:right t)))
			(and (nil? (:right t)) (not (nil? (:left t)))) (BNode. (:left (:left t)) (:key (:left t)) (:value (:left t)) (:right (:left t)))
			:else nil )
		(> (compare (:key t) vic) 0) (BNode. (deletenode (:left t) vic) (:key t) (:value t) (:right t))
		(< (compare (:key t) vic) 0) (BNode. (:left t) (:key t) (:value t) (deletenode (:right t) vic))
))
		
(defn delete [bst victim]
  (if (find bst victim) (BST. (deletenode (:root bst) victim) (dec (:size bst)))
  		bst))


(defn deletenodekey [t vic]
	(cond (= (compare(:value t) vic) 0)
		(cond (and (not (nil? (:left t))) (not (nil? (:right t))))
				(BNode. (deletenodekey (:left t) (:value (predecessor t))) (:key (predecessor t))(:value  (predecessor t)) (:right t))
			(and (nil? (:left t)) (not (nil? (:right t)))) (BNode. (:left (:right t)) (:key (:right t)) (:value (:right t)) (:right (:right t)))
			(and (nil? (:right t)) (not (nil? (:left t)))) (BNode. (:left (:left t)) (:key (:left t)) (:value (:left t)) (:right (:left t)))
			:else nil)
	(nil? t) nil
	:else (BNode. (deletenodekey (:left t) vic) (:key t) (:value t) (deletenodekey (:right t) vic))))


(defn delete-value [bst victim]
  	(if (find-key bst victim) (BST. (deletenodekey (:root bst) victim) (dec (:size bst)))
  		bst)
  )

;; # Map Tree
;;
;; This function takes a tree t and maps a function f over it.
;; If your tree is ((x 3 x) 5 ((x 7 x) 6 x)), then (map-tree t inc)
;; will return ((x 4 x) 6 ((x 8 x) 7 x))

(defn nodemaptree
  [t f] 
  (if (nil? t) nil
  	(BNode. (nodemaptree (:left t) f) (:key t) (f (:value t)) (nodemaptree (:right t) f) ))
 )

(defn map-tree
	[t f]
	(if (nil? (:root t)) nil
	(BST. (nodemaptree (:root t) f) (:size t))))
