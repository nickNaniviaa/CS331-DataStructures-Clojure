(ns bst.t-core
  (:use midje.sweet)
  (:use [bst.core])
  (:import [bst.core BST] [bst.core BNode]))

(facts "about this lab"
  (fact "the student never started it, but this one always does"
        (+ 41 1)  => 42))


(facts "about `BST.`"
	(fact "creates a BST with root and size"
		(let [bst (BST. (BNode. nil 1 "A" nil) 1)]
			(:root bst) => (BNode. nil 1 "A" nil)
			(:size bst) => 1)))

(facts "about `BNode.`"
	(fact "creates a node"
		(let [node (BNode. nil 25 "A" nil)]
			(:left node) => nil
			(:key node) => 25
			(:value node) => "A"
			(:right node) => nil)))

(facts "about `make-node`"
	(fact "create a node"
		(make-node 22 "A") => (BNode. nil 22 "A" nil)
		(make-node 42 "B") => (BNode. nil 42 "B" nil)
		(make-node (make-node 99 "B") 100 "A" (make-node 101 "C")) => (BNode. (BNode. nil 99 "B" nil) 100 "A" (BNode. nil 101 "C" nil))
))

(facts "about `size`"
	(fact "returns the size"
		(let [bst (BST. (make-node 2 "A") 1)]
			(size bst) => 1)))

(facts "about `add`"
	(fact "it adds to the three properly"
		(let [bst (BST. (make-node 25 "A") 1)]	
			(add bst 50 "B") => (BST. (make-node nil 25 "A" (make-node 50 "B")) 2)
			(add bst 25 "B") => (BST. (make-node nil 25 "B" nil) 1)
			(add (add bst 50 "B") 18 "C") => (BST. (make-node (make-node 18 "C") 25 "A" (make-node 50 "B")) 3))))


(facts "about `find`"
	(fact "Finds the key and shows the value"
		(let [bst (BST. (make-node (make-node 18 "C") 25 "A" (make-node 50 "B")) 3)]
			(find bst 25) => "A"
			(find bst 18) => "C"
			(find bst 50) => "B"))
		(fact "returns nil if doesn't"
			(let [bst (BST. (make-node (make-node 18 "C") 25 "A" (make-node 50 "B")) 3)]
			(find bst 30) => nil)))

(facts "about `find-key`"
	(fact "Finds the key by the value"
		(let [bst (BST. (make-node (make-node 18 "C") 25 "A" (make-node 50 "B")) 3)]
			(find-key bst "B") => 50
			(find-key bst "C") => 18
			(find-key bst "A") => 25))
		(fact "returns nil if doesn't"
			(let [bst (BST. (make-node (make-node 18 "C") 25 "A" (make-node 50 "B")) 3)]
			(find-key bst "X") => nil)))



(facts "about `delete`"
	(fact "Delete  the node, by for the key"
		(let [bst (BST. (make-node (make-node (make-node 20 "D") 38 "C" (make-node (make-node 72 "G") 80 "E" nil)) 99 "F" (make-node (make-node 100 "N") 101 "A" (make-node 151 "B"))) 7)]
			(delete bst 20) => (BST. (make-node (make-node nil 38 "C" (make-node (make-node 72 "G") 80 "E" nil)) 99 "F" (make-node (make-node 100 "N") 101 "A" (make-node 151 "B"))) 6)
			(delete bst 101) => (BST. (make-node (make-node (make-node 20 "D") 38 "C" (make-node (make-node 72 "G") 80 "E" nil)) 99 "F" (make-node nil 100 "N" (make-node 151 "B"))) 6)
			(delete bst 99) => (BST. (make-node (make-node (make-node 20 "D") 38 "C" (make-node 72 "G")) 80 "E" (make-node (make-node 100 "N") 101 "A" (make-node 151 "B"))) 6)))
	(fact "it doesnt delete if doesnt exists"
		(let [bst (BST. (make-node (make-node 10 "B") 25 "A" (make-node 80 "C")) 3)]
			(delete bst 14) => (BST. (make-node (make-node 10 "B") 25 "A" (make-node 80 "C")) 3))))

(facts "about `delete-value`"
	(fact "Deletes the value properly"
		(let [bst (BST. (make-node (make-node 20 "N") 50 "C" (make-node nil 70 "I" (make-node 90 "K"))) 4)]
			(delete-value bst "N") => (BST. (make-node nil 50 "C" (make-node nil 70 "I" (make-node 90 "K"))) 3)
			(delete-value bst "I") => (BST. (make-node (make-node 20 "N") 50 "C" (make-node 90 "K")) 3)
			(delete-value bst "C") => (BST. (make-node nil 20 "N" (make-node nil 70 "I" (make-node 90 "K"))) 3)))
	(fact "Doesn't do anything if there is nothing"
		(let [bst (BST. (make-node (make-node 20 "C") 50 "F" (make-node 70 "A")) 3)]
			(delete-value bst "G") => (BST. (make-node (make-node 20 "C") 50 "F" (make-node 70 "A")) 3))))

(facts "about `map-tree`"
	(fact "It maps a function properly"
		(let [bst (BST. (make-node (make-node 20 2) 10 1 (make-node 30 3)) 3)]
			(map-tree bst inc) => (BST. (make-node (make-node 20 3) 10 2 (make-node 30 4)) 3)
			(map-tree bst dec) => (BST. (make-node (make-node 20 1) 10 0 (make-node 30 2)) 3))))

