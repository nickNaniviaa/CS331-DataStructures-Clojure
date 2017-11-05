(ns heap-lab.t-core
  (:use midje.sweet)
  (:use [heap-lab.core]))

(facts "about numbers"
       (fact "one plus one is two."
             (+ 1 1)  =>  2)
       (fact "two plus one is three."
             (+ 2 1)  =>  3))

(facts "about `top`"
	(fact "Returns the top"
	(let [hone (-> (make-heap 7) (add 46) (add 63) (add 50) (add 92) (add 89) (add 86) (add 59))
		 htwo (make-heap  2)]
		(top hone) => 46
		(top htwo) => nil
		(top (add htwo 1)) => 1)))

(facts "about `delete`"
	(fact "Remove the root and decrements the size"
		(let [hone (-> (make-heap 7) (add 46) (add 63) (add 50) (add 92) (add 89) (add 86) (add 59))
		 htwo (make-heap  2)]
		 	(:size (delete hone)) => 6
		 	(:size (->  hone delete delete)) => 5
		 	(take 6 (:data (delete hone))) => (take 6 [50 63 59 92 89 86 nil])
		 	(take 5 (:data (-> hone delete delete))) => (take 5 [59 63 86 92 89 nil nil])
		 	(:size (->  htwo delete)) => 0
		 	(:size (->  htwo delete delete)) => 0
		 	(:data (delete htwo)) => [nil nil]
		 )))


(facts "about `add`"
	(fact "Adds one number to the Heap"
		(let [hone (-> (make-heap 7) (add 46) (add 63) (add 50) (add 92) (add 89) (add 86) (add 59))
		 htwo (make-heap  2)]
		 	(:size (add hone 2)) => 8
		 	(:size (add (add hone 3) 2)) => 9
		 	(:data (add hone 2)) => [2 46 50 63 89 86 59 92 nil nil nil nil nil nil]
		 	(:data (add hone 120)) => [46 63 50 92 89 86 59 120 nil nil nil nil nil nil]
		 	(:size (add htwo 1)) => 1
		 	(:size (add htwo nil)) => 1
		 	(:data (add htwo 1)) => [1 nil]
		 	(:data (add (add htwo 1) 2)) => [1 2]
		 	(:data (add (add (add htwo 10) 2) 3)) => [2 10 3 nil]
		 )))


