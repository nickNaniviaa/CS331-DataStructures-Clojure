(ns deque.t-core
  (:use midje.sweet)
  (:use [deque.core])
  (:import [deque.core Deque] ))

(facts "about this lab"
  (fact "the student never started it."
        (+ 10 2)  => 12))


(facts "about `make-deque`"
	(fact "It creates an empty queue"
	(let [dq (push-front (make-deque) 10)]
		(:front dq) => '(10)
		(:back dq) => '()
		(:size dq) => 1
)))

(facts "about `push-front`"
	(fact "It inserts one element in front of the list and increments the size"
		(let [dq (push-front (make-deque) 1)
			 dx (push-front dq 2)]
			 (deque-size dq) => 1
			 (deque-size dx) => 2
			 (:front dq) => '(1)
			 (:front dx) => '(2 1)
			 (:back dq) => '()
			 (:back dx) => '()
)))

(facts "about `push-back`"
	(fact "It inserts one element in back of the list and increments the size"
		(let [dq (push-front (make-deque) 1)
			 dx (push-back dq 2)]
			 (deque-size dq) => 1
			 (deque-size dx) => 2
			 (:front dq) => '(1)
			 (:front dx) => '(1)
			 (:back (push-back (push-back dx 3) 4)) => '(4 3 2)
			 (:size (push-back (push-back dx 3) 4)) => 4
			 (:back dx) => '(2)
)))

(facts "about `flip-front`"
	(fact "It flips the back list to the front, if necessary"
	 (let [dq (push-back (make-deque) 1)
			dx (reduce push-back dq '(2 3 4))]
			(:front dx) => '()
			(:front (flip-front dx)) => '(1 2 3 4)
			(:back (flip-front dx)) => '()
			(:size (flip-front dx)) => 4
			(:back dx) => '(4 3 2 1)
			(:front (flip-front(push-front dq 4))) => '(4)
			(:size (flip-front(push-front dq 4))) => 2
)))

(facts "about `flip-back`"
	(fact "It flips the front list to the back, if necessary"
	 (let [dq (push-front(make-deque) 1)
			dx (reduce push-front dq '(2 3 4))]
			(:back dx) => '()
			(:back (flip-back dx)) => '(1 2 3 4)
			(:front (flip-back dx)) => '()
			(:size (flip-back dx)) => 4
			(:front dx) => '(4 3 2 1)
			(:back (flip-front(push-back dq 4))) => '(4)
			(:size (flip-front(push-back dq 4))) => 2
)))

(facts "about `front`"
	(fact "It returns the first element of the front list, flips if necessary"
	 (let [dq (push-back (make-deque) 1)
			dx (reduce push-back dq '(2 3 4))]
			(front dx) => 1
			(front (flip-front dx)) => 1
			(front (push-front (make-deque) 4)) => 4
			(front (push-back (make-deque) 5)) => 5
			(front (make-deque)) => nil
)))

(facts "about `back`"
	(fact "It returns the first element of the back list, flips if necessary"
	 (let [dq (push-front (make-deque) 1)
			dx (reduce push-front dq '(2 3 4))]
			(back dx) => 1
			(back (flip-front dx)) => 1
			(back (push-front (make-deque) 4)) => 4
			(back (push-back (make-deque) 5)) => 5
			(back (make-deque)) => nil
)))

(facts "about `pop-front`"
	(fact "Pops/dequeues an element from the front of the deque."
	 (let [dq (push-back(make-deque) 1)
			dx (reduce push-front dq '(2 3 4))]
			(:front (pop-front dq)) => '()
			(:size (pop-front dq)) => 0
			(:front (pop-front dx)) => '(3 2)
			(:size (pop-front (flip-back dx))) => 3
			(:front (pop-front(push-front (make-deque) 4))) => '()
			(:size (pop-front(push-front (make-deque) 4))) => 0
			(:front (pop-front(push-back (make-deque) 4))) => '()
			(:size (pop-front(push-back (make-deque) 4))) => 0
			(:front (pop-front (make-deque))) => '()
			(:size (pop-front (make-deque))) => 0
)))

(facts "about `pop-back`"
	(fact "Pops/dequeues an element from the back of the deque."
	 (let [dq (push-front(make-deque) 1)
			dx (reduce push-back dq '(2 3 4))]
			(:back (pop-back dq)) => '()
			(:size (pop-back dq)) => 0
			(:back (pop-back dx)) => '(3 2)
			(:size (pop-back (flip-back dx))) => 3
			(:back (pop-back (push-front (make-deque) 4))) => '()
			(:size (pop-back (push-front (make-deque) 4))) => 0
			(:back (pop-back (push-back (make-deque) 4))) => '()
			(:size (pop-back (push-back (make-deque) 4))) => 0
			(:back (pop-back (make-deque))) => '()
			(:size (pop-back (make-deque))) => 0
)))