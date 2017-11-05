(ns heap-lab.core)

;; # Array Based Heaps
;;
;; Just in time for thanksgiving, a simple lab about Heaps!
;;
;; We will use vectors to handle this, with a top-level record
;; to keep track of the vector and the size.

(defrecord Heap [size data])

;; We will initialize this using the `make-heap` function.

(defn make-heap
  "Creates an empty heap.  Specify the size for the data vector.
The vector will be populated with `nil`."
  [capacity]
  (Heap. 0 (apply vector (repeat capacity nil))))

;; To access the elements of the heap, we will use these functions
;; `get`, `left`, `right`, and `parent`.

(defn heap-get
  "Return the value of the heap vector at the given index.
Throws an exception if the index is out of the range.
this is part of the implementation, not for public consumption."
  [heap loc]
  (cond (>= loc (count (:data heap)))
        (throw (Exception. (str "Get called with " loc " but last vector slot is " (dec (count (:data heap))))))

        :otherwise
        (get-in heap [:data loc])))

(defn heap-set
  "Set the value of the heap vector at the given index.
Throws an exception if the index is out of the range.
this is part of the implementation, not for public consumption."
  [heap loc value]
  (cond (>= loc (count (:data heap)))
        (throw (Exception. (str "Get called with " loc " but last vector slot is " (dec (count (:data heap))))))

        :otherwise
        (assoc-in heap [:data loc] value)))

(defn heap-left
  "Return the left index."
  [loc]
  (inc (* loc 2)))

(defn heap-right
  "Return the right index."
  [loc]
  (+ 2 (* loc 2)))

(defn heap-parent
  "Return the parent index."
  [loc]
  (int (/ (dec loc) 2)))

;; Now it's time for your code!  You need these three, but you are welcome to
;; write helper functions if you want (e.g., `percolate-down`.)  Do **not** write
;; `midje` tests for them, because they are not part of the spec.

(defn top
  "Return the top element of a heap.
If the heap has no elements, return `nil`."
  [heap]
  (if (= (:size heap) 0) nil
  (heap-get heap 0)))

(defn swap
  "Swaps two elements of the heap"
  [heap floc sloc]
  (-> (assoc-in heap [:data floc] (heap-get heap sloc)) (assoc-in [:data sloc] (heap-get heap floc))))


(defn expand
  [heap]
  (if (= (count (:data heap)) 0) (Heap. (inc (:size heap)) (apply vector (repeat 1 nil)))
  (Heap. (inc (:size heap)) (into (:data heap) (apply vector (repeat (:size heap) nil))))))


(defn heap-sort-delete
  [x ind]
  (let [datasize (dec (:size x))
        rc (heap-right ind)
        lc (heap-left ind)]
    (cond (< rc datasize) (cond (and (> (heap-get x ind) (heap-get x lc)) (> (heap-get x ind) (heap-get x rc)))
                                                                             (if (< (heap-get x lc) (heap-get x rc)) (heap-sort-delete (swap x ind lc) lc)
                                                                                                                      (heap-sort-delete (swap x ind rc) rc))
                                (> (heap-get x ind) (heap-get x lc)) (heap-sort-delete (swap x ind lc) lc)
                                (> (heap-get x ind) (heap-get x rc)) (heap-sort-delete (swap x ind rc) rc)
                                :else x) 
          (< lc datasize) (if (> (heap-get x ind) (heap-get x lc)) (heap-sort-delete (swap x ind lc) lc)
                          x)
    :else x)))


(defn delete
  "Deletes the first element of the heap.
Returns the new heap."
  [heap]
  (cond (nil? heap) nil
        (nil? (top heap)) heap
        (< (:size heap) 1) heap
        (= (:size heap) 1) (Heap. (dec (:size heap)) (:data (heap-set heap 0 nil)))
  :else (Heap. (dec (:size heap)) (:data (heap-sort-delete  (heap-set (heap-set heap 0 (heap-get heap (dec (:size heap)))) (dec (:size heap)) nil) 0)))
))

(defn heap-sort-add
  [x ind]
    (let [parent (heap-parent ind)]
      (cond (= ind 0) x
            (< (heap-get x ind) (heap-get x parent)) (heap-sort-add (swap x ind parent) parent)
        :else x )
  ))

(defn add
  "Adds a new element to the heap.
If the data vector is too small, we resize it."
  [heap data]
  (cond (nil? heap) nil
        (= (count (:data heap)) (:size heap)) (Heap. (inc (:size heap)) (:data (heap-sort-add (heap-set (expand heap) (:size heap) data) (:size  heap))))
  :else (Heap. (inc (:size heap)) (:data (heap-sort-add (heap-set heap (:size heap) data) (:size heap)))) ))

