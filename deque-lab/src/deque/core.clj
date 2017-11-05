(ns deque.core)

(defrecord Deque [front back size])

;; # Your Work

(defn make-deque
  "Create an empty deque."
  []
  (Deque. '() '() 0))

(defn deque-size
  "Return the size of a deque."
  [dq]
  (:size dq))

(defn push-front
  "Adds an element to the front of the deque."
  [dq elt]
  (let [{:keys [front back size]} dq]
    (Deque. (cons elt front) back (inc size))))

(defn push-back
  "Adds an element to the back fo the deque."
  [dq elt]
  (let [{:keys [front back size]} dq]
    (Deque. front (cons elt back) (inc size))))

(defn flip-front
  "Flip the back list to the front list, if necessary."
  [dq]
  (let [{:keys [front back size]} dq]
    (if (empty? front) (Deque. (reverse back) '() size)
    dq ))
)

(defn flip-back
  "Flip the front list to the back list, if necessary."
  [dq]
  (let [{:keys [front back size]} dq]
    (if (empty? back) (Deque. '() (reverse front) size)
    dq ))
)

(defn front
  "Return the front element of the deque.  May cause a flip."
  [dq]
  (if (and (empty? (:front dq)) (< 0 (:size dq))) (-> dq flip-front front)
    (first (:front dq)))
)


(defn back
  "Return the back element of the deque.  May cause a flip."
  [dq]
  (if (and (empty? (:back dq)) (< 0 (:size dq))) (-> dq flip-back back)
    (first (:back dq)))
)

(defn pop-front
  "Pops/dequeues an element from the front of the deque."
  [dq]
  (let [{:keys [front back size]} dq]
  (if (> size 0) (if (empty? front) (-> dq flip-front pop-front)
                                  (Deque. (rest front) back (dec size)))
                 dq ))
)

(defn pop-back
  "Pops/dequeues an element from tohe back of the deque."
  [dq]
  (let [{:keys [front back size]} dq]
  (if (> size 0) (if (empty? back) (-> dq flip-back pop-back)
                                  (Deque. front (rest back) (dec size)))
                 dq))
)
;;Nice Try!