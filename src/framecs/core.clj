(ns framecs.core)

(def groups
  (atom []))

(defn group-filter-fn [group-id]
  (comp (partial = group-id)
        first))

(defn move-to-index [coll filter-fn id index]
  (let [item (filter filter-fn coll)
        sub-coll (remove filter-fn coll)]
    (-> sub-coll
        ((partial take index))
        (concat item)
        (concat (->> sub-coll (drop index))))))

(defn move-group-to-index [groups group-id index]
  (move-to-index groups group-filter-fn group-id index))

(defn group-exists? [group groups]
  (->> groups
       (filter (group-filter-fn group))
       first))

(defn item-id->index [id filter-fn coll]
  (->> coll
       (keep-indexed (fn [idx x]
                       (when ((filter-fn id) x)
                         idx)))
       first
       (#(or % (count coll)))))

(defn update-group [group groups]
  (let [is-empty-group? (-> group second count (<= 0))
        index (item-id->index (first group) group-filter-fn groups)]
    (if is-empty-group?
      (remove (comp (partial = (first group)) first) groups)
      (assoc groups index group))))

(defn add-frame-to-group [group frame-id]
  (-> group
      second
      (conj frame-id)
      ((partial assoc group 1))))

(defn remove-frame-from-group [group frame-id]
  (->> group
       second
       (remove (partial = frame-id))
       (assoc group 1)))

(defn next-neighbor [frame-id frames]
  (->> frames
       (drop-while (partial not= frame-id))
       second
       (#(or % (first frames)))))

(defn frames-for-neighbor [groups sort-fn group-id frame-id]
  (->> groups
       (filter (comp (partial = group-id) first))
       first
       second
       sort-fn
       (next-neighbor frame-id)))

(defn get-next-frame [group-id frame-id]
  (frames-for-neighbor @groups
                       identity
                       group-id
                       frame-id))

(defn get-previous-frame [group-id frame-id]
  (frames-for-neighbor @groups
                       reverse
                       group-id
                       frame-id))

(defn new-frame! [group-id frame-id]
  (swap! groups (fn [gs]
                  (-> group-id
                      (group-exists? gs)
                      (or [group-id []])
                      (add-frame-to-group frame-id)
                      (update-group gs)))))

(defn remove-frame! [group-id frame-id]
  (swap! groups (fn [gs]
                  (some-> group-id
                          (group-exists? gs)
                          (remove-frame-from-group frame-id)
                          (update-group gs)))))
