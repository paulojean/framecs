(ns framecs.core)

(def workspaces
  (atom []))

(defn workspace-filter-fn [workspace-id]
  (comp (partial = workspace-id)
        first))

(defn move-to-index [coll filter-fn id index]
  (let [item (filter filter-fn coll)
        sub-coll (remove filter-fn coll)]
    (-> sub-coll
        ((partial take index))
        (concat item)
        (concat (->> sub-coll (drop index))))))

(defn move-workspace-to-index [workspaces workspace-id index]
  (move-to-index workspaces workspace-filter-fn workspace-id index))

(defn workspace-exists? [workspace workspaces]
  (->> workspaces
       (filter (workspace-filter-fn workspace))
       first))

(defn item-id->index [id filter-fn coll]
  (->> coll
       (keep-indexed (fn [idx x]
                       (when ((filter-fn id) x)
                         idx)))
       first
       (#(or % (count coll)))))

(defn update-workspace [workspace workspaces]
  (let [is-empty-workspace? (-> workspace second count (<= 0))
        index (item-id->index (first workspace) workspace-filter-fn workspaces)]
    (if is-empty-workspace?
      (remove (comp (partial = (first workspace)) first) workspaces)
      (assoc workspaces index workspace))))

(defn add-frame-to-workspace [workspace frame-id]
  (-> workspace
      second
      (conj frame-id)
      ((partial assoc workspace 1))))

(defn remove-frame-from-workspace [workspace frame-id]
  (->> workspace
       second
       (remove (partial = frame-id))
       (assoc workspace 1)))

(defn next-neighbor [frame-id frames]
  (->> frames
       (drop-while (partial not= frame-id))
       second
       (#(or % (first frames)))))

(defn frames-for-neighbor [workspaces sort-fn workspace-id frame-id]
  (->> workspaces
       (filter (comp (partial = workspace-id) first))
       first
       second
       sort-fn
       (next-neighbor frame-id)))

(defn get-next-frame [workspace-id frame-id]
  (frames-for-neighbor @workspaces
                       identity
                       workspace-id
                       frame-id))

(defn get-previous-frame [workspace-id frame-id]
  (frames-for-neighbor @workspaces
                       reverse
                       workspace-id
                       frame-id))

(defn new-frame! [workspace-id frame-id]
  (swap! workspaces (fn [gs]
                  (-> workspace-id
                      (workspace-exists? gs)
                      (or [workspace-id []])
                      (add-frame-to-workspace frame-id)
                      (update-workspace gs)))))

(defn remove-frame! [workspace-id frame-id]
  (swap! workspaces (fn [gs]
                  (some-> workspace-id
                          (workspace-exists? gs)
                          (remove-frame-from-workspace frame-id)
                          (update-workspace gs)))))
