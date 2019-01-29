(ns framecs.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(s/def ::non-empty-string (s/and  string? (comp not empty?)))
(s/def ::frame ::non-empty-string)
(s/def ::frames (s/coll-of ::frame))
(s/def ::workspace (s/tuple ::non-empty-string (s/coll-of ::frame)))
(s/def ::workspaces (s/coll-of ::workspace))

(def workspaces
  (atom []))

(defn update-workspaces! [update-fn]
  (swap! workspaces update-fn))

(defn uuid-str []
  (str (java.util.UUID/randomUUID) ))

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

(s/fdef frame-id->workspace
  :args (s/cat :workspaces ::workspaces :frame-id ::frame-id)
  :ret ::workspace)

(defn frame-id->workspace [workspaces frame-id]
  (->> workspaces
       (filter (fn [[_ frames]]
                 (some (partial = frame-id) frames)))
       first))

(defn item-id->index [filter-fn coll]
  (->> coll
       (keep-indexed (fn [idx x]
                       (when (filter-fn x)
                         idx)))
       first
       (#(or % (count coll)))))

(s/fdef add-frame-to-workspace
  :args (s/cat :workspace ::workspace :workspaces ::workspaces)
  :ret ::workspaces)

(defn update-workspace [workspace workspaces]
  (let [is-empty-workspace? (-> workspace second count (<= 0))
        index (item-id->index (workspace-filter-fn (first workspace))  workspaces)]
    (if is-empty-workspace?
      (->> workspaces
           (remove (comp (partial = (first workspace)) first))
           (into []))
      (assoc workspaces index workspace))))

(s/fdef add-frame-to-workspace
  :args (s/cat :workspace ::workspace :frame-id ::frame-id)
  :ret ::workspace)

(defn add-frame-to-workspace [workspace frame-id]
  (-> workspace
      second
      (conj frame-id)
      ((partial assoc workspace 1))))

(s/fdef remove-frame-from-workspace
  :args (s/cat :workspace ::workspace :frame-id ::frame-id)
  :ret ::workspace)

(defn remove-frame-from-workspace [workspace frame-id]
  (->> workspace
       second
       (remove (partial = frame-id))
       (into [])
       (assoc workspace 1)))

(defn next-neighbor [frames frame-id]
  (->> frames
       (drop-while (partial not= frame-id))
       second
       (#(or % (first frames)))))

(defn frames-for-neighbor [workspaces sort-fn frame-id]
  (some-> workspaces
          (frame-id->workspace frame-id)
          second
          sort-fn
          (next-neighbor frame-id)))

(defn get-next-frame [frame-id]
  (frames-for-neighbor @workspaces
                       identity
                       frame-id))

(defn get-previous-frame [frame-id]
  (frames-for-neighbor @workspaces
                       reverse
                       frame-id))

(defn new-workspace [id]
  [id []])

(defn new-frame! [current-frame-id new-frame-id]
  (update-workspaces! (fn [gs]
                        (-> gs
                            (frame-id->workspace current-frame-id)
                            (or (new-workspace (uuid-str)))
                            (add-frame-to-workspace new-frame-id)
                            (update-workspace gs)))))

(defn remove-frame! [frame-id]
  (update-workspaces! (fn [gs]
                        (some-> gs
                                (frame-id->workspace frame-id)
                                (remove-frame-from-workspace frame-id)
                                (update-workspace gs)))))
