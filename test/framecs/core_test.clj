(ns framecs.core-test
  (:require [clojure.test :refer :all]
            [framecs.core :as core]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(def test-workspaces (atom []))

(defn reset-workspaces [test-fn]
  (reset! test-workspaces [])
  (test-fn))    

(use-fixtures :each reset-workspaces)

(defn all-items-are-distincts? [coll]
  (= (count coll)
     ((comp count distinct) coll)))

(defn generate-workspace-with-frames
  ([] (generate-workspace-with-frames (partial < 0)))
  ([frames-count-pred]
   (gen/generate (gen/such-that #(and ((comp frames-count-pred count second) %)
                                      (all-items-are-distincts? %))
                                (s/gen  ::core/workspace))) ))
(defn generate-workspaces
  ([] (generate-workspaces 1))
  ([min-size] 
   (gen/generate (gen/such-that #(and (-> % count (>= min-size))
                                      (all-items-are-distincts? (map first %)))
                                (s/gen (s/coll-of ::core/workspace))))))

(defn pick-one [coll]
  (->> coll
       count
       rand-int
       (nth coll)))

(deftest core-test
  (with-redefs [core/update-workspaces! (fn [f]
                                          (swap! test-workspaces f))]
    (testing "add-frame-to-workspace"
      (let [workspace (gen/generate (s/gen  ::core/workspace))
            frame-id (gen/generate (s/gen  ::core/frame))
            workspace-updated (core/add-frame-to-workspace workspace frame-id)]
        (is (= 1
               (->> workspace-updated second (filter (partial = frame-id)) count)))
        (is (= frame-id
               (-> workspace-updated second last)))))


    (testing "remove-frame-from-workspace"
      (let [workspace (generate-workspace-with-frames)
            frame-id (pick-one (second workspace))
            workspace-updated (core/remove-frame-from-workspace workspace frame-id)]
        (is (= 1
               (->> workspace second (filter (partial = frame-id)) count)))
        (is (= 0
               (->> workspace-updated second (filter (partial = frame-id)) count)))))

    (testing "update-workspace"
      (let [workspaces (generate-workspaces 2)
            workspace (pick-one workspaces)]
        (is (= 1
               (->> workspaces (filter (partial = workspace)) count))
            "workspace is in the list")
        (let  [workspace-updated (assoc workspace 1 (gen/generate (s/gen ::core/frame)))
               workspaces-updated (core/update-workspace workspace-updated workspaces)]
          (is (= 1
                 (->> workspaces-updated (filter (partial = workspace-updated)) count))
              "when workspace has frames, return workspaces with given workspace udated, in the same place"))
        (let  [workspace-updated (assoc workspace 1 [])
               workspaces-updated (core/update-workspace workspace-updated workspaces)]
          (is (= 0
                 (->> workspaces-updated (filter (partial = workspace-updated)) count))
              "when workspace does not have frames, remove it form workspaces list"))))

    (testing "frame-id->workspace"
      (let [workspaces (generate-workspaces 2)
            workspace (pick-one workspaces)
            frame-id ((comp pick-one second) workspace)]
        (is (= workspace
               (core/frame-id->workspace workspaces frame-id))
            "return the right workspace, given a frame-id")))

    (testing "item-id->index"
      (let [coll (range (rand-int 100))
            index (rand-int (count coll))]
        (is (= index
               (core/item-id->index (partial = index) coll)))))

    (testing "get-next-or-previous-frame"
      (let [workspaces (generate-workspaces 2)
            workspace (pick-one workspaces)]
        (reset! test-workspaces workspaces)
        (let [frame-id ((comp last second) workspace)]
          (is (= ((comp first second) workspace))
              (core/get-next-frame frame-id)))
        (let [frame-id ((comp first second) workspace)]
          (is (= ((comp last second) workspace))
              (core/get-previous-frame frame-id)))))

    (testing "new-frame!"
      (let [_ (reset! test-workspaces [])
            frame-id (gen/generate (s/gen ::core/frame))
            workspaces (core/new-frame! frame-id frame-id)]
        (is (= 1
               (count @test-workspaces))
            "workspaces previously empty, stays with one workspace"))

      (let [frame-id (gen/generate (s/gen  ::core/frame))
            _ (reset! test-workspaces (generate-workspaces))
            _ (core/new-frame! frame-id frame-id)]
        (is (= [frame-id]
               ((comp second last) @test-workspaces))
            "previously empty workspace, stays with one frame"))

      (let [frame-id (gen/generate (s/gen  ::core/frame))
            workspaces (generate-workspaces)
            _ (reset! test-workspaces workspaces)
            current-frame-id ((comp first second pick-one) @test-workspaces)
            _ (core/new-frame! current-frame-id frame-id)]
        (is (= frame-id
               (-> @test-workspaces
                   (core/frame-id->workspace current-frame-id)
                   second
                   last))
            "frame-id is added as the last item in the workspace")))

    (testing "remove-frame!"
      (let [_ (reset! test-workspaces (generate-workspaces))
            workspace (pick-one @test-workspaces)
            frame-id ((comp pick-one second) workspace)]
        (core/remove-frame! frame-id)
        (is (= nil
               (-> @test-workspaces
                   (core/frame-id->workspace frame-id)))
            "frame is not there after being removed"))

      (let [_ (reset! test-workspaces (generate-workspaces))
            frame-id (gen/generate (s/gen ::core/frame))
            _ (core/new-frame! frame-id frame-id)
            [workspace-id _] (core/frame-id->workspace @test-workspaces frame-id)
            ]
        (is (= workspace-id
               (->> @test-workspaces
                    (map first)
                    (filter (partial = workspace-id))
                    first))
            "workspace is there after removal")
        (core/remove-frame! frame-id)
        (is (= nil
               (->> @test-workspaces
                    (map first)
                    (filter (partial = workspace-id))
                    first))
            "workspace isn't there after removal")))))

(run-tests)
