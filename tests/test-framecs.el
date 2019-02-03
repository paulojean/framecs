;;; test-framecs --- Tests for Framecs -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Framecs.

;;; Code:

(require 'framecs)
(require 'dash)

(setq workspace-1 '("workspace-1" ("w1-frame-1")))
(setq workspace-2 '("workspace-2" ("w2-frame-1" "w2-frame-2")))
(setq workspace-3 '("workspace-3" ("w3-frame-1" "w3-frame-2" "w3-frame-3")))
(setq workspaces `(,workspace-1 ,workspace-2 ,workspace-3))

(describe "Framecs id property"
  (it "Properly generate data structure"
    (expect (framecs/frame-properties "some-id")
            :to-equal
            '((framecs-id . "some-id")))))

(describe "select workspace frame"
  (it "return the first frame of a given workspace-id"
    (expect (framecs/select-workspace-frame (first workspace-2) workspaces)
            :to-equal
            (-> workspace-2 second first))))

(describe "get neighbors workspace"
  (describe "return the first frame's workspace"
    (it "on the rigth, when there's one and pass identity as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-1 (-take 2 workspaces) 'identity)
              :to-equal
              (-> workspace-2 second first)))
    (it "on the left, when there's one and pass reverse as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-2 (-take 2 workspaces) 'reverse)
              :to-equal
              (-> workspace-1 second first)))
    (it "in the last position, when on first workspace and pass 'reverse as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-1 workspaces 'reverse)
              :to-equal
              (-> workspace-3 second first)))
    (it "in the first position, when on last workspace and pass 'identity as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-3 workspaces 'identity)
              :to-equal
              (-> workspace-1 second first)))))

(describe "get neihbor frame"
  (describe "return frame on"
    (it "on the rigth, when there's one and pass 'identity as sort-fn"
      (expect (framecs/get-neighbor-frame workspaces 'identity (-> workspace-2 second first))
              :to-equal
              (-> workspace-2 second second)))
    (it "on the left, when there's one and pass 'reverse as sort-fn"
      (expect (framecs/get-neighbor-frame workspaces 'identity (-> workspace-2 second second))
              :to-equal
              (-> workspace-2 second first)))
    (it "in the last position, when on first frame and pass 'reverse as sort-fn"
      (expect (framecs/get-neighbor-frame workspaces 'reverse (-> workspace-2 second first))
              :to-equal
              (-> workspace-2 second last first)))
    (it "in the first position, when on last frame and pass 'identity as sort-fn"
      (expect (framecs/get-neighbor-frame workspaces 'identity (-> workspace-2 second last first))
              :to-equal
              (-> workspace-2 second first)))))

(describe "update workspaces"
  (it "remove workspace when the updated one does not have frames"
    (expect (framecs/update-workspaces workspaces (-replace-at 1 '() workspace-2))
            :to-equal
            (remove workspace-2 workspaces)))
  (it "remove workspace when the updated one does have frames"
    (let* ((update-workspace-2 (-replace-at 1 '("new-id") workspace-2))
           (expected-workspaces (-replace-at 1 update-workspace-2 workspaces)))
      (expect (framecs/update-workspaces workspaces (-replace-at 1 '("new-id") workspace-2))
              :to-equal
              expected-workspaces)))
  (it "add workspace in the last positions, when it does not exist yet"
    (let* ((workspace-4 '("new-workspace" ("w4-frame-1")))
           (expected-workspaces (-snoc workspaces workspace-4)))
      (expect (framecs/update-workspaces workspaces workspace-4)
              :to-equal
              expected-workspaces))))

(describe "remove frame from workspaces"
  (it "can remove frame"
    (let* ((expected-workspace (-replace-at 1 '("w2-frame-2") workspace-2)))
      (expect (framecs/remove-frame-from-workspaces workspace-2 "w2-frame-1")
              :to-equal
              expected-workspace)))
  (it "don't do anything when the given frame-id do no exist in the workspace"
    (expect (framecs/remove-frame-from-workspaces workspace-2 "do-not-exist")
            :to-equal
            workspace-2)))

(describe "add frame from workspace"
  (it "add frame to the last position in the workspace"
    (let* ((expected-workspace (-replace-at 1 '("w2-frame-1" "w2-frame-2" "frame") workspace-2)))
      (expect (framecs/add-frame-to-workspace workspace-2 "frame")
              :to-equal
              expected-workspace))))
