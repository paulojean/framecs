;;; test-framecs --- Tests for Framecs -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;;; Commentary:

;; Tests for Framecs.

;;; Code:

(require 'framecs)
(require 'dash)

(defun create-frames-data (frames active-id)
  (let ((hash (make-hash-table :test 'equal)))
    (puthash :active-frame active-id hash)
    (puthash :frames frames hash)
    hash))

(setq workspace-1 (list "workspace-1" (create-frames-data '("w1-frame-1")
                                                          "w1-frame-1")))
(setq workspace-2 (list "workspace-2" (create-frames-data '("w2-frame-1" "w2-frame-2")
                                                          "w2-frame-1")))
(setq workspace-3 (list "workspace-3" (create-frames-data '("w3-frame-1" "w3-frame-2" "w3-frame-3")
                                                          "w3-frame-2")))

(setq workspaces (list workspace-1 workspace-2 workspace-3))

(defun workspace->frames (workspace)
  (->> workspace second (gethash :frames)))

(defun workspace->active-frame (workspace)
  (->> workspace second (gethash :active-frame)))

(defun equal-workspace? (w1 w2)
  (let ((id-1 (first w1))
        (id-2 (first w2))
        (frames-1 (workspace->frames w1))
        (frames-2 (workspace->frames w2))
        (frame-active-1 (workspace->active-frame w1))
        (frame-active-2 (workspace->active-frame w2)))
    (and (equal id-1 id-2)
         (equal frames-1 frames-2)
         (equal frame-active-1 frame-active-2))))

(defun equal-workspaces? (workspace-1 workspace-2)
  (and (equal (length workspace-1)
              (length workspace-2))
       (->> workspace-1
            (-map-indexed (lambda (index w)
                            (equal-workspace? w
                                              (nth index workspace-2))))
            (-all? (lambda (w) (equal w t))))))

(describe "Framecs id property"
  (it "Properly generate data structure"
    (expect (framecs/frame-properties "some-id")
            :to-equal
            '((framecs-id . "some-id")))))

(describe "get active frame"
  (it "return current active frame of a given workspace"
    (expect (framecs/get-active-frame workspace-3)
            :to-equal
            (->> workspace-3 workspace->frames second))))

(describe "get neighbors workspace"
  (describe "return the workspace"
    (it "on the rigth, when there's one and pass identity as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-1 (-take 2 workspaces) 'identity)
              :to-equal
              workspace-2))
    (it "on the left, when there's one and pass reverse as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-2 (-take 2 workspaces) 'reverse)
              :to-equal
              workspace-1))
    (it "in the last position, when on first workspace and pass 'reverse as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-1 workspaces 'reverse)
              :to-equal
              workspace-3))
    (it "in the first position, when on last workspace and pass 'identity as sort-fn"
      (expect (framecs/get-neighbor-workspace workspace-3 workspaces 'identity)
              :to-equal
              workspace-1))))

(describe "get neihbor frame"
  (describe "return frame on"
    (it "on the rigth, when there's one and pass 'identity as sort-fn"
      (expect (framecs/get-neighbor-frame workspace-2 'identity "w2-frame-1")
              :to-equal
              (->> workspace-2 second (gethash :frames) second)))
    (it "on the left, when there's one and pass 'reverse as sort-fn"
      (expect (framecs/get-neighbor-frame workspace-2 'identity "w2-frame-2")
              :to-equal
              (->> workspace-2 second (gethash :frames) first)))
    (it "in the last position, when on first frame and pass 'reverse as sort-fn"
      (expect (framecs/get-neighbor-frame workspace-2 'reverse "w2-frame-1")
              :to-equal
              (->> workspace-2 second (gethash :frames) last first)))
    (it "in the first position, when on last frame and pass 'identity as sort-fn"
      (expect (framecs/get-neighbor-frame workspace-2 'identity "w2-frame-2")
              :to-equal
              (->> workspace-2 second (gethash :frames) first)))))

(describe "update workspaces"
  (it "remove workspace when the updated one does not have frames"
    (let ((updated-workspaces (framecs/update-workspaces workspaces
                                                         (-replace-at 1
                                                                      (create-frames-data '() "")
                                                                      workspace-2))))
      (expect (equal-workspaces? (-remove-at 1 workspaces)
                                 updated-workspaces)
              :to-be-truthy)))
  (it "replace workspace when the updated one does have frames"
    (let* ((update-workspace-2 (-replace-at 1
                                            (create-frames-data '("new-id") "new-id")
                                            workspace-2))
           (expected-workspaces (-replace-at 1 update-workspace-2 workspaces)))
      (expect (equal-workspaces? (framecs/update-workspaces workspaces
                                                            update-workspace-2)
                                 expected-workspaces)
              :to-be-truthy)))
  (it "add workspace in the last positions, when it does not exist yet"
    (let* ((workspace-4 (list "new-workspace" (create-frames-data '("w4-frame-1")
                                                                  "w4-frame-1")))
           (expected-workspaces (-snoc workspaces workspace-4)))
      (expect (equal-workspaces? (framecs/update-workspaces workspaces workspace-4)
                                 expected-workspaces)
              :to-be-truthy))))

(describe "remove frame from workspaces"
  (it "can remove frame"
    (let* ((expected-workspace (-replace-at 1 (create-frames-data '("w2-frame-2")
                                                                  "w2-frame-2")
                                            workspace-2)))
      (expect (equal-workspace? (framecs/remove-frame-from-workspace workspace-2 "w2-frame-1")
                                expected-workspace)
              :to-be-truthy)))
  (it "don't do anything when the given frame-id does no exist in the workspace"
    (expect (equal-workspace? (framecs/remove-frame-from-workspace workspace-2 "do-not-exist")
                              workspace-2)
            :to-be-truthy)))

(describe "add frame from workspace"
  (it "add frame to the last position in the workspace"
    (let* ((expected-workspace (-replace-at 1 (create-frames-data '("w2-frame-1" "w2-frame-2" "frame")
                                                                  "frame")
                                            workspace-2)))
      (expect (equal-workspace? (framecs/add-frame-to-workspace workspace-2 "frame")
                                expected-workspace)
              :to-be-truthy))))
