(Given "I start framecs"
  'framecs/start-framecs)

(Then "Current frame is a framecs frame"
  (lambda ()
    (let ((frames (framecs/list-frames)))
      (should (equal (selected-frame)
                     (first frames))))))

(And "I create a new frame"
  (lambda ()
    (let ((frames (framecs/list-frames)))
      (framecs/new-frame))))

(Then "There are two frames"
  (lambda ()
    (should (equal 2
                   (length (framecs/list-frames))))))

(And "Current frame is the newly created"
  (lambda ()
    (should (equal (selected-frame)
                   (second (framecs/list-frames))))))

(When "I call \"\\([^\"]+\\)\""
  (lambda (fn)
    (funcall (intern fn))))

(Then "I am in frame number \"\\([^\"]+\\)\"$"
  (lambda (position)
    (let*  ((frames (-> (selected-frame)
                        framecs/frame-id
                        (framecs/frame-id->workspace framecs/*workspaces*)
                        second
                        ((lambda (data) (gethash :frames data)))))
            (nth-frame (-> (nth (string-to-number position) frames)
                           (framecs/frame-by-id (framecs/list-frames)))))
      (should (equal nth-frame
                     (selected-frame))))))

(And "I can create and delete a new frame"
  (lambda ()
    (framecs/new-frame)
    (let ((frames (-> (selected-frame)
                      framecs/frame-id
                      (framecs/frame-id->workspace framecs/*workspaces*)
                      second
                      ((lambda (data) (gethash :frames data))))))
      (framecs/go-to-previous-frame)
      (should (equal (selected-frame)
                     (framecs/frame-by-id (second frames) (framecs/list-frames))))
      (framecs/delete-frame)
      (should (equal nil
                     (framecs/frame-by-id (second frames) (framecs/list-frames))))
      (should (equal 2
                     (length (framecs/list-frames)))))))

(Given "I create a new workspace"
  'framecs/new-workspace)

(Then "I am in workspace number \"\\([^\"]+\\)\"$"
  (lambda (position)
    (let  ((workspace (-> (selected-frame)
                          framecs/frame-id
                          (framecs/frame-id->workspace framecs/*workspaces*)
                          first))
           (nth-workspace (->> framecs/*workspaces*
                               (nth (string-to-number position))
                               first)))
      (should (equal workspace
                     nth-workspace)))) )

(Then "There are \"\\([^\"]+\\)\" workspaces and \"\\([^\"]+\\)\" frames"
  (lambda (workspaces frames)
    (should (equal (string-to-number workspaces)
                   (length framecs/*workspaces*)))
    (should (equal (string-to-number frames)
                   (length (framecs/list-frames))))))

(Given "I delete current workspace"
  'framecs/delete-current-workspace)
