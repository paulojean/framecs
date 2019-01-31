;;; framecs -- better frames for emacs

(require 'dash)

(defvar *workspaces* '())

(defun framecs/gen-uuid ()
  (->> (shell-command-to-string "uuidgen")
       (replace-regexp-in-string "\n" "")))

(defun framecs/frame-id->workspace (frame-id workspaces)
  (->> workspaces
       (-filter (lambda (workspace)
                  (->> workspace
                       second
                       (member frame-id))))
       first))

(defun framecs/frame-id (frame)
  (->> frame
       frame-parameters
       (assq 'framecs-id)
       rest))

(defun framecs/is-framecs-frame (frame)
  (framecs/frame-id frame))

(defun framecs/list-frames ()
  (->> (frame-list)
       (-filter 'framecs/is-framecs-frame)))

(defun framecs/select-workspace-frame (workspace-id workspaces)
  (->> workspaces
       (-find (lambda (w) (-> w first (equal workspace-id))))
       second
       first))

(defun framecs/get-neighbor-workspace (workspace workspaces sort-fn)
  (let ((workspace-id (first workspace))
        (workspace-ids (->> workspaces
                            (-map 'first)
                            (funcall sort-fn))))
    (-> (-drop-while (lambda (workspace) (-> workspace (equal workspace-id) not))
                     workspace-ids)
        second
        (or (first workspace-ids))
        (framecs/select-workspace-frame workspaces))))

(defun framecs/frame-by-id (frame-id)
  (->> (framecs/list-frames)
       (-filter (lambda (frame)
                  (-> frame framecs/frame-id (equal frame-id))))
       first))

(defun framecs/get-neighbor-frame (workspaces sort-fn frame-id)
  (let ((frames-ids (->> workspaces
                         (framecs/frame-id->workspace frame-id)
                         second
                         (funcall sort-fn))))
    (-> (-drop-while (lambda (frame) (-> frame (equal frame-id) not))
                     frames-ids)
        second
        (or (first frames-ids)))))

(defun framecs/frame-properties (frame-id)
  "Format frame id"
  `((framecs-id . ,frame-id)))

(defun framecs/replace-frames-in-workspace (workspace new-frames)
  (-replace-at 1 new-frames workspace))

(defun framecs/remove-workspace-from-workspaces (workspaces workspace-updated)
  (-filter (lambda (w)
             (not (equal (first w)
                         (first workspace-updated))))
           workspaces))

(defun framecs/workspace-exists? (workspaces workspace)
  (->> workspaces
       (-map (lambda (w) (first w)))
       (member (first workspace))))

(defun framecs/upsert-workspace (workspaces workspace-updated)
  (let ((workspace-id (first workspace-updated)))
    (if (framecs/workspace-exists? workspaces workspace-updated)
      (-map (lambda (w)
              (if (equal workspace-id (first w))
                  workspace-updated
                w))
            workspaces)
      (-concat workspaces `(,workspace-updated)))))

(defun framecs/update-workspaces (workspaces workspace-updated)
  (if (-> workspace-updated second length (> 0))
    (framecs/upsert-workspace workspaces workspace-updated)
    (framecs/remove-workspace-from-workspaces  workspaces workspace-updated)))

(defun framecs/remove-frame-from-workspaces (workspace frame-id)
  (->> workspace
       second
       (remove frame-id)
       (framecs/replace-frames-in-workspace workspace)))

(defun framecs/add-frame-to-workspace (workspace frame-id)
  (-> workspace
      second
      (-snoc frame-id)
      ((lambda (frames)
         (framecs/replace-frames-in-workspace workspace frames)))))

(defun framecs/delete-framecs-frame (current-frame)
  (let* ((frame-id (framecs/frame-id current-frame))
         (workspace (framecs/frame-id->workspace frame-id *workspaces*)))
    (delete-frame current-frame)
    (setq *workspaces*
          (framecs/update-workspaces *workspaces*
                                     (framecs/remove-frame-from-workspaces workspace
                                                                           frame-id)))))

(defun framecs/update-frame-properties! (frame properties)
  (modify-frame-parameters frame properties))

;;;#autoload
(defun framecs/go-to-previous-workspace ()
  (interactive)
  (-> (selected-frame)
      framecs/frame-id
      (framecs/frame-id->workspace *workspaces*)
      (framecs/get-neighbor-workspace *workspaces* #'reverse)
      framecs/frame-by-id
      select-frame))

;;;#autoload
(defun framecs/go-to-next-workspace ()
  (interactive)
  (-> (selected-frame)
      framecs/frame-id
      (framecs/frame-id->workspace *workspaces*)
      (framecs/get-neighbor-workspace *workspaces* #'identity)
      framecs/frame-by-id
      select-frame))

;;;#autoload
(defun framecs/delete-current-workspace ()
  (interactive)
  (let* ((frame-id (framecs/frame-id (selected-frame)))
         (workspace (framecs/frame-id->workspace frame-id *workspaces*)))
    (dolist (f-id (second workspace))
      (framecs/delete-framecs-frame (framecs/frame-by-id f-id)))
    (framecs/remove-workspace-from-workspaces *workspaces* workspace)))

;;;#autoload
(defun framecs/new-workspace ()
  (interactive)
  (let ((frame-id (framecs/gen-uuid))
        (workspace-id (framecs/gen-uuid)))
    (->> frame-id
         framecs/frame-properties
         new-frame
         select-frame)
    (setq *workspaces*
          (framecs/update-workspaces *workspaces*
                                     (framecs/add-frame-to-workspace `(,workspace-id ())
                                                                     frame-id)))))

;;;#autoload
(defun framecs/go-to-previous-frame ()
  (interactive)
  (->> (selected-frame)
       framecs/frame-id
       (framecs/get-neighbor-frame *workspaces* #'reverse)
       framecs/frame-by-id
       select-frame))

;;;#autoload
(defun framecs/go-to-next-frame ()
  (interactive)
  (->> (selected-frame)
       framecs/frame-id
       (framecs/get-neighbor-frame *workspaces* #'identity)
       framecs/frame-by-id
       select-frame))

;;;#autoload
(defun framecs/delete-frame ()
  (interactive)
  (let ((current-frame (selected-frame)))
    (if (framecs/is-framecs-frame current-frame)
      (framecs/delete-framecs-frame current-frame)
      (framecs/delete-non-framecs-frame))))

;;;#autoload
(defun framecs/new-frame ()
  (interactive)
  (let* ((current-frame-id (framecs/frame-id (selected-frame)))
         (workspace (framecs/frame-id->workspace current-frame-id *workspaces*))
         (new-frame-id (framecs/gen-uuid)))
    (->> new-frame-id
         framecs/frame-properties
         new-frame
         select-frame)
    (setq *workspaces*
          (framecs/update-workspaces *workspaces*
                               (framecs/add-frame-to-workspace workspace
                                                               new-frame-id)))))

;;;#autoload
(defun framecs/start-framecs ()
  (interactive)
  (unless (framecs/list-frames)
    (let ((frame-id (framecs/gen-uuid))
          (workspace-id (framecs/gen-uuid)))
      (->> (framecs/frame-properties frame-id)
           (framecs/update-frame-properties! (selected-frame)))
      (setq *workspaces* (framecs/update-workspaces *workspaces*
                                                    (framecs/add-frame-to-workspace `(,workspace-id ())
                                                                                    frame-id))))))

(provide 'framecs)

;;; framecs.el ends here
