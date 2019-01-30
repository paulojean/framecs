;;; framecs -- better frames for emacs

(require 'dash)

(defvar *workspaces* '())

(defun framecs/gen-uuid ()
  (->> (shell-command-to-string "uuidgen")
       (replace-regexp-in-string "\n" "")))

(defun framecs/workspace->frames (workspace)
  (-> workspace
      rest
      first))

(defun framecs/frame-id->workspace (frame-id workspaces)
  (->> workspaces
       (-filter (lambda (workspace)
		  (->> workspace
                       framecs/workspace->frames
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

(defun framecs/frame-by-id (frame-id)
  (->> (framecs/list-frames)
       (-filter (lambda (frame)
		  (-> frame framecs/frame-id (equal frame-id))))
       first))

(defun framecs/go-to-neighbor-frame (workspaces sort-fn frame-id)
  (let ((frames-ids (->> workspaces
			 (framecs/frame-id->workspace frame-id)
                         framecs/workspace->frames
			 (funcall sort-fn))))
    (-> (-drop-while (lambda (frame) (-> frame (equal frame-id) not))
		     frames-ids)
        rest
	first
	(or (first frames-ids))
	framecs/frame-by-id
	select-frame)))

(defun framecs/frame-properties (frame-id)
  "Format frame id"
  `((framecs-id . ,frame-id)))

(defun framecs/replace-frames-in-workspace (workspace new-frames)
  (-replace-at 1 new-frames workspace))

(defun framecs/remove-workspave-from-workspaces (workspaces workspace-updated)
  (-filter (lambda (w)
             (not (equal (first w)
                         (first workspace-udpated))))))

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
  (if (-> workspace-updated framecs/workspace->frames length (> 0))
    (framecs/upsert-workspace workspaces workspace-updated)
    (framecs/remove-frame-from-workspaces workspaces workspace-updated)))

(defun framecs/remove-frame-from-workspaces (workspace frame-id)
  (->> workspace
       framecs/workspace->frames
       (remove frame-id)
       (framecs/replace-frames-in-workspace workspace)))

(defun framecs/add-frame-to-workspace (workspace frame-id)
  (-> workspace
      framecs/workspace->frames
      (-snoc frame-id)
      ((lambda (frames)
         (framecs/replace-frames-in-workspace workspace frames)))))

(defun framecs/delete-framecs-frame (current-frame)
  (let* ((frame-id (framecs/frame-id current-frame))
         (workspace (framecs/frame-id->workspace frame-id *workspaces*)))
    (delete-frame current-frame)
    (setq *workspaces*
          (framecs/update-workspaces *workspaces*
                               (framecs/remove-frame-from-workspaces workspace frame-id)))))

(defun framecs/update-frame-properties! (frame properties)
  (modify-frame-parameters frame properties))

;;;#autoload
(defun framecs/go-to-previous-frame ()
  (interactive)
  (->> (selected-frame)
       framecs/frame-id
       (framecs/go-to-neighbor-frame *workspaces* #'reverse)
       select-frame))

;;;#autoload
(defun framecs/go-to-next-frame ()
  (interactive)
  (->> (selected-frame)
       framecs/frame-id
       (framecs/go-to-neighbor-frame *workspaces* #'identity)
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
                               (framecs/add-frame-to-workspace workspace new-frame-id)))))

;;;#autoload
(defun framecs/start-framecs ()
  (interactive)
  (unless (framecs/list-frames)
    (let ((frame-id (framecs/gen-uuid))
	  (group-id (framecs/gen-uuid)))
      (->> (framecs/frame-properties frame-id)
           (framecs/update-frame-properties! (selected-frame)))
      (setq *workspaces* (framecs/update-workspaces *workspaces*
                                                    (framecs/add-frame-to-workspace `(,group-id ()) frame-id))))))

(provide 'framecs)

;;; framecs.el ends here
