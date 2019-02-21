;;; framecs -- better frames for emacs -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl)

(defvar *workspaces* '())

(defvar framecs/display-frames-fn nil
  "Handler function to display frames list")

(defvar framecs/display-full-path-when-showing-buffer t
  "Display full path, next to buffer name")

(defun framecs/apply-first (f coll)
  `(,(funcall f (first coll)) . ,(cdr coll)))

(defun framecs/gen-uuid ()
  (->> (shell-command-to-string "uuidgen")
       (replace-regexp-in-string "\n" "")))

(defun framecs/frame-id->workspace (frame-id workspaces)
  (--find (->> it
               second
               (gethash :frames)
               (member frame-id))
          workspaces))

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

(defun framecs/get-active-frame (workspace)
  (->> workspace
       second
       (gethash :active-frame)))

(defun framecs/get-neighbor-workspace (workspace workspaces sort-fn)
  (let ((workspaces-sorted (funcall sort-fn workspaces)))
    (-> (--drop-while (not (equal (first it)
                                  (first workspace)))
                      workspaces-sorted)
        second
        (or (first workspaces-sorted)))))

(defun framecs/frame-by-id (frame-id frames)
  (--find (-> it framecs/frame-id (equal frame-id))
          frames))

(defun framecs/get-neighbor-frame (workspace sort-fn frame-id)
  (let ((frames-ids (->> workspace
                         second
                         (gethash :frames)
                         (funcall sort-fn))))
    (-> (--drop-while (not (equal it
                                  frame-id))
                      frames-ids)
        second
        (or (first frames-ids)))))

(defun framecs/frame-properties (frame-id)
  "Format frame id"
  `((framecs-id . ,frame-id)))

(defun framecs/replace-frames-in-workspace (workspace new-frames)
  (-replace-at 1 new-frames workspace))

(defun framecs/remove-workspace-from-workspaces (workspaces workspace-updated)
  (--filter (not (equal (first it)
                        (first workspace-updated)))
            workspaces))

(defun framecs/workspace-exists? (workspaces workspace)
  (--find (equal (first it)
                 (first workspace))
          workspaces))

(defun framecs/upsert-workspace (workspaces workspace-updated)
  (let ((workspace-id (first workspace-updated)))
    (if (framecs/workspace-exists? workspaces workspace-updated)
        (-map (lambda (w)
                (if (equal workspace-id (first w))
                    workspace-updated
                  w))
              workspaces)
      (-snoc workspaces workspace-updated))))

(defun framecs/update-workspaces (workspaces workspace-updated)
  (if (->> workspace-updated second (gethash :frames) length (< 0))
      (framecs/upsert-workspace workspaces workspace-updated)
    (framecs/remove-workspace-from-workspaces  workspaces workspace-updated)))

(defun framecs/next-active-frame (frames current-active)
  (-> (--find-index (equal it current-active)
                    frames)
      (+ 1)
      (nth frames)
      (or (first frames))))

(defun framecs/remove-frame-from-workspace (workspace frame-id)
  (let* ((workspace-data (second workspace))
         (new-data (copy-hash-table workspace-data))
         (frames (gethash :frames new-data))
         (next-active-frame (framecs/next-active-frame frames
                                                       (gethash :active-frame new-data))))
    (if (-> frame-id (member frames) not)
        workspace
      (progn
        (puthash :active-frame next-active-frame new-data)
        (puthash :frames (remove frame-id frames) new-data)
        (framecs/replace-frames-in-workspace workspace new-data)))))

(defun framecs/add-frame-to-workspace (workspace frame-id)
  (let* ((workspace-data (second workspace))
         (new-data (copy-hash-table workspace-data)) (frames (gethash :frames new-data)))
    (puthash :active-frame frame-id new-data)
    (puthash :frames (-snoc frames frame-id) new-data)
    (framecs/replace-frames-in-workspace workspace new-data)))

(defun framecs/delete-framecs-frame (current-frame)
  (let* ((frame-id (framecs/frame-id current-frame))
         (workspace (framecs/frame-id->workspace frame-id *workspaces*)))
    (delete-frame current-frame)
    (setq *workspaces*
          (framecs/update-workspaces *workspaces*
                                     (framecs/remove-frame-from-workspace workspace
                                                                          frame-id)))))

(defun framecs/update-frame-properties! (frame properties)
  (modify-frame-parameters frame properties))

(defun framecs/format-buffer-display-name (buffer)
  (if framecs/display-full-path-when-showing-buffer
      (format "%s => %s"
              (buffer-name buffer)
              (buffer-file-name buffer))
    (buffer-name buffer)))

(defun framecs/select-frame-id-from-workspace (frames workspace)
  (->> workspace
       second
       (gethash :frames)
       (--map `(,(framecs/frame-by-id it frames) . ,it))
       (--map (framecs/apply-first 'frame-selected-window it))
       (--map (framecs/apply-first 'window-buffer it))
       (--map (framecs/apply-first 'framecs/format-buffer-display-name it))
       ((lambda (m)
          (funcall framecs/display-frames-fn "Frames from current workspace" m)))))

;;;#autoload
(defun framecs/rename-workspace (name)
  (interactive "sWorkspace name:")
  (let* ((workspace (-> (selected-frame)
                        framecs/frame-id
                        (framecs/frame-id->workspace *workspaces*)))
         (workspace-data (copy-hash-table (second workspace))))
    (puthash :name name workspace-data)
    (setq *workspaces* (framecs/update-workspaces *workspaces*
                                                  (framecs/replace-frames-in-workspace workspace workspace-data)))))

;;;autoload
(defun framecs/select-workspace ()
  (interactive)
  (->> *workspaces*
       (--map-indexed `(,(gethash :name (second it) (int-to-string it-index))
                        .
                        ,(framecs/get-active-frame it)))
       (funcall framecs/display-frames-fn "Workspaces")
       ((lambda (frame-id)
          (framecs/frame-by-id frame-id (framecs/list-frames))))
       select-frame-set-input-focus))

;;;autoload
(defun framecs/select-frame-from-current-workspace ()
  (interactive)
  (let* ((frames (framecs/list-frames))
         (workspace (-> (selected-frame)
                        framecs/frame-id
                        (framecs/frame-id->workspace *workspaces*)))
         (frame-id (framecs/select-frame-id-from-workspace frames workspace)))
    (-> frame-id
        (framecs/frame-by-id frames)
        select-frame-set-input-focus)
    (setq *workspaces* (framecs/update-workspaces *workspaces*
                                                  (framecs/update-active-frame workspace frame-id)))))

;;;#autoload
(defun framecs/go-to-previous-workspace ()
  (interactive)
  (-> (selected-frame)
      framecs/frame-id
      (framecs/frame-id->workspace *workspaces*)
      (framecs/get-neighbor-workspace *workspaces* #'reverse)
      framecs/get-active-frame
      (framecs/frame-by-id (framecs/list-frames))
      select-frame-set-input-focus))

;;;#autoload
(defun framecs/go-to-next-workspace ()
  (interactive)
  (-> (selected-frame)
      framecs/frame-id
      (framecs/frame-id->workspace *workspaces*)
      (framecs/get-neighbor-workspace *workspaces* #'identity)
      framecs/get-active-frame
      (framecs/frame-by-id (framecs/list-frames))
      select-frame-set-input-focus))

;;;#autoload
(defun framecs/delete-current-workspace ()
  (interactive)
  (let* ((frames (framecs/list-frames))
         (frame-id (framecs/frame-id (selected-frame)))
         (workspace (framecs/frame-id->workspace frame-id *workspaces*)))
    (dolist (f-id (->> workspace second (gethash :frames)))
      (framecs/delete-framecs-frame (framecs/frame-by-id f-id frames)))
    (framecs/remove-workspace-from-workspaces *workspaces* workspace)))

;;;#autoload
(defun framecs/new-workspace ()
  (interactive)
  (let ((frame-id (framecs/gen-uuid))
        (workspace-id (framecs/gen-uuid)))
    (->> frame-id
         framecs/frame-properties
         new-frame
         select-frame-set-input-focus)
    (setq *workspaces*
          (framecs/update-workspaces *workspaces*
                                     (framecs/add-frame-to-workspace (list workspace-id (make-hash-table :test 'equal))
                                                                     frame-id)))))

(defun framecs/update-active-frame (workspace frame-id)
  (let ((new-data (copy-hash-table (second workspace))))
    (puthash :active-frame frame-id new-data)
    (framecs/replace-frames-in-workspace workspace new-data)))

;;;#autoload
(defun framecs/go-to-previous-frame ()
  (interactive)
  (let* ((current-frame-id (framecs/frame-id (selected-frame)))
         (workspace (framecs/frame-id->workspace current-frame-id *workspaces*))
         (next-frame-id (framecs/get-neighbor-frame workspace #'reverse current-frame-id)))
    (-> next-frame-id
        (framecs/frame-by-id (framecs/list-frames))
        select-frame-set-input-focus)
    (setq *workspaces* (framecs/update-workspaces *workspaces*
                                                  (framecs/update-active-frame workspace next-frame-id)))))

;;;#autoload
(defun framecs/go-to-next-frame ()
  (interactive)
  (let* ((current-frame-id (framecs/frame-id (selected-frame)))
         (workspace (framecs/frame-id->workspace current-frame-id *workspaces*))
         (next-frame-id (framecs/get-neighbor-frame workspace #'identity current-frame-id)))
    (-> next-frame-id
        (framecs/frame-by-id (framecs/list-frames))
        select-frame-set-input-focus)
    (setq *workspaces* (framecs/update-workspaces *workspaces*
                                                  (framecs/update-active-frame workspace next-frame-id)))))

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
         select-frame-set-input-focus)
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
                                                    (framecs/add-frame-to-workspace (list workspace-id (make-hash-table :test 'equal))
                                                                                    frame-id))))))

(provide 'framecs)

;;; framecs.el ends here
