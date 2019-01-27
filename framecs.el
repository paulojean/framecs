;;; framecs -- better frames for emacs

(require 'dash)

(require 'clomacs)

(clomacs-defun framecs-new-frame!
               framecs.core/new-frame!
               :lib-name "framecs"
               :namespace framecs.core
               :doc "Add frame to list")
(clomacs-defun framecs-remove-frame!
               framecs.core/remove-frame!
               :lib-name "framecs"
               :namespace framecs.core
               :doc "Remove frame from list")
(clomacs-defun framecs-get-next-frame
               framecs.core/get-next-frame
               :lib-name "framecs"
               :namespace framecs.core
               :doc "Get next frame id")
(clomacs-defun framecs-get-previous-frame
               framecs.core/get-previous-frame
               :lib-name "framecs"
               :namespace framecs.core
               :doc "Get previous frame id")

(defun framecs/gen-uuid ()
  (->> (shell-command-to-string "uuidgen")
       (replace-regexp-in-string "\n" "")))

(defun framecs/update-frame-properties! (frame properties)
  (modify-frame-parameters frame properties))

(defun framecs/frame-name-property (name)
  `((name . ,name)))

(defun framecs/buffer-directory-name (buffer)
  (let ((dir-structure (-> buffer
                           expand-file-name
                           (split-string "/"))))
    (nth (-> dir-structure
             length
             (- 2))
         dir-structure)))

(defun framecs/frame-properties (group-id frame-id)
  "Format group and frame id"
  `((framecs-group . ,group-id)
    (framecs-id . ,frame-id)))

(defun framecs/is-framecs-frame (frame)
  (->> frame
       frame-parameters
       (assq 'framecs-id)))

(defun framecs/list-frames ()
  (->> (frame-list)
       (-filter 'framecs/is-framecs-frame)))

(defun framecs/frame-group (frame)
  (->> frame
       frame-parameters
       (assq 'framecs-group)
       cdr))

(defun framecs/frame-id (frame)
  (->> frame
       frame-parameters
       (assq 'framecs-id)
       cdr))
 
(defun framecs/has-id (id frame)
  (->> frame
       framecs/frame-id
       (equal id)))

(defun framecs/frame-by-id (frames id)
  (->> frames
       (-filter (lambda (f)
                  (framecs/has-id id f)))
       first))

(defun framecs/go-to-neighbor (target-frame-fn)
  (let* ((frames (framecs/list-frames))
         (current-frame (selected-frame))
         (group-id (framecs/frame-group current-frame))
         (frame-id (framecs/frame-id current-frame))
         (target-frame-id (funcall target-frame-fn group-id frame-id)))
    (->> target-frame-id
         (framecs/frame-by-id frames)
         select-frame)))

(defun framecs/go-to-previous ()
  (interactive)
  (framecs/go-to-neighbor #'framecs-get-previous-frame))

(defun framecs/go-to-next ()
  (interactive)
  (framecs/go-to-neighbor #'framecs-get-next-frame))

(defun framecs/delete-framecs-frame (current-frame)
  (let ((group-id (framecs/frame-group current-frame))
        (frame-id (framecs/frame-id current-frame)))
    (delete-frame current-frame)
    (framecs-remove-frame! group-id frame-id)))

(defun framecs/delete-non-framecs-frame (frame)
  (delete-frame frame))

;;;#autoload
(defun framecs/update-frame-name ()
  (interactive)
  (let* ((default-name (framecs/buffer-directory-name buffer-file-name))
         (name (read-string (format "Rename frame to (default %s):" default-name)
                            nil
                            nil
                            default-name)))
    (->> name
         framecs/frame-name-property
         (framecs/update-frame-properties! (selected-frame)))))

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
  (let ((group-id (framecs/frame-group (selected-frame)))
        (frame-id (framecs/gen-uuid)))
    (-> (framecs/frame-properties group-id frame-id)
        new-frame
        select-frame)
    (framecs-new-frame! group-id frame-id)))

;;;#autoload
(defun framecs/start-framecs ()
  (interactive)
  (unless (framecs/list-frames)
    (let ((group-id (framecs/gen-uuid))
          (frame-id (framecs/gen-uuid)))
      (->> (framecs/frame-properties group-id frame-id)
           (framecs/update-frame-properties! (selected-frame)))
      (framecs-new-frame! group-id frame-id))))

(provide 'framecs)

;;; framecs.el ends here
