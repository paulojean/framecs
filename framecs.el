;;; framecs -- better frames for emacs

(require 'dash)

(defun framecs/frame-properties (index)
  `((framecs-index . ,index)))

(defun framecs/list-frames ()
  (->> (frame-list)
       (-filter 'framecs/is-framecs-frame)))

(defun framecs/has-index (index frame)
  (->> frame
       frame-parameters
       (assq 'framecs-index)
       cdr
       (equal index)))

(defun framecs/frame-by-index (frames name)
  (->> frames
       (-filter (lambda (f)
                  (framecs/has-index name f)))
       first))

(defun framecs/update-frame-properties (frame properties)
  (modify-frame-parameters frame properties))

(defun framecs/frame-index (frame)
  (->> frame
       frame-parameters
       (assq 'framecs-index)
       cdr))

(defun framecs/shift-to-left (frame)
  (->> frame
       framecs/frame-index
       ((lambda (index) (- index 1)))
       framecs/frame-properties
       (modify-frame-parameters frame)))

(defun framecs/delete-frame ()
  (let* ((frames (framecs/list-frames))
         (total-frames (length frames))
         (current-frame (selected-frame))
         (current-index (framecs/frame-index current-frame)))
    (->> (framecs/frame-properties -1)
         (modify-frame-parameters current-frame))
    (->> frames
         (-map 'frame-parameters)
         (-map (lambda (params) (assq 'framecs-index params)))
         (-map 'cdr)
         (-sort #'<)
         (-drop current-index)
         (-map (lambda (index) (framecs/frame-by-index frames index)))
         (-map 'framecs/shift-to-left))
    (delete-frame current-frame)))

(defun framecs/next-frame-index (frame op)
  (->> frame
       frame-parameters
       (assq 'framecs-index)
       cdr
       ((lambda (index)
          (funcall op index 1)))))

(defun framecs/next-frame-name (total-frames next-frame-index)
  (if (> next-frame-index total-frames)
    1
    next-frame-index))

(defun framecs/previous-frame-name (total-frames next-frame-index)
  (if (< next-frame-index 1)
    total-frames
    next-frame-index))

(defun framecs/go-to-neighbor (op frame-index-fn)
  (let* ((frames (framecs/list-frames))
         (total-frames (length frames))
         (next-frame-index (framecs/next-frame-index (selected-frame)
                                                      op)))
    (->> next-frame-index
         (funcall frame-index-fn total-frames)
         (framecs/frame-by-index frames)
         select-frame)))

(defun framecs/go-to-previous ()
  (interactive)
  (framecs/go-to-neighbor #'- #'framecs/previous-frame-name))

(defun framecs/go-to-next ()
  (interactive)
  (framecs/go-to-neighbor #'+ #'framecs/next-frame-name))

(defun framecs/is-framecs-frame (frame)
  (->> frame
       frame-parameters
       (assq 'framecs-index)))

(defun framecs/new-frame ()
  (interactive)
  (->> (framecs/list-frames)
       length
       (+ 1)
       framecs/frame-properties
       new-frame
       select-frame))

(defun framecs/start-framecs ()
  (interactive)
  (unless (framecs/list-frames)
    (->> (framecs/frame-properties 1)
         (modify-frame-parameters (selected-frame)))))

(provide 'framecs.el)

;;; framecs.el ends here
