;;; framecs-ivy -- ivy support for framecs -*- lexical-binding: t; -*-

(require 'ivy)

(setq framecs/display-frames-fn (lambda (frames)
                                  (cdr (assoc (ivy-read "Frames from current workspace" frames) frames))))

(provide 'framecs-ivy)

;;; framecs-ivy.el ends here
