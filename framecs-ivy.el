;;; framecs-ivy -- ivy support for framecs -*- lexical-binding: t; -*-

(require 'ivy)

(setq framecs/display-frames-fn (lambda (display-message frames)
                                  (cdr (assoc (ivy-read display-message frames) frames))))

(provide 'framecs-ivy)

;;; framecs-ivy.el ends here
