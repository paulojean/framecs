# Framecs 
----
Giving more power to `frames` in `emacs` (or, yet another `workspace` library for `emacs`)

## Installation


### Dependencies
`framecs` uses [`clomacs`](https://github.com/clojure-emacs/clomacs) and [`dash`](https://github.com/magnars/dash.el), so it assumes that you've installed them as well.

### Initialising

You can clone this repository and add it to emacs path. E.g: you can clone it to `~/.emacs.d/lisp`, then:

```el
(add-to-list 'load-path "~/.emacs.d/lisp/framecs")
(require 'framecs)
(framecs/start-framecs)
```

## Main functions

- `framecs/new-frame`: create a new frame e move to it;
- `framecs/delete-frame`: delete current frame;
- `framecs/go-to-next-frame`: go to the frame "on the right" (if the current frame is the laste one, go to the first frame;
- `framecs/go-to-previous-frame`: go to the frame "on the left" (if the current frame is the first one, go to the last frame;
