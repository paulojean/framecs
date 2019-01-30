# Framecs 

Giving more power to `frames` in `emacs` (or, yet another `workspace` library for `emacs`)

## Installation


### Dependencies
`framecs` uses [`dash`](https://github.com/magnars/dash.el).

### Initialising

You can copy all `.el` files in this repository and add them to emacs path. E.g: you can put everything in `~/.emacs.d/lisp`, then:

```el
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'framecs)
(framecs/start-framecs)
```

## Main functions

- `framecs/new-frame`: create a new frame e move to it;
- `framecs/delete-frame`: delete current frame;
- `framecs/go-to-next-frame`: go to the frame "on the right" (if the current frame is the laste one, go to the first frame;
- `framecs/go-to-previous-frame`: go to the frame "on the left" (if the current frame is the first one, go to the last frame;

-------

## TODO
- [ ] Enable more workspaces (currently all the frames are added to the same workspace)
- [ ] Persist/Restore session
  - [ ] Number/order of workspaces
  - [ ] window positions
  - [ ] buffers oppeneds
- [ ] Investigate
  - [x] Try it whithout `clomacs`(?), to avoid startup time/the need of a server running
  - [ ] Enable per workspace (or frame) buffer list
