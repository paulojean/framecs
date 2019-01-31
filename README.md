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

**Note:** one workspace in a collection of frames, i.e: we cycling through frames from one workspace you won't see other frames.

- `framecs/new-frame`: create a new frame e move to it;
- `framecs/delete-frame`: delete current frame;
- `framecs/go-to-next-frame`: go to the frame "on the right" (if the current frame is the laste one, go to the first frame;
- `framecs/go-to-previous-frame`: go to the frame "on the left" (if the current frame is the first one, go to the last frame;
- `framecs/new-workspace`: create a new workspace;
- `framecs/delete-current-workspace`: delete all frames from current workspace and remove it;
- `framecs/go-to-next-workspace`: go to the workspace "on the right" (if the current workspace is the laste one, go to the first workspace;
- `framecs/go-to-previous-workspace`: go to the workspace "on the left" (if the current workspace is the first one, go to the last workspace;

-------

## TODO
- [x] Enable more workspaces (currently all the frames are added to the same workspace)
- [ ] Persist workspace's active frame (so, when moving between workspaces you always arrive in the same frame that you left)
- [ ] Add view of current workspaces/frames
- [ ] Reorder workspaces/frames
- [ ] Rename workspaces/frames (making it easier to look)
- [ ] Persist/Restore session
  - [ ] Number/order of workspaces
  - [ ] window positions
  - [ ] buffers oppeneds
- [ ] Investigate
  - [x] Try it whithout `clomacs`(?), to avoid startup time/the need of a server running
  - [ ] Change frame's workspace
  - [ ] Enable per workspace (or frame) buffer list
