[![MELPA](https://melpa.org/packages/tab-bar-buffers-badge.svg)](https://melpa.org/#/tab-bar-buffers)

# tab-bar-buffers

This package piggy-backs on *tab-bar-mode* to implement a simple buffer manager.  Instead of managing tabs it manages buffers.

<img width="1491" alt="tab-bar-buffers" src="https://user-images.githubusercontent.com/1565643/177202664-34c1335a-a58b-4a64-b329-51ee5dff5fd7.png">

## Installation

Add the following tor your init file.

``` emacs-lisp
(tab-bar-buffers-mode t)
```

Or enable `tab-bar-buffers-mode` from the Easy Customization form.

``` emacs-lisp
(customize-group 'tab-bar-buffers)
```


## Customization

`tab-bar-buffers-mode` has a few customization items of its own.

- *Uninteresting buffers* are buffer names that are not interesting
- *Uninteresting prefixes* are buffer name prefixes that are not interesting (default `(" " "*")`)
- *Interesting buffers* are buffers that are always interesting
 
If `tab-bar-buffers-mode` determines a buffer is uninteresting, it will **not** be shown in the tab bar.
 
Visible buffers are always shown.

Some of tab-bar-mode's customization options are also relevant for tab-bar-buffers.

- tab-bar-close-button-show
- tab-bar-position
- tab-bar-select-tab-modifiers
- tab-bar-tab-hints

If `tab-bar-tab-name-function` is `tab-bar-tab-name-truncated`, then `tab-bar-tab-name-truncated-max` and `tab-bar-tab-name-ellipsis` are honored.

### Faces

The face for `tab-bar-tab` is used for `current-buffer`.  `tab-bar-tab-inactive` is used for all other buffers.

### Bindings

Key and mouse bindings set by tab-bar-mode are preserved by advising some of tab-bar-mode's commands around their tab-bar-buffers equivalent, as defined by `tab-bar--define-keys`.

- tab-new -> tab-bar-buffers-new-buffer
- tab-bar-select-tab -> tab-bar-buffers-select-buffer
- tab-next -> tab-bar-buffers-next
- tab-previous -> tab-bar-buffers-prev
- tab-recent -> tab-bar-buffers-recent
- tab-last -> tab-bar-buffers-last
- tab-bar-close-tab -> tab-bar-buffers-close-buffer
