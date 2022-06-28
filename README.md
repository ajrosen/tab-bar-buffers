# tab-bar-buffers

This package piggy-backs on *tab-bar-mode* to implement a simple
buffer manager.  Instead of managing tabs it manages buffers.

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

Some of tab-bar-mode's customization options are also relevant for
tab-bar-buffers.

- tab-bar-close-button-show
- tab-bar-position
- tab-bar-select-tab-modifiers
- tab-bar-tab-hints

If `tab-bar-tab-name-function` is `tab-bar-tab-name-truncated` then
`tab-bar-tab-name-truncated-max` and `tab-bar-tab-name-ellipsis`
are honored.

The face for `tab-bar-tab` is used for `current-buffer`.
`tab-bar-tab-inactive` is used for all other buffers.

Key and mouse bindings are preserved by advising some of
tab-bar-mode's commands around their tab-bar-buffers equivalent, as
defined by `tab-bar--define-keys`.

- tab-new
- tab-bar-select-tab
- tab-next
- tab-previous
- tab-recent
- tab-last
- tab-bar-close-tab
