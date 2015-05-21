[![Build Status](https://travis-ci.org/abo-abo/swiper.svg?branch=master)](https://travis-ci.org/abo-abo/swiper)

## Swiper

Package for GNU Emacs that gives you an overview as you search for a regex

![swiper.png](http://oremacs.com/download/swiper.png)

The package uses the `ivy` back end for the overview, see also
[swiper-helm](https://github.com/abo-abo/swiper-helm).

## Screenshots

![ivy-swiper-1.png](http://oremacs.com/download/ivy-swiper-1.png)

There's also a ten minute [video demo](https://www.youtube.com/watch?v=VvnJQpTFVDc).

## Ivy

Ivy is a generic completion method for Emacs, similar to
`icomplete-mode`. It aims to be more efficient, more simple, and more
pleasant to use than the alternatives. It's also highly customizable
and very small.

To try it, just call <kbd>M-x</kbd> `ivy-mode`, and all generic
completion, including file and buffer names, will be done with Ivy.

## Installation

You can install the package from MELPA / GNU ELPA.
Here is some typical configuration:

```elisp
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key [f6] 'ivy-resume)
```

## Issues

Recently, the `ivy` package that provided `ivy.el` was removed from MELPA.  Now, the `swiper` package provides `ivy.el`. You should remove the outdated `ivy` package from your system.
