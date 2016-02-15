[![Build Status](https://travis-ci.org/abo-abo/swiper.svg?branch=master)](https://travis-ci.org/abo-abo/swiper) [![MELPA](https://melpa.org/packages/swiper-badge.svg)](https://melpa.org/#/swiper)

## Swiper

Package for GNU Emacs that shows an overview during regex searching.

![swiper.png](http://oremacs.com/download/swiper.png)

The package uses the `ivy` back end for the overview, see also
[swiper-helm](https://github.com/abo-abo/swiper-helm).

## Screenshots

![ivy-swiper-1.png](http://oremacs.com/download/ivy-swiper-1.png)

There's also a ten minute [video demo](https://www.youtube.com/watch?v=VvnJQpTFVDc).

## Ivy

Ivy is a generic completion mechanism for Emacs. While it operates
similarly to other completion schemes such as `icomplete-mode`, Ivy
aims to be more efficient, smaller, simpler, and smoother to use yet
highly customizable.

To try Ivy, just call <kbd>M-x</kbd> `ivy-mode`. This will enable
generic Ivy completion, including specific completion for file and
buffer names.

## Installation

Install the `swiper` package from MELPA / GNU ELPA.

## Documentation

### Manual
The manual is available as [HTML](http://oremacs.com/swiper/).

After installing from MELPA, the manual is also available through the `(ivy)` Info node.

The source file for the Info page is
[here](https://github.com/abo-abo/swiper/blob/master/doc/ivy.org).

### Wiki
Ivy and Swiper wiki is here: [the wiki](https://github.com/abo-abo/swiper/wiki).

### Small config example
```elisp
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
```
