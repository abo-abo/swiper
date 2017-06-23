[![Build Status](https://travis-ci.org/abo-abo/swiper.svg?branch=master)](https://travis-ci.org/abo-abo/swiper)

***flexible, simple tools for minibuffer completion in Emacs***

This repository contains:

**Ivy**, a generic completion mechanism for Emacs.

**Counsel**, a collection of Ivy-enhanced versions of common Emacs
commands.

**Swiper**, an Ivy-enhanced alternative to isearch.

# Ivy

[![MELPA](http://melpa.org/packages/ivy-badge.svg)](http://melpa.org/#/ivy)

Ivy is a generic completion mechanism for Emacs. While it operates
similarly to other completion schemes such as `icomplete-mode`, Ivy
aims to be more efficient, smaller, simpler, and smoother to use yet
highly customizable.

To try Ivy, just call <kbd>M-x</kbd> `ivy-mode`. This will enable
generic Ivy completion, including specific completion for file and
buffer names.

### Installation

Install the `ivy` package from MELPA / GNU ELPA.

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
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
```

# Counsel

`ivy-mode` ensures that any Emacs command using
`completing-read-function` uses ivy for completion.

Counsel takes this further, providing versions of common Emacs
commands that are customised to make the best use of ivy. For example,
`counsel-find-file` has some additional keybindings. Pressing
<kbd>DEL</kbd> will move you to the parent directory.

# Swiper

[![MELPA](https://melpa.org/packages/swiper-badge.svg)](https://melpa.org/#/swiper)

Swiper is an alternative to isearch that uses ivy to show an overview
of all matches.

![swiper.png](http://oremacs.com/download/swiper.png)

A helm version of swiper is also available:
[swiper-helm](https://github.com/abo-abo/swiper-helm).

## Screenshots

![ivy-swiper-1.png](http://oremacs.com/download/ivy-swiper-1.png)

There's also a ten minute [video demo](https://www.youtube.com/watch?v=VvnJQpTFVDc).

# Frequently asked questions

Q: How do I enter an input that matches one of the candidates instead
   of this candidate? Example: create a file `bar` when a file
   `barricade` exists in the current directory.

A: Press <kbd>C-M-j</kbd>. Alternatively, you can make the prompt line selectable with `(setq ivy-use-selectable-prompt t)`.

# Contributing

## Copyright Assignment

These package are subject to the same [copyright assignment](http://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html) policy as Emacs itself, org-mode, CEDET and other packages in [GNU ELPA](http://elpa.gnu.org/packages/). Any [legally significant](http://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant) contributions can only be accepted after the author has completed their paperwork. Please see [the request form](http://git.savannah.gnu.org/cgit/gnulib.git/tree/doc/Copyright/request-assign.future) if you want to proceed.

The copyright assignment isn't a big deal, it just says that the copyright for your submitted changes to Emacs belongs to the FSF. This assignment works for all projects related to Emacs. To obtain it, you need to send one email, then send one letter (if you live in the US, it's digital), and wait for some time (in my case, I had to wait for one month).

## Style

The basic code style guide is to use `(setq indent-tabs-mode nil)`. It is provided for you in [.dir-locals.el](https://github.com/abo-abo/swiper/blob/master/.dir-locals.el), please obey it.

Before submitting the change, run `make compile` and `make test` to make sure that it doesn't introduce new compile warnings or test failures. Also run `make checkdoc` to see that your changes obey the documentation guidelines.

Use your own judgment for the commit messages, I recommend a verbose style using `magit-commit-add-log`.
