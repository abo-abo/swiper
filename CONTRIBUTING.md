# Reporting issues

Before reporting a bug, please take a look at the [FAQ](https://github.com/abo-abo/swiper/blob/master/README.md), as well as the [manual](http://oremacs.com/swiper/) and the [wiki](https://github.com/abo-abo/swiper/wiki). Please also make sure that there is not yet an existing issue.

In order to fix it, we need to be able to reproduce the bug you encountered as closely as possible, so please describe the problem as detailed as you can. It is important that we can rule out interference with other emacs packages or customisations, therefore use `emacs -Q` to bypass your init file or use the shell code below to sandbox your emacs environment even further.

```
$ emacs --version
GNU Emacs 25.3.1
$ cd /tmp
$ git clone https://github.com/abo-abo/swiper.git
$ cd swiper
$ git log -1 --oneline
3cd7637 ivy-overlay.el: Expect window to be 1 row taller
$ HOME="$PWD" emacs -L .
```

You can use `eval-expression` to load swiper/ivy and counsel and continue from there.

```
(require 'swiper)
(ivy-mode 1)

(require 'counsel)
(counsel-mode 1)
```

# Contributing code

The basic code style guide is to use `(setq indent-tabs-mode nil)`. It is provided for you in [.dir-locals.el](https://github.com/abo-abo/swiper/blob/master/.dir-locals.el), please obey it.

Before submitting the change, run `make compile` and `make test` to make sure that it doesn't introduce new compile warnings or test failures. Also run `make checkdoc` to see that your changes obey the documentation guidelines.

Use your own judgment for the commit messages, I recommend a verbose style using `magit-commit-add-log`.

# Copyright Assignment

These package are subject to the same [copyright assignment](http://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html) policy as Emacs itself, org-mode, CEDET and other packages in [GNU ELPA](http://elpa.gnu.org/packages/). Any [legally significant](http://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant) contributions can only be accepted after the author has completed their paperwork. Please see [the request form](http://git.savannah.gnu.org/cgit/gnulib.git/tree/doc/Copyright/request-assign.future) if you want to proceed.

The copyright assignment isn't a big deal, it just says that the copyright for your submitted changes to Emacs belongs to the FSF. This assignment works for all projects related to Emacs. To obtain it, you need to send one email, then send one letter (if you live in the US, it's digital), and wait for some time (in my case, I had to wait for one month).
