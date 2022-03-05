dockerfile-mode
===============
Known to work with Emacs 24 and later

If you just want to use it, you can get it via MELPA.

A Dockerfile mode for emacs

``` emacs-lisp
(add-to-list 'load-path "/your/path/to/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
```

Adds syntax highlighting as well as the ability to build the image
directly using `C-c C-b` from the buffer (`C-c M-b` to bypass docker build cache).

You can specify the image name in the file itself by adding a line like this
at the top of your Dockerfile.

``` emacs-lisp
## -*- dockerfile-image-name: "your-image-name-here" -*-
```

If you don't, you'll be prompted for an image name each time you build.
You may want to add the following to your emacs config:

``` emacs-lisp
(put 'dockerfile-image-name 'safe-local-variable #'stringp)
```

You can change the binary to use with
```emacs-lisp
(setq dockerfile-mode-command "docker")
```
