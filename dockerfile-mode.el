;;; dockerfile-mode.el --- Major mode for editing Docker's Dockerfiles -*- lexical-binding: t -*-

;; Copyright (c) 2013 Spotify AB
;; Package-Requires: ((emacs "24"))
;; Homepage: https://github.com/spotify/dockerfile-mode
;; URL: https://github.com/spotify/dockerfile-mode
;; Version: 1.7
;; Keywords: docker languages processes tools
;;
;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;; use this file except in compliance with the License. You may obtain a copy of
;; the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations under
;; the License.

;;; Commentary:

;; Provides a major mode `dockerfile-mode' for use with the standard
;; `Dockerfile' file format.  Additional convenience functions allow
;; images to be built easily.

;;; Code:

(require 'sh-script)
(require 'rx)


(declare-function cygwin-convert-file-name-to-windows "cygw32.c" (file &optional absolute-p))

(defgroup dockerfile nil
  "Dockerfile editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "dockerfile-"
  :group 'languages)

(defcustom dockerfile-mode-command "docker"
  "Which binary to use to build images."
  :group 'dockerfile
  :type 'string)

(defcustom dockerfile-use-sudo nil
  "Runs docker builder command with sudo."
  :type 'boolean
  :group 'dockerfile)

(defcustom dockerfile-build-force-rm nil
  "Runs docker builder command with --force-rm switch."
  :type 'boolean
  :group 'dockerfile)

(defcustom dockerfile-build-pull nil
  "Runs docker builder command with --pull switch."
  :type 'boolean
  :group 'dockerfile)

(defcustom dockerfile-build-args nil
  "List of --build-arg to pass to docker build.

Each element of the list will be passed as a separate
 --build-arg to the docker build command."
  :type '(repeat string)
  :group 'dockerfile)

(defcustom dockerfile-build-progress "auto"
  "Type of --progress output (auto, plain, tty) of docker build."
  :group 'dockerfile
  :type 'string)

(defcustom dockerfile-build-extra-options nil
  "Extra command-line options to send to docker build.

Use this variable to add custom command-line switches not covered by
existing dockerfile-build-* variables.

Example:
(setq-default dockerfile-build-extra-options \"--network host\")"
  :group 'dockerfile
  :type 'string)

(defcustom dockerfile-use-buildkit nil
  "Use Docker buildkit for building images?

This is the new buildsystem for docker, and in time it will replace the old one
but for now it has to be explicitly enabled to work.
It is supported from docker 18.09"
  :type 'boolean)

(defcustom dockerfile-enable-auto-indent t
  "Toggles the auto indentation functionality."
  :type 'boolean)

(defcustom dockerfile-indent-offset (or standard-indent 2)
  "Dockerfile number of columns for margin-changing functions to indent."
  :type 'integer
  :safe #'integerp
  :group 'dockerfile)

(defface dockerfile-image-name
  '((t (:inherit (font-lock-type-face bold))))
  "Face to highlight the base image name after FROM instruction.")

(defface dockerfile-image-alias
  '((t (:inherit (font-lock-constant-face bold))))
  "Face to highlight the base image alias inf FROM ... AS <alias> construct.")

(defconst dockerfile--from-regex
  (rx "from " (group (+? nonl)) (or " " eol) (? "as " (group (1+ nonl)))))

(defvar dockerfile-font-lock-keywords
  `(,(cons (rx (or line-start "onbuild ")
               (group (or "from" "maintainer" "run" "cmd" "expose" "env" "arg"
                          "add" "copy" "entrypoint" "volume" "user" "workdir" "onbuild"
                          "label" "stopsignal" "shell" "healthcheck"))
               word-boundary)
           font-lock-keyword-face)
    (,dockerfile--from-regex
     (1 'dockerfile-image-name)
     (2 'dockerfile-image-alias nil t))
    ,@(sh-font-lock-keywords)
    ,@(sh-font-lock-keywords-2)
    ,@(sh-font-lock-keywords-1))
  "Default `font-lock-keywords' for `dockerfile mode'.")

(defvar dockerfile-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" #'dockerfile-build-buffer)
    (define-key map "\C-c\M-b" #'dockerfile-build-no-cache-buffer)
    (define-key map "\C-c\C-c" #'comment-region)
    (define-key map [menu-bar dockerfile-mode] (cons "Dockerfile" menu-map))
    (define-key menu-map [dfc]
      '(menu-item "Comment Region" comment-region
                  :help "Comment Region"))
    (define-key menu-map [dfb]
      '(menu-item "Build" dockerfile-build-buffer
                  :help "Send the Dockerfile to docker build"))
    (define-key menu-map [dfb]
      '(menu-item "Build without cache" dockerfile-build-no-cache-buffer
                  :help "Send the Dockerfile to docker build without cache"))
    map))

(defvar dockerfile-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?= "." table)
    table)
  "Syntax table for `dockerfile-mode'.")

(define-abbrev-table 'dockerfile-mode-abbrev-table nil
  "Abbrev table used while in `dockerfile-mode'.")

(unless dockerfile-mode-abbrev-table
  (define-abbrev-table 'dockerfile-mode-abbrev-table ()))

(defun dockerfile-indent-line-function ()
  "Indent lines in a Dockerfile.

Lines beginning with a keyword are ignored, and any others are
indented by one `dockerfile-indent-offset'. Functionality toggled
by `dockerfile-enable-auto-indent'."
  (when dockerfile-enable-auto-indent
    (unless (member (get-text-property (line-beginning-position) 'face)
             '(font-lock-comment-delimiter-face font-lock-keyword-face))
     (save-excursion
       (beginning-of-line)
       (unless (looking-at-p "\\s-*$") ; Ignore empty lines.
         (indent-line-to dockerfile-indent-offset))))))

(defun dockerfile-build-arg-string ()
  "Create a --build-arg string for each element in `dockerfile-build-args'."
  (mapconcat (lambda (arg) (concat "--build-arg="  (replace-regexp-in-string "\\\\=" "=" (shell-quote-argument arg))))
             dockerfile-build-args " "))

(defun dockerfile-standard-filename (file)
  "Convert the FILE name to OS standard.
If in Cygwin environment, uses Cygwin specific function to convert the
file name.  Otherwise, uses Emacs' standard conversion function."
  (if (fboundp 'cygwin-convert-file-name-to-windows)
      (replace-regexp-in-string
       (rx "\\") "\\\\" (cygwin-convert-file-name-to-windows file) t t)
    (convert-standard-filename file)))

(defun dockerfile-tag-string (image-name)
  "Return a --tag shell-quoted IMAGE-NAME string.

Returns an empty string if IMAGE-NAME is blank."
    (if (string= image-name "") "" (format "--tag %s " (shell-quote-argument image-name))))

(define-obsolete-variable-alias 'docker-image-name 'dockerfile-image-name "2017-10-22")

(defvar dockerfile-image-name nil
  "Name of the dockerfile currently being used.
This can be set in file or directory-local variables.")

(defvar dockerfile-image-name-history nil
  "History of image names read by `dockerfile-read-image-name'.")

(defun dockerfile-read-image-name ()
  "Read a docker image name."
  (read-string "Image name: " dockerfile-image-name 'dockerfile-image-name-history))


;;;###autoload
(defun dockerfile-build-buffer (image-name &optional no-cache)
  "Build an image called IMAGE-NAME based upon the buffer.

If the prefix arg NO-CACHE is set, don't cache the image.

The shell command used to build the image is:

    sudo docker build    \\
      --no-cache         \\
      --force-rm         \\
      --pull             \\
      --tag IMAGE-NAME   \\
      --build-args args  \\
      --progress type    \\
      -f filename        \\
      directory"

  (interactive (list (dockerfile-read-image-name) prefix-arg))
  (save-buffer)
    (compilation-start
        (format
            "%s%s%s build %s %s %s %s %s --progress %s %s -f %s %s"
            (if dockerfile-use-buildkit "DOCKER_BUILDKIT=1 " "")
            (if dockerfile-use-sudo "sudo " "")
            dockerfile-mode-command
            (if no-cache "--no-cache" "")
            (if dockerfile-build-force-rm "--force-rm " "")
            (if dockerfile-build-pull "--pull " "")
            (dockerfile-tag-string image-name)
            (dockerfile-build-arg-string)
            dockerfile-build-progress
            (or dockerfile-build-extra-options "")
            (shell-quote-argument (dockerfile-standard-filename
				   (or (file-remote-p (buffer-file-name) 'localname)
				       (buffer-file-name))))
            (shell-quote-argument (dockerfile-standard-filename
				   (or (file-remote-p default-directory 'localname)
				       default-directory))))
    nil
    (lambda (_) (format "*docker-build-output: %s *" image-name))))

;;;###autoload
(defun dockerfile-build-no-cache-buffer (image-name)
  "Build an image called IMAGE-NAME based upon the buffer without cache."
  (interactive (list (dockerfile-read-image-name)))
  (dockerfile-build-buffer image-name t))

(defun dockerfile--imenu-function ()
  "Find the previous headline from point.

Search for a FROM instruction.  If an alias is used this is
returned, otherwise the base image name is used."
  (when (re-search-backward dockerfile--from-regex nil t)
    (let ((data (match-data)))
      (when (match-string 2)
        ;; we drop the first match group because
        ;; imenu-generic-expression can only use one offset, so we
        ;; normalize to `1'.
        (set-match-data (list (nth 0 data) (nth 1 data) (nth 4 data) (nth 5 data))))
      t)))

;;;###autoload
(define-derived-mode dockerfile-mode prog-mode "Dockerfile"
  "A major mode to edit Dockerfiles.
\\{dockerfile-mode-map}"
  (set-syntax-table dockerfile-mode-syntax-table)
  (set (make-local-variable 'imenu-generic-expression)
       `(("Stage" dockerfile--imenu-function 1)))
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults)
       '(dockerfile-font-lock-keywords nil t))
  (setq local-abbrev-table dockerfile-mode-abbrev-table)
  (set (make-local-variable 'indent-line-function) #'dockerfile-indent-line-function))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (concat "[/\\]"
                           "\\(?:Containerfile\\|Dockerfile\\)"
                           "\\(?:\\.[^/\\]*\\)?\\'")
                   'dockerfile-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))

(provide 'dockerfile-mode)

;;; dockerfile-mode.el ends here
