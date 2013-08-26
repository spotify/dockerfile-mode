;;; Copyright (c) 2013 Spotify AB
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License. You may obtain a copy of
;;; the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;;; License for the specific language governing permissions and limitations under
;;; the License.

(require 'sh-script)

(defvar docker-image-name nil)

(defgroup dockerfile nil
  "dockerfile code editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "dockerfile-"
  :group 'languages)

(defcustom dockerfile-mode-hook nil
  "*Hook called by `dockerfile-mode'."
  :type 'hook
  :group 'dockerfile)

(defcustom dockerfile-use-sudo nil
  "Runs docker builder command with sudo.")

(defvar dockerfile-mode-syntax-table nil
  "Syntax table used while in `dockerfile-mode'.")
(setq dockerfile-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?\# "<" dockerfile-mode-syntax-table)
(modify-syntax-entry ?\n ">" dockerfile-mode-syntax-table)

(defvar dockerfile-font-lock-keywords
  `(
    ("#.*$" . font-lock-comment-face)
    ("^\\([Ff][Rr][Oo][Mm]\\|[mM][aA][iI][nN][tT][aA][iI][nN][eE][rR]\\|[rR][uU][nN]\\|[eE][nN][vV]\\|[cC][mM][dD]\\|[eE][xX][pP][oO][sS][eE]\\|[iI][nN][sS][eE][rR][tT]\\|[cC][oO][pP][yY]\\|[eE][nN][tT][rR][yY][pP][oO][iI][nN][tT]\\|[vV][oO][lL][uU][mM][eE]\\|[aA][dD][dD]\\)\\b" . font-lock-keyword-face)
    ,@(sh-font-lock-keywords)
    ,@(sh-font-lock-keywords-2)
    ,@(sh-font-lock-keywords-1)
    )
  "Default font-lock-keywords for `dockerfile mode'.")

(defvar dockerfile-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'dockerfile-build-buffer)
    (define-key map "\C-c\C-z" 'dockerfile-test-function)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map [menu-bar dockerfile-mode] (cons "Dockerfile" menu-map))
    (define-key menu-map [dfc]
      '(menu-item "Comment Region" comment-region
		  :help "Comment Region"))
    (define-key menu-map [dfb]
      '(menu-item "Build" dockerfile-build-buffer
		  :help "Send the Dockerfile to docker build"))
    map))

(defvar dockerfile-mode-abbrev-table nil
  "Abbrev table used while in `dockerfile-mode'.")

(unless dockerfile-mode-abbrev-table
  (define-abbrev-table 'dockerfile-mode-abbrev-table ()))

(defun dockerfile-build-buffer (image-name)
  "Build an image based upon the buffer"
  (interactive
   (if (null docker-image-name)
      (list (read-string "image-name:" nil nil))
     (list docker-image-name)))
  (save-buffer)
  (if (stringp image-name)
      (shell-command
       (concat (if dockerfile-use-sudo "sudo " "") "docker build -t " image-name " " (file-name-directory (buffer-file-name)) "&")
       "*docker-build-output*")
    (print "docker-image-name must be a string, consider surrounding it with double quotes")))

;;;###autoload
(defun dockerfile-mode ()
  "A major mode to edit Dockerfiles.
\\{dockerfile-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dockerfile-mode-map)

  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq local-abbrev-table dockerfile-mode-abbrev-table)

  (make-local-variable	'font-lock-defaults)
  (setq major-mode 'dockerfile-mode
	mode-name "dockerfile"
	font-lock-defaults '(dockerfile-font-lock-keywords nil))
  (set-syntax-table dockerfile-mode-syntax-table)
  (run-mode-hooks 'dockerfile-mode-hook))

(provide 'dockerfile-mode)

