;;; About

;; Define the command `ie-last-video-find-readme' that switches to
;; a buffer visiting the README of the last Inside Emacs video
;; being edited.

;;; Packages

(require 's)
(require 'dash)
(require 'f)

;;; Last Video

(defvar ie-last-video-main-dir "~/work/videos/inside-emacs/"
  "Path where the Inside Emacs videos are edited.")

(defun ie-last-video-name (inside-emacs-dir)
  "Return the directory of the last Inside Emacs video being edited.

INSIDE-EMACS-DIR is the directory of the video Inside Emacs."
  (let ((default-directory inside-emacs-dir))
    (-last-item (--filter (s-contains-p "inside-" it)
                          (s-lines (shell-command-to-string "ls"))))))

(defun ie-last-video-readme ()
  "Return the path of the README of the last Inside Emacs being edited."
  (f-join ie-last-video-main-dir
          (ie-last-video-name ie-last-video-main-dir)
          "README.org"))

(defun ie-last-video-find-readme ()
  "Switch to a buffer visiting file return by `ie-last-video-readme'."
  (interactive)
  (find-file (ie-last-video-readme)))

;;; Comments

(comment ; ie-last-video-find-readme
 (ie-last-video-name "~/work/videos/inside-emacs/")
 (ie-last-video-readme)
 (ie-last-video-find-readme)
 )

(comment ; s-contains-p, --filter, -last-item
 (s-contains-p "inside" "insiemacs")
 (--filter (s-contains-p "inside-" it)
           '("inside-emacs-1" "inside-emacs-2" "uie")) ; ("inside-emacs-1" "inside-emacs-2")
 (-last-item '(1 3 2)) ; 2
 )

;;; Footer

(provide 'ie-last-video)
