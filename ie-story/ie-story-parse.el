;;; About

;; A boring part but really important when making Inside Emacs videos
;; is to transform the story (written in a specific formated text
;; file) to beautiful footage.  So far it involves 3 steps:
;;   1) parse the story (specific formated text),
;;   2) generate "svg" files (the story) from the specific
;;      formated text,
;;   3) generate "kdenlive" files with the previous "svg" files.
;;
;; The part 1) is treated in this file, and the parts 2) and 3)
;; are treated in the file ./ie-story-generate.el
;;
;; In this file, we define the following parsing functions:
;;   `ie-story-parse-goto-next-description',
;;   `ie-story-parse-description',
;;   `ie-story-parse-scene-title',
;;   `ie-story-parse-next-scene',
;;   `ie-story-parse-goto-next-scene',
;;   `ie-story-parse-scenes'.
;;
;; The stories of Inside Emacs videos are written as described
;; bellow (and also in the file ./ie-story-template.org)
;;
;; * scenes
;; ** scene 0: intro
;; # description
;; a description splited
;; into two lines
;;
;; ** scene 1: First Scene
;; # description
;; A one line paragraph
;;
;; # description
;; we handle only
;; paragraph with 4 lines
;; to be readable
;; for the viewer
;;
;; The descriptions belong to subsections (** scenes 1: scene title)
;; which belongs to the main section (* scenes).  Each paragraph of
;; the story must start in the line following (# description) line and
;; end up at the first empty line.

;;; Packages

(require 'comment) ; https://github.com/tonyaldon/emacs.d/blob/master/settings/packages/comment.el
(require 'f)
(require 's)

;;; Parse story

(defun ie-story-parse-goto-next-description (&optional bound)
  "Move point to the next description paragraph.

Return (point) if a description paragraph is found.
nil if not."
  (when (looking-at "# description") (forward-char))
  (when (search-forward "# description" bound t)
    (next-line)
    (beginning-of-line)
    (point)))

(defun ie-story-parse-description (description-beginning)
  "Return the description paragraph at DESCRIPTION-BEGINNING
as a list of string

Each element represents a line."
  (save-excursion
    (when description-beginning
      (let ((description-end (progn (search-forward-regexp "^$" nil t)
                                    (backward-char)
                                    (point))))
        (s-lines (buffer-substring-no-properties description-beginning
                                                 description-end))))))

(defun ie-story-parse-scene-title (scene-buffer-position
                                   &optional kebab-case)
  "Return the title of the scene at SCENE-BUFFER-POSITION.

If KEBAB-CASE is t, return the title of the scene but in kebab-case."
  (save-excursion
    (goto-char scene-buffer-position)
    (search-forward-regexp ": *" nil t)
    (let* ((beg (point))
           (end (progn (end-of-line) (point)))
           (name (buffer-substring-no-properties beg end)))
      (if kebab-case
          (s-downcase (s-replace " " "-" name))
        name))))

(defun ie-story-parse-next-scene ()
  "Return buffer position of the next scene heading."
  (save-excursion
    (when (looking-at "^\\** scene ") (forward-char))
    (when (search-forward-regexp "^\\** scene " nil t)
      (beginning-of-line)
      (point))))

(defun ie-story-parse-goto-next-scene ()
  "Go to the next heading scene."
  (when-let ((next-scene (ie-story-parse-next-scene)))
    (goto-char next-scene)))

(defun ie-story-parse-scenes ()
  "Return the list of buffer position of the scenes in the current buffer."
  (save-excursion
    (beginning-of-buffer)
    (let ((scenes '()))
      (while (ie-story-parse-goto-next-scene)
        (beginning-of-line)
        (add-to-list 'scenes (point))
        (next-line))
      (reverse scenes))))

;;; Comments

;;;; ie-story-parse

(comment ; ie-story-parse-goto-next-description
 (let ((story
        "#+TITLE: Inside Emacs
#+AUTHOR: Tony aldon

* a heading
* another heading
* scenes
** scene 0: intro
# description
a description splited
into two lines

** scene 1: First Scene
# description
A one line paragraph

# description
we handle only
paragraph with 4 lines
to be readable
for the reader"))
   (with-temp-buffer
     (insert story)
     (beginning-of-buffer)
     (ie-story-parse-goto-next-description)
     (equal (line-number-at-pos) 9))) ; t
 )

(comment ; ie-story-parse-description
 (let ((story
        "#+TITLE: Inside Emacs
#+AUTHOR: Tony aldon

* a heading
* another heading
* scenes
** scene 0: intro
# description
a description splited
into two lines

** scene 1: First Scene
# description
A one line paragraph

# description
we handle only
paragraph with 4 lines
to be readable
for the reader"))
   (with-temp-buffer
     (insert story)
     (goto-line 9)
     (ie-story-parse-description (point)))) ; ("a description splited" "into two lines")
 )

(comment ; ie-story-parse-scene-title
 (let ((story
        "#+TITLE: Inside Emacs
#+AUTHOR: Tony aldon

* a heading
* another heading
* scenes
** scene 0: intro
# description
a description splited
into two lines

** scene 1: First Scene
# description
A one line paragraph

# description
we handle only
paragraph with 4 lines
to be readable
for the reader"))
   (with-temp-buffer
     (insert story)
     (goto-line 7)
     (message "%s" (ie-story-parse-scene-title (point)))
     (message "%s" (ie-story-parse-scene-title (point) t))
     (goto-line 12)
     (message "%s" (ie-story-parse-scene-title (point)))
     (message "%s" (ie-story-parse-scene-title (point) t))
     ))
 )

(comment ; ie-story-parse-next-scene
 (let ((story
        "#+TITLE: Inside Emacs
#+AUTHOR: Tony aldon

* a heading
* another heading
* scenes
** scene 0: intro
# description
a description splited
into two lines

** scene 1: First Scene
# description
A one line paragraph

# description
we handle only
paragraph with 4 lines
to be readable
for the reader"))
   (with-temp-buffer
     (insert story)
     (beginning-of-buffer)
     (let ((scene-0 (save-excursion (goto-line 7) (point))))
       (equal (ie-story-parse-next-scene) scene-0)))) ; t
 )

(comment ; ie-story-parse-goto-next-scene
 (let ((story
        "#+TITLE: Inside Emacs
#+AUTHOR: Tony aldon

* a heading
* another heading
* scenes
** scene 0: intro
# description
a description splited
into two lines

** scene 1: First Scene
# description
A one line paragraph

# description
we handle only
paragraph with 4 lines
to be readable
for the reader"))
   (with-temp-buffer
     (insert story)
     (beginning-of-buffer)
     (ie-story-parse-goto-next-scene)
     (equal (line-number-at-pos) 7))) ; t
 )

(comment ; ie-story-parse-scenes
 (let ((story
        "#+TITLE: Inside Emacs
#+AUTHOR: Tony aldon

* a heading
* another heading
* scenes
** scene 0: intro
# description
a description splited
into two lines

** scene 1: First Scene
# description
A one line paragraph

# description
we handle only
paragraph with 4 lines
to be readable
for the reader"))
   (with-temp-buffer
     (insert story)
     (let ((scene-1 (progn (goto-line 7) (point)))
           (scene-2 (progn (goto-line 12) (point))))
       (equal (ie-story-parse-scenes) (list scene-1 scene-2))))) ; t
 )

;;;; emacs-lisp

(comment ; s-concat, s-join, f-join, f-mkdir
 (f-join "r-images"
         (s-concat
          (s-join "-" `("description" "scene" ,(number-to-string 1)))
          ".svg")) ; "r-images/description-scene-1.svg"
 (f-mkdir "dir-test")
 (s-lines "uie\ntony") ; '("uie" "tony")
 (s-replace " " "-" "tony aldon") ; "tony-aldon"

 )

(comment ; reverse, pop, search-forward-regexp
 (search-forward-regexp "^$")
 (setq test-list '(a b c)) ; '(a b c)
 (reverse test-list) ; '(c b a)
 (pop test-list) ; a
 test-list ; '(b c)
 )

;;; Footer

(provide 'ie-story-parse)
