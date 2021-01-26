;;; About

;; A boring part but really important when making Inside Emacs videos
;; is to transform the story (written in a specific formated text
;; file) to beautiful footage.  So far it involves 3 steps:
;;   1) parse the story (specific formated text),
;;   2) generate "svg" files (the story) from the specific
;;      formated text,
;;   3) generate "kdenlive" files with the previous "svg" files.
;;
;; In this file, we treat the parts 2) and 3) and
;; The part 1) is treated in the file ./ie-story-parse.el .
;;
;; Usage:
;; To generate the description svg files telling the story of
;; Inside Emacs videos, visite the file where the story is written
;; (following the format described in ./ie-story-parse.el and run
;; the command `ie-story-generate-all-descriptions-svg':
;;
;;   M-x ie-story-generate-all-descriptions-svg
;;
;; The generated svg files are saved in the subdirectory
;; `ie-story-generate-images-dir'.

;;; Packages

(require 'comment) ; https://github.com/tonyaldon/emacs.d/blob/master/settings/packages/comment.el
(require 'dash)
(require 'f)
(require 'ie-story-parse)
(require 'svg)

;;; Generate svg files

(defvar ie-story-generate-images-dir "r-images"
  "Name of the subdirectory where the svg images are saved.

See `ie-story-generate-all-descriptions-svg'.")

(defun ie-story-generate-scene-title-svg (scene-buffer-position folder)
  "Generate svg title scene of Inside Emacs at SCENE-BUFFER-POSITION.

The svg file generated is save in FOLDER with a unique name."
  (unless (f-exists? folder) (f-mkdir folder))
  (let* ((title (ie-story-parse-scene-title scene-buffer-position))
         (title-kebab-case (ie-story-parse-scene-title
                            scene-buffer-position t))
         (path (f-join folder
                       (s-concat
                        (s-join "-" `("description"
                                      ,title-kebab-case
                                      "title"))
                        ".svg")))
         (svg (svg-create 1920 1080))
         (style "font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:96px;line-height:1.25;font-family:Ramabhadra;-inkscape-font-specification:'Ramabhadra Bold';letter-spacing:0px;word-spacing:0px;fill:#f0f0f0;fill-opacity:0.941176;stroke:none;stroke-width:0.264583")
         (x-start-line 90)
         (y-start-line 194))
    (svg-text svg title
              :style style :x x-start-line :y y-start-line)
    (with-temp-buffer
      (svg-print svg)
      (write-region (point-min) (point-max) path))))

(defun ie-story-generate-description-svg (lines scene index folder)
  "Generate svg description of Inside Emacs.

LINES is a list of strings representing the description.
SCENE is the scene (kebab-case) of the scene the description belongs to.
INDEX is the apparition order of the description in the SCENE.

The svg file generated is save in FOLDER with a unique name."
  (unless (f-exists? folder) (f-mkdir folder))
  (let ((path (f-join folder
                      (s-concat
                       (s-join "-" `("description" ,scene ,(number-to-string index)))
                       ".svg")))
        (svg (svg-create 1920 1080))
        (style "font-style:normal;font-variant:normal;font-weight:bold;font-stretch:normal;font-size:96px;line-height:1.25;font-family:Ramabhadra;-inkscape-font-specification:'Ramabhadra Bold';letter-spacing:0px;word-spacing:0px;fill:#f0f0f0;fill-opacity:0.941176;stroke:none;stroke-width:0.264583")
        (lines-reversed (reverse lines))
        (x-start-line 90)
        (y-start-lines '(1016 896 776 656)))
    (while lines-reversed
      (svg-text svg (pop lines-reversed)
                :style style :x x-start-line :y (pop y-start-lines)))
    (with-temp-buffer
      (svg-print svg)
      (write-region (point-min) (point-max) path))))

(defun ie-story-generate-descriptions-in-scene-svg (scene-buffer-position folder)
  "Generate all svg descriptions of Inside Emacs in the scene at SCENE-BUFFER-POSITION.

The svg files generated are saved in FOLDER.
See `ie-story-generate-all-descriptions-svg'."
  (save-excursion
    (goto-char scene-buffer-position)
    (let* ((bound (ie-story-parse-next-scene))
           (scene-name (ie-story-parse-scene-title scene-buffer-position t))
           (description-index 1)
           description-lines)
      (while (ie-story-parse-goto-next-description bound)
        (setq description-lines (ie-story-parse-description (point)))
        (ie-story-generate-description-svg description-lines
                                           scene-name
                                           description-index
                                           folder)
        (setq description-index (1+ description-index))))))

(defun ie-story-generate-all-descriptions-svg ()
  "Generate all svg descriptions of Inside Emacs for the current buffer.

The files are saved in the subdirectory `ie-story-generate-images-dir'."
  (interactive)
  (--each (ie-story-parse-scenes)
    (ie-story-generate-scene-title-svg
     it ie-story-generate-images-dir)
    (ie-story-generate-descriptions-in-scene-svg
     it ie-story-generate-images-dir))
  (message "svg description files saved in %s"
           ie-story-generate-images-dir))

;;; Comments

(comment ; ie-story-generate-scene-title-svg
 (let ((default-directory (f-full "test"))
       (folder "r-images")
       (story
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
     (ie-story-generate-scene-title-svg (point) folder)
     (goto-line 12)
     (ie-story-generate-scene-title-svg (point) folder)))
 )

(comment ; ie-story-generate-description-svg
 (let ((default-directory (f-full "test"))
       (folder "r-images"))
   (ie-story-generate-description-svg '("line 1") "my-scene" 1 folder)
   (ie-story-generate-description-svg '("line 1" "line 2")
                                      "my-scene" 2 folder)
   (ie-story-generate-description-svg '("line 1" "line 2" "line 3")
                                      "my-scene" 3 folder)
   (ie-story-generate-description-svg '("line 1" "line 2"
                                        "line 3" "line 4")
                                      "my-scene" 4 folder))
 )

(comment ; ie-story-generate-descriptions-in-scene-svg
 (let ((default-directory (f-full "test"))
       (folder "r-images")
       (story
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
     (ie-story-generate-descriptions-in-scene-svg (point) folder)
     (goto-line 12)
     (ie-story-generate-descriptions-in-scene-svg (point) folder)))
 )

(comment ; ie-story-generate-all-descriptions-svg
 (let ((default-directory (f-full "test"))
       (story
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
     (call-interactively 'ie-story-generate-all-descriptions-svg)))
 )

;;; Footer

(provide 'ie-story-generate)
