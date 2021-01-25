;;; Example 2

;; Our goal is to generate a .kdenlive project file named
;; "3-images-2-folders-no-timeline.kdenlive".
;;
;; To do it we use `kdenlive-skeleton-with-images'and
;; `kdenlive-write' functions.
;;
;; Our current directory, looks like this:
;;
;; .
;; ├── example-2.el
;; ├── kdenlive
;; │   └── 3-images-2-folders-no-timeline.kdenlive
;; └── r-images
;;     ├── image-in-folder-1.svg
;;     ├── image-in-folder-2.svg
;;     └── not-in-a-folder.svg
;;
;; In GUI kdenlive, we want:
;;   "image-in-folder-1.svg" belongs to "folder-1" folder
;;   "image-in-folder-2.svg" belongs to "folder-2" folder
;;   "not-in-a-folder.svg" belongs to any specific folders

(require 'kdenlive)

(let* ((path (f-full "kdenlive/3-images-2-folders-no-timeline.kdenlive"))
       (root (f-full "kdenlive"))
       (folders '("folder-1" "folder-2"))
       (images
        `((300 ,(f-full "r-images/image-in-folder-1.svg") "folder-1")
          (300 ,(f-full "r-images/image-in-folder-2.svg") "folder-2")
          (300 ,(f-full "r-images/not-in-a-folder.svg") nil))))
  (setq mlt (kdenlive-skeleton-with-images root folders images))
  (unless (f-exists? (f-join default-directory "kdenlive"))
    (f-mkdir "kdenlive"))
  (kdenlive-write mlt path t))
