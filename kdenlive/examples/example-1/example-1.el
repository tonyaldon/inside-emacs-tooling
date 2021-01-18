;;; Example 1

;; We create a .kdenlive project:
;;     kdenlive/3-images-2-folders-no-timeline.kdenlive
;; The root of the project (where reside the .kdenlive file) is:
;;     kdenlive/
;; The project has 3 svg images:
;;     r-images/image-1-in-folder-images.svg
;;     r-images/image-2-in-folder-images.svg
;;     r-images/not-in-a-folder.svg
;; In GUI kdenlive, emacs-power.svg and generate-kdenlive-file.svg images
;; belongs to the folder: "images".
;; not-in-a-folder.svg images doesn't belong to any specific GUI kdenlive folder.
;; In GUI kdenlive, "audios" is an empty folder.

(require 'kdenlive)

(let* ((path (f-full "kdenlive/3-images-2-folders-no-timeline.kdenlive"))
			 (root (f-full "kdenlive/"))
			 (mlt (kdenlive-mlt root))
			 (folders '((1 . "images") (2 . "audios")))
			 (producers-resources-folders
				`(; ((PRODUCER-ID DURATION) RESOURCE FOLDER-ID)
					((1 . 300) ,(f-full "r-images/image-1-in-folder-images.svg") 1)
					((2 . 300) ,(f-full "r-images/image-2-in-folder-images.svg") 1)
					((3 . 300) ,(f-full "r-images/not-in-a-folder.svg") nil)))
			 (playlists '(; (ID NAME AUDIO)
										(1  "Audio 2" t)
										(2  "Audio 1" t)
										(3  "Video 1" nil)
										(4  "Video 2" nil)
										(5  "Video 3" nil)))
			 (tracks '(; (PLAYLIST-ID HIDE)
								 (1 . "video")
								 (2 . "video")
								 (3 . nil)
								 (4 . nil)
								 (5 . nil))))
	(setq mlt (kdenlive-append mlt (kdenlive-profile)))
	(setq mlt (--reduce-from (kdenlive-append
														acc (kdenlive-producer-image (caar it) (cdar it)
																												 (nth 1 it) (nth 2 it)))
													 mlt producers-resources-folders))
	(setq mlt (kdenlive-append mlt (kdenlive-playlist-main-bin
																	(-map 'car producers-resources-folders)
																	folders)))
	(setq mlt (kdenlive-append mlt (kdenlive-producer-black)))
	(setq mlt (kdenlive-append mlt (kdenlive-playlist-black-track)))
	(setq mlt (--reduce-from (kdenlive-append
														acc (kdenlive-playlist (car it) (nth 1 it) (nth 2 it)))
													 mlt playlists))
	(setq mlt (kdenlive-append mlt (kdenlive-maintractor tracks)))

	(kdenlive-write mlt path t))
