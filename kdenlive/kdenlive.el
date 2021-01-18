;;; About

;; Functions and commands that can generate .kdenlive file.
;; That means files that kdenlive understands.
;; see: https://kdenlive.org, https://github.com/KDE/kdenlive
;;
;; [2021-01-15 Fri] :
;;   I'm working on kdenlive version 17.12.3 thought
;;   I know that we are already past version 20.12,
;;   but in my old laptop kdenlive 20.12 is too slow.
;;   Nevertheless, if you generate a .kdenlive file with this package,
;;   when opening your file with kdenlive 20.12, kdenlive ask you to
;;   to adapt your file to the newer version.
;;   I'm fine with that.  My goal with this package isn't to manipulate deeply
;;   or generate complex kdenlive files, but just to help me automate some
;;   repetive tasks involving kdenlive during the production of the Inside Emacs
;;   videos : https://www.youtube.com/channel/UCQCrbWOFRmFYqoeou0Qv3Kg

;;; Code

;; in svg.el and dom.el, it seems that almost every operation on the dom
;; is destructive.
;; In kdenlive.el, we try to work with pure function (take an input value
;; and return a value without changing the state of emacs inside the function).

(require 'xml)
(require 'dom)
(require 'comment) ; https://github.com/tonyaldon/emacs.d/blob/master/settings/packages/comment.el

(defvar kdenlive-version "17.12.3"
  "The kdenlive version.")

(defvar kdenlive-mlt-default
  '((title . "Anonymous Submission")
    (version . "6.6.0")
    (producer . "main bin")
    (LC_NUMERIC . "en_US.UTF-8"))
  "Alist of (attribute . value) that define the kdenlive mlt root node.")

(defvar kdenlive-profile-hd-1080p-60fps
  '((width . "1920")
    (frame_rate_den . "1")
    (height . "1080")
    (display_aspect_num . "16")
    (display_aspect_den . "9")
    (frame_rate_num . "60")
    (colorspace . "709")
    (sample_aspect_den . "1")
    (description . "HD 1080p 60 fps")
    (progressive . "1")
    (sample_aspect_num . "1"))
  "Alist of (attribute . value) that define the kdenlive profile
for a video with the caracteristics \"HD 1080p 60 fps\".")

(defvar kdenlive-producer-image-properties-hd-1080p-60fps
  '(("eof" . "pause")
    ("ttl" . "25")
    ("aspect_ratio" . "1")
    ("progressive" . "1")
    ("seekable" . "1")
    ("loop" . "1")
    ("mlt_service" . "pixbuf")
    ("meta.media.width" . "1920")
    ("meta.media.height" . "1080"))
  "Alist of default property names and values (\"name\" . \"value\") for
an image producer 1920x1080 pixels and the video edited at 60fps.

\"length\", \"resource\", \"kdenlive:folderid\" and \"kdenlive:duration\" are
property names that vary.  This is why they are not in
`kdenlive-producer-image-properties-hd-1080p-60fps'.

See: `kdenlive-producer-image'.")

(defvar kdenlive-docproperties-hd-1080p-60fps
  '(("audiotargettrack" . "2")
    ("decimalPoint" . ".")
    ("disablepreview" . "0")
    ("enableproxy" . "0")
    ("generateimageproxy" . "0")
    ("generateproxy" . "0")
    ("kdenliveversion" . "17.12.3")
    ("position" . "0")
    ("profile" . "atsc_1080p_60")
    ("proxyextension" . "mkv")
    ("proxyimageminsize" . "2000")
    ("proxyminsize" . "1000")
    ("proxyparams" . "-vf yadif,scale=960:-2 -qscale 3 -vcodec mjpeg -acodec pcm_s16le")
    ("version" . "0.96")
    ("verticalzoom" . "1")
    ("videotargettrack" . "3")
    ("zonein" . "0")
    ("zoneout" . "100")
    ("zoom" . "7"))
  "Alist of default property names and values (\"name\" . \"value\") for
the <playlist id =\"main bin\"> of a kdenlive project edited in HD 1080p at 60fps.

Here are exclude the properties \"previewchunks\", \"previewextension\", \"previewparameters\",
\"dirtypreviewchunks\", \"documentid\".  We maybe have to add them later.

See: `kdenlive-playlist-main-bin'.")

(defun kdenlive-property (name child)
  "Return a 'property' node with attribute 'name' and value NAME.

CHILD corresponds to the value of the property name.
For instance, for the kdenlive property node,

  <property name=\"length\">60</property>

we produce it by this function call:

  (kdenlive-property \"length\" \"60\")."
  (dom-node 'property `((name . ,name)) child))

(defun kdenlive-producer-image (id duration resource &optional folder producer-properties)
  "Return a producer node for the image RESOURCE.

ID is a identifier number use in the mlt kdenlive dom to identify a producer.
RESOURCE is the absolute path to the image.
DURATION is the number of frames.  For instance, for a 60fps videos,
an image that last 5 secondes has a DURATION equal to 300 (* 60 5).

FOLDER is a number that refer in the mlt kdenlive dom
to the folder the image belongs to in your kdenlive GUI interface.
For instance, if the image belong (in kdenlive) to the folder \"my-folder\"
defined in the .kdenlive file by

<property name=\"kdenlive:folder.-1.2\">descriptions</property>

then FOLDER must be set to 2.

PRODUCER-PROPERTIES is of the form of `kdenlive-producer-image-properties-hd-1080p-60fps'.
"
  (let* ((id-string (int-to-string id))
         (dur (int-to-string duration))
         (out-string (int-to-string (1- duration)))
         (dir (int-to-string (or folder -1)))
         (producer (dom-node 'producer
                             `((id . ,id-string) (out . ,out-string) (in . "0"))))
         (properties (or producer-properties
                         kdenlive-producer-image-properties-hd-1080p-60fps)))
    (--each properties
      (dom-append-child producer (kdenlive-property (car it) (cdr it))))
    (--each `(("resource" . ,resource) ("length" . ,dur)
              ("kdenlive:duration" . ,dur) ("kdenlive:folderid" . ,dir))
      (dom-append-child producer (kdenlive-property (car it) (cdr it))))
    producer))

(defvar kdenlive-producer-black-hd-1080p-60fps
  '(("length" . "15000")
		("eof" . "pause")
		("resource" . "black")
		("aspect_ratio" . "0")
		("mlt_service" . "colour")
		("set.test_audio" . "0"))
  "Alist of default property names and values (\"name\" . \"value\") for
the \"black\" producer 1920x1080 pixels and the video edited at 60fps.

This is a special mandatory producer kdenlive always needs.  It is used
by the playlist \"black_track\".  And the playlist \"black_track\" must
appear first (first child) in the special \"tractor\" node \"maintractor\".")

(defun kdenlive-producer-black (out)
  "Return the producer node \"black\".

See `kdenlive-producer-black-hd-1080p-60fps'."
  (let* ((out-string (int-to-string out))
				 (producer (dom-node 'producer
                             `((id . "black") (out . ,out-string) (in . "0")))))
		(--each kdenlive-producer-black-hd-1080p-60fps
			(dom-append-child producer (kdenlive-property (car it) (cdr it))))
		producer))

(defun kdenlive-entry (id duration)
  "Return an \"entry\" node aimed to be a child of the node \"playlist\"
with id \"main bin\".

ID is the identifier of the producer node in the kdenlive dom and
DURATION is the number of frame the producer lasts. "
  (let ((id-string (int-to-string id))
        (out-string (int-to-string (1- duration))))
    (dom-node 'entry `((producer . ,id-string)
											 (out . ,out-string)
											 (in . "0")))))

;; I may have to add this properties to <playlist id="main bin"> later
;; kdenlive:documentnotes
;; kdenlive:clipgroups

;; ("xml_retain" . "1")

(defun kdenlive-playlist-main-bin (producers &optional folders)
  "Return the node \"playlist\" with id \"main bin\".

This node holds project-specific (meta) data, the
bin folders as well as their hierarchy, clip groups,
and some more stuff.

PRODUCERS is an alist of (ID . DURATION) where ID is the identifier
of a producer node in the kdenlive dom and DURATION is number of frame
the producer last.

FOLDERS is an alist of (ID . FOLDER) where FOLDER is the name
of the folder use in the GUI kdenlive interface and ID is the
identifier use in the kdenlive dom by the producers.  ID must
be a strickly positive integer.

See `kdenlive-docproperties-hd-1080p-60fps'."
  (let ((main-bin (dom-node 'playlist `((id . "main bin")))))
	  (--each folders
			(dom-append-child
			 main-bin
			 (kdenlive-property
				(s-concat "kdenlive:folder.-1." (int-to-string (car it)))
				(cdr it))))
    (--each producers
      (dom-append-child main-bin (kdenlive-entry (car it) (cdr it))))
		(--each kdenlive-docproperties-hd-1080p-60fps
			(dom-append-child
			 main-bin
			 (kdenlive-property
				(s-concat "kdenlive:docproperties." (car it))
				(cdr it))))
		(dom-append-child main-bin (kdenlive-property "xml_retain" "1"))
    main-bin))

(defun kdenlive-mlt (root kdenlive-mlt-alist)
  "Return the mlt `dom-node' of a kdenlive project with the caracteristics
describe in KDENLIVE-MLT-ALIST and setting path root to ROOT.

See `kdenlive-mlt-default' for an example of KDENLIVE-PROFILE-ALIST."
  (let ((kd-mlt-alist (cons `(root . ,root) kdenlive-mlt-alist)))
    (dom-node 'mlt kd-mlt-alist)))

(defun kdenlive-profile (kdenlive-profile-alist)
  "Return the profile `dom-node' of a kdenlive project with the caracteristics
describe in KDENLIVE-PROFILE-ALIST.

See `kdenlive-profile-hd-1080p-60fps' for an example of KDENLIVE-PROFILE-ALIST."
  (dom-node 'profile kdenlive-profile-alist))

(defun kdenlive-append (mlt node)
  "Append NODE to the children of kdenlive MLT node."
  (let ((mlt-local mlt))
    (dom-append-child mlt-local node)))

(defun kdenlive-print (mlt)
  "Convert DOM into a string containing the xml representation."
  (insert "<?xml version='1.0 encoding='utf-8'?>")
  (dom-print mlt))


;;;; TODO
;;;;; playlist id="main bin"

;; any producer node <producer id="1" out="603" in="0"> must be added
;; to <playlist id="main bin"> node as <entry out="603" producer="1" in="0"/>

;;;;; producer

;;;;; producer id="black"
;;;;; playlist id="black_track"

;;;;; playlist

;;;;; tractor id="maintractor"

;;; Comments

;;;; kdenlive

(comment ; kdenlive-producer-image, kdenlive-property, kdenlive-producer-black
 (dom-print
  (kdenlive-producer-image 2 60 "/absolute/path/to/image.svg" nil '(("tony" . "jim")))) ; <producer id="2" out="59" in="0"><property name="tony">jim</property><property name="resource">/absolute/path/to/image.svg</property><property name="length">60</property><property name="kdenlive:duration">60</property><property name="kdenlive:folderid">-1</property></producer>
 (dom-print
  (kdenlive-producer-image 2 60 "/absolute/path/to/image.svg")) ; <producer id="2" out="59" in="0"><property name="eof">pause</property><property name="ttl">25</property><property name="aspect_ratio">1</property><property name="progressive">1</property><property name="seekable">1</property><property name="loop">1</property><property name="mlt_service">pixbuf</property><property name="meta.media.width">1920</property><property name="meta.media.height">1080</property><property name="resource">/absolute/path/to/image.svg</property><property name="length">60</property><property name="kdenlive:duration">60</property><property name="kdenlive:folderid">-1</property></producer>
 (dom-print
  (kdenlive-producer-image 2 60 "/absolute/path/to/image.svg" 3)
  t)

 ;; <producer id="2" out="59" in="0">
 ;; <property name="eof">pause</property>
 ;; <property name="ttl">25</property>
 ;; <property name="aspect_ratio">1</property>
 ;; <property name="progressive">1</property>
 ;; <property name="seekable">1</property>
 ;; <property name="loop">1</property>
 ;; <property name="mlt_service">pixbuf</property>
 ;; <property name="meta.media.width">1920</property>
 ;; <property name="meta.media.height">1080</property>
 ;; <property name="resource">/absolute/path/to/image.svg</property>
 ;; <property name="length">60</property>
 ;; <property name="kdenlive:duration">60</property>
 ;; <property name="kdenlive:folderid">3</property>
 ;; </producer>

 (dom-print (dom-append-child (dom-node 'producer '((id . 1) (out . 60) (in . 0)))
                              (kdenlive-property "length" "60")))
 (dom-print
  (let ((producer (dom-node 'producer '((id . 1) (out . 60) (in . 0)))))
    (--each '(("test" . "11") ("test2" . "22"))
      (dom-append-child producer
                        (kdenlive-property (car it) (cdr it))))
    producer))
 (dom-print (kdenlive-property "length" "60")) ; <property name="length">60</property>

 (dom-print (kdenlive-producer-black 500))
 )

(comment ; kdenlive-entry, kdenlive-playlist-main-bin
 (dom-print (kdenlive-entry 2 60)) ; <entry producer="2" out="59" in="0" />
 (dom-print (kdenlive-playlist-main-bin nil nil))
 (kdenlive-playlist-main-bin '((1 . 60) (2 . 180)))
 (kdenlive-playlist-main-bin '((1 . 60) (2 . 180))
														 '((1 . "folder-1") (2 . "folder-2")))
 )
(comment ; kdenlive-mlt, kdenlive-append, kdenlive-profile, kdenlive-print
 (kdenlive-profile kdenlive-profile-hd-1080p-60fps)
 (setq kd-root (f-join default-directory "test"))
 (kdenlive-mlt kd-root kdenlive-mlt-default)
 (kdenlive-append (dom-node 'mlt)
                  (kdenlive-profile '((width . "1920") (height . "1080"))))
 (kdenlive-print (dom-node 'mlt)) ; <?xml version='1.0 encoding='utf-8'?><mlt />
 (kdenlive-print
  (kdenlive-append (dom-node 'mlt)
                   (kdenlive-profile '((width . "1920") (height . "1080"))))) ; <?xml version='1.0 encoding='utf-8'?><mlt><profile width="1920" height="1080" /></mlt>
 (setq kd-root (f-join default-directory "test"))
 (kdenlive-print (kdenlive-append (kdenlive-mlt kd-root kdenlive-mlt-default)
                                  (kdenlive-profile kdenlive-profile-hd-1080p-60fps)))
 )

;;;; dom.el

(comment ; dom-node, dom-attr, dom-by-id
 (info "(elisp) Document Object Model")
 (dom-node 'body '((width . "100"))) ; (body ((width . "100")))
 (dom-attr (dom-node 'body '((width . "100"))) 'width) ; "100"
 (dom-attr (dom-node 'body '((width . "100"))) 'id) ; nil
 (dom-attr (dom-node 'body '((width . "100") (id . "body-id"))) 'id) ; "body-id"
 (let ((dom-1 (dom-node
               'body
               '((width . "101"))
               '(div ((class . "thing"))
                     "Foo"
                     (div nil
                          "Yes"))))
       (dom-2 '(body
                ((width . "101"))
                (div ((class . "thing"))
                     "Foo"
                     (div nil
                          "Yes")))))
   (equal dom-1 dom-2)) ; t

 (dom-by-id (dom-node 'body '((width . "100")))
            "node-id") ; nil
 (dom-by-id (dom-node 'body '((id . "node-id") (width . "100")))
            "node-id") ; ((body ((id . "node-id") (width . "100"))))
 (dom-by-id (dom-node 'body '((id . "node-other-id") (width . "100")))
            "node-id") ; nil
 (dom-by-id
  (dom-node
   'body '((id . "body-id"))
   '(div ((id . "same-id") (width . "100")))
   '(div ((id . "same-id") (width . "200"))))
  "same-id") ; ((div ((id . "same-id") (width . "100"))) (div ((id . "same-id") (width . "200"))))
 )

(comment ; dom-pp, dom-print
 (dom-pp (dom-node 'body '((width . "100")))) ; (body ((width . "100")))
 (dom-print (dom-node 'body '((width . "100")))) ; <body width="100" />
 (dom-print
  (dom-node
   'body
   '((width . "101"))
   '(div ((class . "thing"))
         "Foo"
         (div nil
              "Yes")))
  t)

 ;; <body width="101">
 ;; <div class="thing">Foo
 ;; <div>Yes</div>
 ;; </div>
 ;; </body>

 (dom-print
  (dom-node
   'body '((id . "body-id"))
   '(div ((id . "div-id-1")))
   '(div ((id . "div-id-2"))))
  t)

 ;; <body id="body-id">
 ;; <div id="div-id-1" />
 ;; <div id="div-id-2" />
 ;; </body>

 (dom-print
  (dom-append-child
   (dom-node
    'body
    '((width . "101"))
    '(div ((class . "thing"))
          "Foo"
          (div nil
               "Yes")))
   (dom-node 'div nil "append"))
  t)

 ;; <body width="101">
 ;; <div class="thing">Foo
 ;; <div>Yes</div>
 ;; </div>
 ;; <div>append</div>
 ;; </body>

 (dom-print
  (dom-add-child-before
   (dom-node
    'body
    '((width . "101"))
    '(div ((class . "thing"))
          "Foo"
          (div nil
               "Yes")))
   (dom-node 'div nil "first child")
   nil)
  t)

 ;; <body width="101">
 ;; <div>first child</div>
 ;; <div class="thing">Foo
 ;; <div>Yes</div>
 ;; </div>
 ;; </body>
 )

;;;; emacs-lisp

(comment ; setcdr, setcar, nconc
 ;; setcdr is destructive
 (setq test-setcdr '(a b c))
 (setcdr test-setcdr '(d e)) ; (d e)
 test-setcdr ; (a d e)
 ;; setcar is destructive
 (setq test-setcar '(a b c))
 (setcar test-setcar 'z) ; z
 test-setcar ; (z b c)
 ;; nconc is destructive but for its last arguments that must be a list
 (setq test-nconc-list-1 '(A B C))
 (nconc '(u) '(v w) test-nconc-list-1) ; (u v w A B C)
 test-nconc-list-1 ; (A B C)
 (setq test-nconc-list-2 '(D E F))
 (nconc test-nconc-list-2 '(u) '(v w)) ; (D E F u v w)
 test-nconc-list-2
 )

(comment ; cddr, regexp-quote
 (cddr '(a u i e)) ; (i e)
 (cons '1 nil) ; (1)
 (cons nil '1) ; (nil . 1)
 (regexp-quote "\\") ; "\\\\"
 )

;;; Footer

(provide 'kdenlive)
