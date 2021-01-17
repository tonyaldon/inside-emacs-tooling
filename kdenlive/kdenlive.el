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

(comment ; kdenlive-producer-image, kdenlive-property
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
