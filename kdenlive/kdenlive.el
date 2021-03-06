;;; About

;; Functions and commands that can generate .kdenlive files.
;; That means files that kdenlive understands.
;; see: https://kdenlive.org, https://github.com/KDE/kdenlive
;;
;; [2021-01-15 Fri] :
;;   I'm working on kdenlive version 17.12.3 thought
;;   I know that we are already past version 20.12,
;;   but in my old laptop kdenlive 20.12 is too slow.
;;   Nevertheless, if you generate a .kdenlive file with this package,
;;   when opening your file with kdenlive 20.12, kdenlive asks you to
;;   to adapt your file to the newer version.
;;   I'm fine with that.  My goal with this package isn't to deeply manipulate
;;   or generate complex kdenlive files, but just to help me automate some
;;   repetive tasks involving kdenlive during the production of the Inside Emacs
;;   videos : https://www.youtube.com/channel/UCQCrbWOFRmFYqoeou0Qv3Kg

;;; Code

;; Note:
;; In svg.el and dom.el, it seems that almost every operation on
;; the dom is destructive.
;; In kdenlive.el, we try to work with pure functions (take an input
;; value and return a value without changing the state of emacs in
;; the body function).

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

(defvar kdenlive-producer-black-id "black"
  "id of the producer `kdenlive-producer-black'.

This variable is also used in `kdenlive-playlist-black-track'.")

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

(defvar kdenlive-playlists
  '((1  "Audio 2" t)
    (2  "Audio 1" t)
    (3  "Video 1" nil)
    (4  "Video 2" nil)
    (5  "Video 3" nil))
  "The default list of (ID NAME AUDIO) playlists used by kdenlive.

ID in `kdenlive-playlists' is an PLAYLIST-ID in `kdenlive-tracks'.")

(defvar kdenlive-tracks
  '((1 . "video")
    (2 . "video")
    (3 . nil)
    (4 . nil)
    (5 . nil))
  "The default alist (PLAYLIST-ID HIDE) representing the stack of
tracks in kdenlive.

The order matters.  The first cons in the alist `kdenlive-tracks'
is the bottom track in kdenlive.

PLAYLIST-ID refers to an ID in `kdenlive-playlists'.")

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

(defun kdenlive-producer-black (&optional out)
  "Return the producer node \"black\".

See `kdenlive-producer-black-hd-1080p-60fps'."
  (let* ((out-string (or (and out (int-to-string out)) "500"))
         (producer (dom-node 'producer
                             `((id . ,kdenlive-producer-black-id)
                               (out . ,out-string)
                               (in . "0")))))
    (--each kdenlive-producer-black-hd-1080p-60fps
      (dom-append-child producer (kdenlive-property (car it) (cdr it))))
    producer))

(defun kdenlive-playlist-black-track ()
  "Return the \"black_track\" playlist node.

See `kdenlive-producer-black' and `kdenlive-producer-black-hd-1080p-60fps'."
  (let ((playlist (dom-node 'playlist '((id . "black_track")))))
    (dom-append-child playlist
                      (dom-node 'entry `((producer . ,kdenlive-producer-black-id)
                                         (out . "0")
                                         (in . "0"))))
    playlist))

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

(defun kdenlive-playlist-id (id)
  "Return the formatted playlist identifier from ID."
  (s-concat "playlist" (int-to-string id)))

(defun kdenlive-playlist (id name &optional audio)
  "Return a playlist node.

If AUDIO is non-nil, the playist is an audio track."
  (let* ((playlist-id (kdenlive-playlist-id id))
         (playlist (dom-node 'playlist `((id . ,playlist-id)))))
    (dom-append-child playlist (kdenlive-property "kdenlive:track_name" name))
    (when audio
      (dom-append-child playlist (kdenlive-property "kdenlive:audio_track" "1")))
    playlist))

(defun kdenlive-track (id hide)
  "Return a \"track\" node.

ID is the id of a playlist as in `kdenlive-playlist-id'.
HIDE can be nil, \"video\" or \"audio\"."
  (let ((playlist-id (kdenlive-playlist-id id)))
    (if hide
        (dom-node 'track `((producer . ,playlist-id) (hide . ,hide)))
      (dom-node 'track `((producer . ,playlist-id))))))

(defun kdenlive-maintractor (tracks)
  "Return the \"maintractor\" tractor node.

TRACKS is an alist (ID . HIDE) where ID is the id of a playlist
as in `kdenlive-playlist-id'.  HIDE can be nil, \"video\" or \"audio\"."
  (let* ((maintractor
          (dom-node 'tractor
                    `((id . "maintractor")
                      (title . "Anonymous Submission")
                      (global_feed . "1")
                      (out . "0")
                      (in . "0")))))
    (dom-append-child maintractor
                      (dom-node 'track '((producer . "black_track"))))
    (--each tracks
      (dom-append-child maintractor (kdenlive-track (car it) (cdr it))))
    maintractor))

(defun kdenlive-mlt (root &optional mlt-attributes)
  "Return the mlt `dom-node' of a kdenlive project with the attributes
describe in MLT-ATTRIBUTES and setting path root to ROOT.

See `kdenlive-mlt-default' for an example of MLT-ATTRIBUTES."
  (let ((mlt-attibutes-with-root
         (cons `(root . ,root)
               (or mlt-attributes kdenlive-mlt-default))))
    (dom-node 'mlt mlt-attibutes-with-root)))

(defun kdenlive-profile (&optional profile-attributes)
  "Return the profile `dom-node' of a kdenlive project with the caracteristics
describe in PROFILE-ATTRIBUTES.

See `kdenlive-profile-hd-1080p-60fps' for an example of PROFILE-ATTRIBUTES."
  (dom-node 'profile (or profile-attributes kdenlive-profile-hd-1080p-60fps)))

(defun kdenlive-append (mlt node)
  "Return MLT node with NODE appended to its children."
  (let ((mlt-local mlt))
    (dom-append-child mlt-local node)))

(defun kdenlive-append-image-producers (mlt producers)
  "Return MLT node with image PRODUCERS appended to its children.

MLT is a dom node.
PRODUCERS is a list of (((PRODUCER-ID DURATION) RESOURCE FOLDER-ID)).
For instance, the following list is a valid PRODUCERS list:
  `(((1 . 300) ,(f-full \"image-in-folder-1.svg\") 1)
    ((2 . 300) ,(f-full \"image-in-folder-2.svg\") 2)
    ((3 . 300) ,(f-full \"not-in-a-folder.svg\") nil))"
  (--reduce-from
   (kdenlive-append acc (kdenlive-producer-image
                         (caar it) (cdar it) (nth 1 it) (nth 2 it)))
   mlt producers))

(defun kdenlive-append-playlists (mlt playlists)
  "Return MLT node with PLAYLISTS appended to its children.

MLT is a dom node.
PLAYLISTS is a list of list of the form (ID NAME AUDIO).
`kdenlive-playlists' is a valid PLAYLISTS."
  (--reduce-from
   (kdenlive-append
    acc (kdenlive-playlist (car it) (nth 1 it) (nth 2 it)))
   mlt playlists))

(defun kdenlive-image-in-folder (duration resource in-folder)
  "Return the list '(duration image in-folder).

Descriptive helper aimed to used to build \"images\" list argument
of `kdenlive-skeleton-with-images' function.
DURATION is the number of frames the image last,
RESOURCE is the full path of the image,
FOLDER is a folder name also in FOLDERS the image belong
to inside GUI kdenlive."
  (list duration resource in-folder))

(defun kdenlive-folder-id (folder folders)
  "Return the id of FOLDER based on its position in FOLDERS."
  (when folder
    (1+ (-elem-index folder folders))))

(defun kdenlive-skeleton-with-images (root folders images &optional playlists tracks)
  "Return a `kdenlive-mlt' dom skeleton with ROOT, FOLDERS and IMAGES settings.

ROOT is the full path of the kdenlive project.
FOLDERS is a list of folder names that appears inside GUI kdenlive.
IMAGES is a list of (DURATION RESOURCE FOLDER) where:
   DURATION is the number of frames the image last,
   RESOURCE is the full path of the image,
   FOLDER is a folder name also in FOLDERS the image belong
   to inside GUI kdenlive."
  (let ((folders-alist
         (--annotate (kdenlive-folder-id it folders) folders))
        ;; list of ((PRODUCER-ID DURATION) RESOURCE FOLDER-ID)
        (producers
         (--map-indexed
          (list (cons (1+ it-index) (car it))
                (nth 1 it)
                (kdenlive-folder-id (nth 2 it) folders))
          images))
        (playlists (or playlists kdenlive-playlists))
        (tracks (or tracks kdenlive-tracks)))
    (-> (kdenlive-mlt root)
        (kdenlive-append (kdenlive-profile))
        (kdenlive-append-image-producers producers)
        (kdenlive-append (kdenlive-playlist-main-bin
                          (-map 'car producers)
                          folders-alist))
        (kdenlive-append (kdenlive-producer-black))
        (kdenlive-append (kdenlive-playlist-black-track))
        (kdenlive-append-playlists playlists)
        ;; must be append last
        (kdenlive-append (kdenlive-maintractor tracks)))))

(defun kdenlive-serialize (mlt &optional pretty)
  "Convert DOM into a string containing the xml representation."
  (let ((mlt-xml))
    (with-temp-buffer
      (insert "<?xml version='1.0' encoding='utf-8'?>")
      (when pretty (insert "\n"))
      (dom-print mlt pretty)
      (setq mlt-xml (buffer-substring-no-properties (point-min) (point-max))))
    mlt-xml))

(defun kdenlive-write (mlt path &optional pretty)
  "Write serialized MLT dom to file PATH."
  (f-write (kdenlive-serialize mlt pretty) 'utf-8 path))

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

(comment ; kdenlive-producer-black, kdenlive-playlist-black-track
 (dom-print (kdenlive-producer-black))
 (dom-print (kdenlive-producer-black 500))
 (kdenlive-playlist-black-track) ; (playlist ((id . "black_track")) (entry ((producer . "black") (out . "0") (in . "0"))))
 )
(comment ; kdenlive-entry, kdenlive-playlist-main-bin, kdenlive-playlist
 (dom-print (kdenlive-entry 2 60)) ; <entry producer="2" out="59" in="0" />
 (dom-print (kdenlive-playlist-main-bin nil nil))
 (kdenlive-playlist-main-bin '((1 . 60) (2 . 180)))
 (kdenlive-playlist-main-bin '((1 . 60) (2 . 180))
                             '((1 . "folder-1") (2 . "folder-2")))
 (kdenlive-playlist-id 2) ; "playlist2"
 (dom-print (kdenlive-playlist 3 "Video"))
 (dom-print (kdenlive-playlist 1 "Audio" t))
 )

(comment ; kdenlive-track, kdenlive-maintractor
 (kdenlive-track 2 nil)
 (kdenlive-track 1 "video")
 (dom-print (kdenlive-maintractor '((1 . "video") (2 . "video") (3 . nil)))
            t)

 ;; <tractor id="maintractor" title="Anonymous Submission" global_feed="1" out="0" in="0">
 ;; <track producer="black_track" />
 ;; <track producer="playlist1" hide="video" />
 ;; <track producer="playlist2" hide="video" />
 ;; <track producer="playlist3" />
 ;; </tractor>

 )

(comment ; kdenlive-append-image-producers, kdenlive-append-playlists
 (let ((mlt (kdenlive-mlt (f-full "test/kdenlive")))
       (producers
        `(((1 . 300) ,(f-full "image-in-folder-1.svg") 1)
          ((2 . 300) ,(f-full "image-in-folder-2.svg") 2)
          ((3 . 300) ,(f-full "not-in-a-folder.svg") nil))))
   (kdenlive-append-image-producers mlt producers))

 (let ((mlt (kdenlive-mlt (f-full "test/kdenlive")))
       (playlists '((1  "Audio 1" t) (2  "Video 1" nil))))
   (kdenlive-append-playlists mlt playlists)))

(comment ; kdenlive-mlt, kdenlive-append, kdenlive-profile, kdenlive-serialize
 (kdenlive-profile)
 (kdenlive-profile kdenlive-profile-hd-1080p-60fps)

 (setq kd-root (f-join default-directory "test"))
 (kdenlive-mlt kd-root)
 (kdenlive-mlt kd-root kdenlive-mlt-default)
 (kdenlive-append (dom-node 'mlt)
                  (kdenlive-profile '((width . "1920") (height . "1080"))))
 (kdenlive-serialize (dom-node 'mlt)) ; <?xml version='1.0 encoding='utf-8'?><mlt />
 (kdenlive-serialize
  (kdenlive-append (dom-node 'mlt)
                   (kdenlive-profile '((width . "1920") (height . "1080"))))) ; <?xml version='1.0 encoding='utf-8'?><mlt><profile width="1920" height="1080" /></mlt>
 (kdenlive-serialize
  (kdenlive-append (dom-node 'mlt)
                   (kdenlive-profile '((width . "1920") (height . "1080"))))
  t)

 (setq kd-root (f-join default-directory "test"))
 (kdenlive-serialize (kdenlive-append (kdenlive-mlt kd-root) (kdenlive-profile)))
 )

(comment ; kdenlive-folder-id
 (kdenlive-folder-id "folder-1" '("folder-1" "folder-2")) ; 1
 (kdenlive-folder-id "folder-2" '("folder-1" "folder-2")) ; 2
 (kdenlive-folder-id nil '("folder-1" "folder-2")) ; 2
 (--annotate (kdenlive-folder-id it '("folder-1" "folder-2"))
             '("folder-1" "folder-2")) ; ((1 . "folder-1") (2 . "folder-2"))
 )

(comment ; kdenlive-image-in-folder
 (kdenlive-image-in-folder 300
                           (f-full "test/image-in-folder-1.svg")
                           "folder-1") ; '(300 "/home/tony/work/settings/emacs.d/.emacs.d/packages/inside-emacs-tooling/kdenlive/test/image-in-folder-1.svg" "folder-1")
 (kdenlive-image-in-folder 300
                           (f-full "test/not-in-a-folder.svg")
                           nil) ; '(300 "/home/tony/work/settings/emacs.d/.emacs.d/packages/inside-emacs-tooling/kdenlive/test/not-in-a-folder.svg" nil)
 )

(comment ; kdenlive-skeleton-with-images, kdenlive-write
 (let* ((path (f-full "test/3-images-2-folders-no-timeline.kdenlive"))
        (root (f-full "test"))
        (folders '("folder-1" "folder-2"))
        (images
         `((300 ,(f-full "test/image-in-folder-1.svg") "folder-1")
           (300 ,(f-full "test/image-in-folder-2.svg") "folder-2")
           (300 ,(f-full "test/not-in-a-folder.svg") nil))))
   (setq mlt (kdenlive-skeleton-with-images root folders images))
   (unless (f-exists? (f-join default-directory "test"))
     (f-mkdir "test"))
   (kdenlive-write mlt path t))
 )

(comment ; kdenlive-write
 (setq kd-root (let ((test (f-join default-directory "test")))
                 (unless (f-exists? (f-join default-directory "test"))
                   (f-mkdir test))
                 test))
 (kdenlive-write (kdenlive-mlt kd-root)
                 (f-join kd-root "test-1.kdenlive"))
 (kdenlive-write (kdenlive-mlt kd-root)
                 (f-join kd-root "test-1-pretty.kdenlive")
                 t)
 (kdenlive-write
  (kdenlive-append (kdenlive-mlt kd-root kdenlive-mlt-default)
                   (kdenlive-profile kdenlive-profile-hd-1080p-60fps))
  (f-join kd-root "test-2-pretty.kdenlive")
  t)
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

(comment ; cddr, regexp-quote, s-blank?
 (cddr '(a u i e)) ; (i e)
 (cons '1 nil) ; (1)
 (cons nil '1) ; (nil . 1)
 (regexp-quote "\\") ; "\\\\"
 (s-blank? nil)
 )

(comment ; list, ->, --annotate, --map-indexed, -elem-index
 (list '(1 . 300) "foo" 1) ; ((1 . 300) "foo" 1)
 (-> '(2 3 5) (append '(8 13)) (-slice 1 -1)) ; (3 5 8)
 (--annotate (< 1 it) '(0 1 2 3)) ; ((nil . 0) (nil . 1) (t . 2) (t . 3))
 (--map-indexed (- it it-index) '(1 2 3 4)) ; (1 1 1 1)
 (-elem-index "bar" '("foo" "bar" "baz")) ; 1
 )

;;; Footer

(provide 'kdenlive)
