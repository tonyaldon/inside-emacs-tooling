# About

In `inside-emacs-tooling`, you'll find `emacs-lisp` tooling
I use to make [Inside Emacs](https://www.youtube.com/channel/UCQCrbWOFRmFYqoeou0Qv3Kg)
video series.

# Motivation

1. Automate boring and repetitive parts of making the videos,
2. Drastically cut down the time to produce the videos,
3. Trying to produce each video in less than 2 hours (it currently
   takes me 8 to 10 hours [Inside Emacs #6 (part 5)](https://www.youtube.com/watch?v=w4wxGOijyZs)
   / and more than 20 hours for the first one [Inside Emacs #1](https://www.youtube.com/watch?v=F1IXixEhQwk)).

# Stories

A boring part but really important when making Inside Emacs videos
is to transform each video story (written in a specific formated text
file) to beautiful footage.  So far it involves 3 steps:
1. parse the story (specific formated text),
2. generate `.svg` files (the story) from the specific
   formated text,
3. generate `.kdenlive` files with the previous `.svg` files.

The parsing part is done in
[ie-story-parse.el](./ie-story/ie-story-parse.el),
and the generating part is done in
[ie-story-generate.el](./ie-story/ie-story-generate.el).

## Formated story

The stories of Inside Emacs videos are written following
the format bellow (see also
[ie-story-template.org](./ie-story/ie-story-template.org) file):

```text
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
for the viewer
```

The Inside Emacs stories must follow the following rules:
1. The descriptions belong to subsections like `** scene 1: scene title`
   which belongs to the main section `* scenes`.
2. Each paragraph of the story must start in the line following
  `# description` line and end up at the first empty line.

## Usage

### Generate svg files from a story

To generate the `.svg` files corresponding to a story written
following the formated text described above, inside emacs, in the same
buffer of the story is written, run the command
`ie-story-generate-all-descriptions-svg`, for instance like this:

```
M-x ie-story-generate-all-descriptions-svg
```

The `.svg` files are saved in the subdirectory,
defined by `ie-story-generate-images-dir` variable (which is by
default `"r-images"`), relative to the `default-directory` your story
is saved in.

Play with
[ie-story-example-1](./ie-story/examples/example-1/ie-story-example-1.org)
to see how `ie-story-generate-all-descriptions-svg` behaves.

### Generate kdenlive files from a story

To generate the `.kdenlive` files corresponding to a story written
following the formated text described above, inside emacs, in the same
buffer of the story is written, run the command
`ie-story-generate-all-edited-scenes-kdenlive`, for instance like this:

```
M-x ie-story-generate-all-edited-scenes-kdenlive
```

The `.kdenlive` files are saved in the subdirectory,
defined by `ie-story-generate-kdenlive-dir` variable (which is by
default `"kdenlive"`), relative to the `default-directory` your story
is saved in.

Play with
[ie-story-example-2](./ie-story/examples/example-2/ie-story-example-2.org)
to see how `ie-story-generate-all-edited-scenes-kdenlive` behaves.

# Kdenlive

I'm using [kdenlive](https://kdenlive.org/en/) to edit Inside Emacs
videos.

After [Inside Emacs #6 (part 5)](https://www.youtube.com/watch?v=w4wxGOijyZs),
I decided to automate the `kdenlive` editing part of the videos with `emacs`.

When you work with GUI `kdenlive`, anything you do (with the mouse)
and you save is saved in a `.kdenlive` file
([kdenlive project file format](https://kdenlive.org/en/project/kdenlive-project-file-format/))
that is a `xml` file that follows the [mlt xml format](https://www.mltframework.org/docs/mltxml/)
with some custom xml attributes namespaced by `kdenlive:`.

So, the idea is to generate the `.kdenlive` files for each new videos
with the common parts and settings.  And then, working with GUI
`kdenlive` to end parts that can't be automated.

[kdenlive.el](./kdenlive/kdenlive.el) implements `emacs` functions that
can generate `.kdenlive` files.

I know that `kdenlive` is already at version `20.12`, but `kdenlive.el`
generate `.kdenlive` files compatible with the version `17.12.3`.
See the top comments in the [kdenlive.el](./kdenlive/kdenlive.el)
file for more details.

## Usage

### example 1

Go to [kdenlive example-1](./kdenlive/examples/example-1/example-1.el)
to see how `kdenlive-...` functions are put together to produce
a simple `.kdenlive` file.

### example 2

You can find all we describe here in [kdenlive
example-2](./kdenlive/examples/example-2/example-2.el) file.

Our goal is to generate a `.kdenlive` project file named
`kdenlive/3-images-2-folders-no-timeline.kdenlive` verifying those
conditions:
1. the kdenlive profile of the video is `HD 1080p 60 fps`,
2. it contains the following `svg` images `image-in-folder-1.svg`,
   `image-in-folder-2.svg`, `not-in-a-folder.svg`,
3. and in GUI kdenlive:
   - `image-in-folder-1.svg` belongs to `folder-1` folder,
   - `image-in-folder-2.svg` belongs to `folder-2` folder,
   - `not-in-a-folder.svg` belongs to any specific folders.

To do so, we just need to use the `kdenlive-skeleton-with-images` and
`kdenlive-write` functions defined in
[kdenlive.el](./kdenlive/kdenlive.el).

Assuming your `svg` images are in the subdirectory `r-images` (stands
for raw images) and your code is in the file `example-2.el`, running
the following commands in your terminal:

```bash
cd path/to/example-2/
tree
```

you should see:

```
.
├── example-2.el
└── r-images
    ├── image-in-folder-1.svg
    ├── image-in-folder-2.svg
    └── not-in-a-folder.svg
```

Now in `example-2.el`, copy/past the following code:

```elisp
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

```

Now switch to a buffer visiting `example-2.el` and evaluate the buffer
with `eval-buffer`, for instance running:

```
M-x eval-buffer
```

This creates the desired `.kdenlive` project file.  Now you can
open it inside `kdenlive` and edit the video with GUI kdenlive.

In your terminal, if you run `tree`, you should see:

```
.
├── example-2.el
├── kdenlive
│   └── 3-images-2-folders-no-timeline.kdenlive
└── r-images
    ├── image-in-folder-1.svg
    ├── image-in-folder-2.svg
    └── not-in-a-folder.svg
```

# Last video

When I'm working on a video, I often have to visit its corresponding
README.  The video being edited/produced is refered to the `last
video`.

To visit this `last video` you can run the command
`ie-last-video-find-readme` defined in
[ie-last-video.el](./ie-last-video.el) , for instance by running:

```
M-x ie-last-video-find-readme
```

This command depends on:
1. the variable `ie-last-video-main-dir` that store the path where
   Inside Emacs videos are edited,
2. the naming of the directory of each video:
   - `inside-emacs-1` for Inside Emacs #1 video and,
   - `inside-emacs-6-part-5` for Inside Emacs #6 (part 5) video.

# License

Project under MIT license
