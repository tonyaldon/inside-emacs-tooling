# About

In `inside-emacs-tooling`, you'll find `emacs-lisp` tooling
I use to make [Inside Emacs](https://www.youtube.com/channel/UCQCrbWOFRmFYqoeou0Qv3Kg)
video series.

# Motivation

1. Automate boring and repetitive parts of making the videos,
2. Drastically cut down the time to produce the videos,
3. Trying to produce each video in less than 2 hours (it currently
   takes me 8 to 10 hours)

# Stories

A boring part but really important when making Inside Emacs videos
is to transform each video story (written in a specific formated text
file) to beautiful footage.  So far it involves 3 steps:
1. parse the story (specific formated text),
2. generate `.svg` files (the story) from the specific
   formated text,
3. generate `.kdenlive` files with the previous `.svg` files.

The parsing part is done in
[ie-story-parse](./ie-story/ie-story-parse.el),
and the generating part is done in
[ie-story-generate](./ie-story/ie-story-generate.el).

Note that the generating part of `.kdenlive` files is not already done.

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

To generate the `.svg` files corresponding to a story written
following the formated text described above, inside emacs, in the same
buffer of the story is written, run the command
`ie-story-generate-all-descriptions-svg`, for instance like this:

```
M-x ie-story-generate-all-descriptions-svg
```

The `.svg` files are saved in the subdirectory `r-images` relative to
the default directory your story is saved.

Play with
[ie-story-example-1](./ie-story/examples/example-1/ie-story-example-1.org)
to see how `ie-story-generate-all-descriptions-svg` behaves.

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

[kdenlive.el](./kdenlive/kdenlive.el) implements `emacs` commands that
can generate `.kdenlive` files.

I know that `kdenlive` is already at version `20.12`, but `kdenlive.el`
generate `.kdenlive` files compatible with the version `17.12.3`.
See the top comments in the [kdenlive.el](./kdenlive/kdenlive.el)
file for more details.

# License

Project under MIT license
