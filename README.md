# About

In `inside-emacs-tooling`, you'll find `emacs-lisp` tooling
I use to make the video series [Inside Emacs](https://www.youtube.com/channel/UCQCrbWOFRmFYqoeou0Qv3Kg).

# Motivation

1. Automate boring and repetitive parts of making the videos,
2. Drastically cut down the time to produce the video,
3. Trying to produce each video in less than 2 hours (it currently
   takes me 8 to 10 hours)

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
