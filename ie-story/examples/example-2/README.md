# Generate kdenlive files from the story

To generate the `.kdenlive` files corresponding to the story written
in the file [/ie-story-example-2.org](./ie-story-example-2.org),
visit that file in an `emacs` buffer and run the command
`ie-story-generate-all-edited-scenes-kdenlive`, for instance like
this:

```
M-x ie-story-generate-all-edited-scenes-kdenlive
```

The `.kdenlive` files are saved in the subdirectory
defined by `ie-story-generate-kdenlive-dir` variable (which is by
default `"kdenlive"`) relative to the `default-directory` the story
[/ie-story-example-2.org](./ie-story-example-2.org) is saved in.
