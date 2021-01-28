# Generate both svg and kdenlive files the story

To generate the both `.svg` and `.kdenlive` files corresponding to the
story written in the file [/ie-story-example-3.org](./ie-story-example-3.org),
visit that file in an `emacs` buffer and run the command
`ie-story-generate-all`, for instance like this:

```
M-x ie-story-generate-all
```

The `.svg` files are saved in the subdirectory
defined by `ie-story-generate-images-dir` variable (which is by
default `"r-images"`) relative to the `default-directory` the story
[/ie-story-example-3.org](./ie-story-example-3.org) is saved in.

The `.kdenlive` files are saved in the subdirectory
defined by `ie-story-generate-kdenlive-dir` variable (which is by
default `"kdenlive"`) relative to the `default-directory` the story
[/ie-story-example-3.org](./ie-story-example-3.org) is saved in.
