# Generate svg files from the story

To generate the `.svg` files corresponding to the story written
in the file [/ie-story-example-1.org](./ie-story-example-1.org),
visit that file in an `emacs` buffer and run the command
`ie-story-generate-all-descriptions-svg`, for instance like this:

```
M-x ie-story-generate-all-descriptions-svg
```

The `.svg` files are saved in the subdirectory
defined by `ie-story-generate-images-dir` variable (which is by
default `"r-images"`) relative to the `default-directory` the story
[/ie-story-example-1.org](./ie-story-example-1.org) is saved in.
