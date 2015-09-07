# stb_image.erl

Erlang interface to [stb_image.h](https://github.com/nothings/stb/blob/master/stb_image.h).

# Install

Add this to your [rebar](https://github.com/rebar/rebar/wiki/Getting-started) deps:
```erlang
{stb_image, ".*", {git, "git@github.com:eddieantonio/stb_image.erl.git", "v0.1.0"}}
```

# Usage

In its simplest form, load an image data from a filename like so:

```erlang
{ok, {X, Y, NChannels, Data}} = stb_image:load("image.jpg").
```

`Data` is a binary as returned by `stb_image`. Image data is in row-major
form, with channels interleaved. That is, if the data is returned with
3 channels (RGB), then the first pixel's components are given by:
`<<R, G, B, _/binary>>`. Similarly, if the number of channels is 2, the
first pixel is given as `<<Intensity, Alpha, _/binary>>`.

## Specifying request number of components

You may also specify how many channels should be returned using the
`stb_image:load/2`. The requested depth may be a number between 1 and 4,
or `gray`, `gray_alpha`, `rgb`, `rgb_alpha`, or `default`. This
corresponds to the `req_comp` argument of
[`stbi_load()`](https://github.com/eddieantonio/stb_image.erl/blob/master/c_src/stb_image.h#L227).

```erlang
{ok, {X, Y, 3, Data}} = stb_image:load("image.bmp", rgb),
{ok, {X, Y, 4, DataWithAlpha}} = stb_image:load("image.png", rgb_alpha).
```

# License

`stb_image.h` is public domain. `stb_image.erl` is Unlicensed. Use as
you wish.
