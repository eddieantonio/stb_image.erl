# stb_image.erl

[![Build Status](https://travis-ci.org/eddieantonio/stb_image.erl.svg?branch=master)](https://travis-ci.org/eddieantonio/stb_image.erl)

Erlang interface to [stb_image.h](https://github.com/nothings/stb/blob/master/stb_image.h).

# Install

Add this to your [rebar][] deps:
```erlang
{stb_image, ".*", {git, "git@github.com:eddieantonio/stb_image.erl.git", "v0.2.0"}}
```

[rebar]: https://github.com/rebar/rebar/wiki/Getting-started

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

## Requesting a specific number of components

You may also specify how many channels should be returned using
`stb_image:load/2`. The requested depth may be a number between 1 and 4,
or `gray`, `gray_alpha`, `rgb`, `rgb_alpha`, or `default`. This
corresponds to the `req_comp` argument of
[`stbi_load()`][stbi_load]

```erlang
{ok, {X, Y, 3, Data}} = stb_image:load("image.bmp", rgb),
{ok, {X, Y, 4, DataWithAlpha}} = stb_image:load("image.png", rgb_alpha).
```

## Errors

As in standard Erlang fashion, when a successful return is given by
`{ok, Result}`, an unsuccessful return is given by `{error, Reason}`. In
`stb_image`, this reason is often an atom representing a [POSIX
errno][errno]. For example, when the file does not exist, `{error,
enoent}` ("No (file) entry") is returned. However, due to the fact that
the underlying error mechanisms are **NOT thread-safe** `Reason` may
**NOT** correspond to the true reason; hence use `Reason` with caution.
That said, the fact that an error occurred is always deterministic, and
thus reliable. Hence, it is perfect find to do this:

```erlang
load_image(Name) ->
   case stb_image:load(Name, rgb_alpha) of
      {ok, Image} ->
         Image;
      {error, _} ->
         exit("Could not load image")
   end.
```

However **DO NOT** rely on this:

```erlang
load_image(Name) ->
   case stb_image:load(Name, rgb_alpha) of
      {ok, Image} ->
         Image;
      {error, enoent} ->
         exit("Could not find the image");
      {error, eacces} ->
         exit("Could not access the image");
   end.
````


[stbi_load]: https://github.com/eddieantonio/stb_image.erl/blob/master/c_src/stb_image.h#L227.
[errno]: http://man7.org/linux/man-pages/man3/errno.3.html

# License

`stb_image.h` is public domain. `stb_image.erl` is Unlicensed. Use as
you wish.
