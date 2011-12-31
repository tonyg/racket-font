# First-principles font rendering for Racket

Over in
[gst-cairo-widgets](https://github.com/tonyg/gst-cairo-widgets), I've
been experimenting with the-simplest-thing-that-could-possibly-work
for font rendering. A small program,
[chew.c](https://github.com/tonyg/gst-cairo-widgets/blob/master/chew.c),
uses FreeType 2 to extract metrics and glyph outlines from system
fonts (TrueType, Postscript, etc.) and writes the resulting
information using a simple text-based format.

`font.rkt` here reads fonts encoded in such a format. Two example
typefaces are provided, `gentium-basic-italic.gz` and
`gentium-basic-regular.gz` (see
[here](http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=gentium)
for information about the Gentium typeface family).

`font-directory.rkt` provides a simple searchable font directory. Font
faces can be loaded into the directory at runtime.

`font-render.rkt` uses a `dc<%>` to render individual glyphs and
strings.

## The example

`example.rkt` draws a simple string to a Racket GUI window. It
produces output something like this:

<img src="http://github.com/tonyg/racket-font/raw/master/example.png" alt="Output of example.rkt"/>

## Copyright and License

### The code

`racket-font` is [open-source](http://www.opensource.org/) software,
licensed under the very liberal [MIT
license](http://www.opensource.org/licenses/mit-license.php):

    Copyright (c) 2011 Tony Garnock-Jones <tonygarnockjones@gmail.com>

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

### The fonts

The included Gentium fonts are Copyright (c) 2003-2008 SIL
International (<http://www.sil.org/>), with Reserved Font Names
"Gentium" and "SIL". They are licensed under the [SIL Open Font
License, Version 1.1](http://scripts.sil.org/OFL).
