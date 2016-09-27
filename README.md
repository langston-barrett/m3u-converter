# m3u-converter

[![Build Status](https://travis-ci.org/siddharthist/m3u-converter.svg?branch=master)](https://travis-ci.org/siddharthist/m3u-converter)

This program generates nicely formatted, human and machine readable tracklists
from M3U files.

## Architecture

This program uses Parsec, a Haskell library for parsing well-defined grammars.
Similarly to Pandoc, it parses an M3U file to a Haskell datatype (aka internal
representation), and provides outputters that convert that datatype into various
text formats. Using this architecture, it's simple to write an outputter for any
new text format.

Done:
* Markdown (ordered)
* YAML

Not done:
* JSON
* HTML (ordered and unordered)
* Markdown (unordered)
* LaTeX

## Grammar
```
<m3u>          ::= '#EXTM3U\n' <maybeentries>
<maybeentries> ::= '' | <entry> '\n' <entries>
<entry>        ::= <maybeextinf> <path>
<maybeextinf>  ::= '' | <extinf> '\n'
<extinf>       ::= '#EXTINF:' <seconds> ',' <artist> '-' <title>
```

## Grammar Detail
```
<seconds>      ::= <digits>
<digits>       ::= <digit> <maybedigits>
<maybedigits>  ::= '' | <digits>
<digit>        ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

<artist>       ::= <string>
<title>        ::= <string>
<path>         ::= <string>
<string>       ::= \S+.*
```
(One or more non-whitespace characters, followed by anything else)

# TODO
* The artist always has a blank char at the end
* make M3U a toJSON object
