# Quaviver

A portable and extensible floating point base conversion and string library.

## Introduction

Quaviver is a collection of Common Lisp systems that implement various
algorithms for changing the base (or radix) of floating point numbers
and for parsing and writing numbers. It includes many utility
functions for reading and writing numerical digits, performing
arithmetic on floating point significands and binary encoding of
floating point numbers into formats like [binary32][]. Quaviver also
includes a high-level interface to reading, writing and parsing
numbers in various formats.

## QUAVIVER/STREAM system and package

The QUAVIVER/STREAM package includes three different functions for
parsing, reading and writing numbers. Each one of these functions take
several keyword arguments which control the serialization method. All
three functions include a keyword argument :STYLE which selects the
overall number format as per the following table.

| Value            | Description                                           |
|------------------|-------------------------------------------------------|
| :BLUB            | Generic integers and floating point                   |
| :C89             | C89 Standard integers and floating point              |
| :C99             | C99 Standard integers and floating point              |
| :C11             | C11 Standard integers and floating point              |
| :C17             | C17 Standard integers and floating point              |
| :C23             | C23 Standard integers and floating point              |
| :C++98           | C++98 Standard integers and floating point            |
| :C++03           | C++03 Standard integers and floating point            |
| :C++11           | C++11 Standard integers and floating point            |
| :C++14           | C++14 Standard integers and floating point            |
| :C++17           | C++17 Standard integers and floating point            |
| :C++20           | C++20 Standard integers and floating point            |
| :C++23           | C++23 Standard integers and floating point            |
| :C++26           | C++26 Standard integers and floating point            |
| :COMMON-LISP     | Common Lisp ratios, integers and floating point       |
| :COMMON-LISP/CCL | Common Lisp including CCL infinity and NaN extensions |
| :FORTRAN         | Fortran integers and floating point                   |
| :JSON            | JSON integers and floating point                      |
| :PYTHON          | Python integers and floating point                    |

### parse-number

```common-lisp
(parse-number string
              &key (start 0) (end (length string)) (base 10) junk-allowed
                   (integer t) (ratio t) (float t)
                   float-type (style :common-lisp)
                   (whitespace '(#\space #\tab #\page #\newline #\return)))
;; => number, position
```

Parse a number from STRING. START and END specify a substring of STRING. 

The initial number base is determined by BASE. Some values of STYLE
allow number prefixes that can change the default base. For example,
the C styles will parse hexadecimal numbers via the prefix `0x` if
that is permitted by the specific standard.

JUNK-ALLOWED determines if trailing non-whitespace is permitted.

INTEGER, RATIO, and FLOAT are booleans that control whether to read
integers, fractions or floating point numbers. By default all of these
are non-NIL so if the specific style supports it than any one of these
numbers will be parsed.

The default floating type is determined by FLOAT-TYPE. Some styles,
for example Common Lisp, have the ability to specify the floating
point type using the exponent marker or a type suffix.

WHITESPACE is a character bag or a predicate function that is used to
skip over whitespace before the number. It is also used to determine
if there is trailing junk after the number.

NUMBER is the result of parsing.

POSITION the index to the end of the number in STRING.

#### Examples

```
CL-USER> (asdf:load-system "quaviver/stream")
          
T
CL-USER> (quaviver/stream:parse-number "  3.1415926535897932384626433832795028841971d0")
3.141592653589793d0
46
CL-USER> (quaviver/stream:parse-number "  314")
314
5
CL-USER> (quaviver/stream:parse-number "  0xfa" :style :c99)
250
6
CL-USER> (quaviver/stream:parse-number "  0xfa.e10" :style :c99)
; Evaluation aborted on #<QUAVIVER.CONDITION:INVALID-CHARACTER-ERROR {1002EF2FA3}>.
CL-USER> (quaviver/stream:parse-number "  0xfa.e10" :style :c++20)
700.0d0
10
```

### read-number

```common-lisp
(read-number stream
             &key (base 10) junk-allowed
                  (integer t) (ratio t) (float t)
                  float-type (style :common-lisp)
                  (whitespace '(#\space #\tab #\page #\newline #\return)))
;; => number
```

READ-NUMBER is identical to PARSE-NUMBER, except that it reads the
number from a stream versus a string.

### write-number

```common-lisp
(write-number value stream
              &key (base 10) (style :common-lisp))
```

WRITE-NUMBER writes VALUE to STREAM. BASE and STYLE have the same
meanings as in PARSE-NUMBER.

[binary32]: https://en.wikipedia.org/wiki/Single-precision_floating-point_format
