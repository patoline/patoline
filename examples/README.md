Examples of Patoline documents
==============================

This folder contains several simple examples of Patoline documents. They are
intended to demonstrate basic functionalities while also give examples of
advanced features. The examples can be compiled with commands such as the
following.

```bash
patoline minimal.txp
patoline --driver DriverCairo hello_world.txp
patoline --driver DriverGL xkcd.txp
patoline genealogy.txp
patoline bilingual_zh_en.txp
```

`bilingual_zh_en.txp` mixes Chinese, English, and Korean (no math). Patoline’s
default body font is **WenQuanYi Zen Hei** when the font is
found (e.g. Debian/Ubuntu package `fonts-wqy-zenhei`); otherwise it falls back
to Alegreya and CJK will not show.

**Important when developing Patoline from a git checkout:** compiling a `.txp`
uses ocamlfind’s `patoline.format.DefaultFormat`. If you change the format or
fonts in the source tree, run `dune install --prefix DIR` (then put
`DIR/lib` first in `OCAMLPATH`, or call `DIR/bin/patoline`) or `opam install .`
so that `patoline` links against your **local** libraries—not an older copy
from opam. Otherwise CJK fixes in `DefaultFormat` will not apply.

You can still add `--extra-fonts-dir` with a directory containing
`wqy-zenhei.ttc` if the font is not in the usual system paths.
