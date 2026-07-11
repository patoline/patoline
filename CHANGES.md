### 0.3 (2026-07-11)

This release is a fork of [patoline/patoline](https://github.com/patoline/patoline) @ `master` and
adds an aggressive OCaml 5 upgrade, additional layout/typography features, CJK + multi-language
hyphenation support, and a small bundled HTTP server.  The original `master` only contains the
2018 release notes (see below), so everything in this `0.3` section is **new** compared to the
upstream tree.

The file is organised in three blocks:

1. **Versus upstream `patoline/patoline` (`master`)** — what this fork changes vs. the
   original repository.
2. **`0.2 (2018-11-22)`** — copied verbatim from upstream for reference.
3. **`0.1 (2013-11-21)`** — copied verbatim from upstream for reference.

---

#### Versus upstream `patoline/patoline` (`master`)

##### Compiler / build system / toolchain

- **OCaml floor lifted to `5.2.0`**, ceiling `< 5.5.0`.  CI runs against `5.2.1` and `5.4.1`.
  Upstream targets OCaml `4.03.0..4.07.1` with Earley `2.0.0`; this fork is incompatible
  with OCaml 4.x.
- **`dune-project` bumped to `(lang dune 3.0)`** and `patoline.opam` regenerated; upstream
  still ships `(lang dune 2.7)`.
- **`patoline.opam` deps relaxed**: `camlzip`, `sqlite3`, `imagelib` are now declared with
  `(>= ...)`.  `ocaml` is bounded `(>= 5.0.0) & (< 5.5.0)`.  Adds `stdlib-shims`, `uucp`,
  `uunf` to `depends`.  Drops `lablgl` from `depopts` (it is still wired at the dune level
  via `(optional)` stanzas instead).
- **Opam metadata** gains `(tags "latex" "typesetting" "documentation" "typography")`.
- **`pa_ocaml` preprocessor no longer used at build time.**  Earley `parser` syntax in
  `pa_patoline/`, `unicodelib/`, `cesure/`, `patobuild/`, and the previous
  `patoline_ocaml/` is shipped already expanded to plain OCaml.  The `pa_ast.ml`,
  `pa_lexing.ml`, `pa_main.ml`, `pa_ocaml.ml`, `pa_ocaml_prelude.ml`, `pa_parser.ml`
  sources are vendored from `ocaml-earley 3.0.0` (CeCILL-B) and live inside `pa_patoline/`
  as a separate `pa_ocaml_lib` library.
- **`patoline_ocaml/` deleted**, sources merged into `pa_patoline/`.  The vendoring is
  documented in `pa_patoline/VENDORED_NOTES.md` and the dune comment block.
- **`pa_patoline/dune` split into three stanzas**: a `pa_ocaml_lib` library (vendored
  pa_ocaml sources), a `pa_patoline_support` library (`pa_patoline_ast_helpers.ml`,
  `(wrapped false)` so `open Pa_Patoline_ast_helpers` works), and the `pa_patoline`
  executable linking `pa_patoline`, `prefixTree`, `Subsup`.  Upstream has a single
  `(preprocess (action (run pa_ocaml …)))` stanza and a `pa_ocaml` library dep.
- **`patobuild/dune`** lists `build parallel patoline pragma serve` modules and depends on
  `pa_ocaml_lib` and `pa_patoline_support` instead of `pa_ocaml`/`patoline_ocaml`.
- **`.github/workflows/ci.yml` rewritten** on top of `actions/checkout@v4`,
  `ocaml/setup-ocaml@v2`, and `actions/cache@v4`.  Adds a `poppler-utils` step, an
  `examples/{minimal,hello_world,xkcd,genealogy}.txp` PDF regression that smoke-tests
  every produced PDF with `pdfinfo` / `pdftotext`, plus an `opam lint patoline.opam`
  step.  Upstream CI uses `actions/checkout@v2` / `actions/cache@v2` / `avsm/setup-ocaml@v1`
  and only pins + installs the package.

##### `pa_patoline` / parser layer

- **`pa_patoline_ast_helpers.ml` (new)** — the single place where `pa_patoline.ml` and
  `Subsup.ml` build Parsetree fragments: include-like modules, macro blocks, display
  math (`newPar` + `displayedFormula`), headings (`newStruct` / `go_up`), `write_main`,
  `wrap_document`, the `fun_` alias that points at `Pa_parser.fun_` (replacing
  `Exp.fun_` after the OCaml 5.2 API change), the title rule returning
  `[ str_let_any … ]`, and `maths_invisible` rewritten to call
  `Maths.node (Maths.glyphs "invisible")` inside `Maths.Normal`.
- **`print_math_deco`** no longer wraps `print_math_deco_finish` in a redundant
  `Pa_ast.exp_list`, avoiding `[[Maths.Ordinary …]]` and broken builds for
  `examples/hello_world.txp`.
- **`ldot_append_safe`** in `pa_ocaml.ml` lets `module_path_suit_aux` extend an `Lapply`
  accumulator as `Ldot(Lapply(l, r), x)`, fixing a long-standing grammar misparse of
  `module type of F(D).Output` constructs.
- **Helpers added** to keep the earley-generated grammars compatible with OCaml 5.2/5.4
  Parsetree: `longident_loc`, `string_loc`, `dot2`, `dot3`, `lid_wrap`,
  `ldot_append_safe`.  `loc_expr` / `loc_pat` / `loc_typ` populate `*_loc_stack = []`
  (the OCaml 5.0+ minimum).
- **`packages/Interactive.ml`**: `module type of DefaultFormat.Format(D) with module Output
  := DefaultFormat.Format(D).Output` now preprocesses to a correct ML form (previously
  the RHS path was truncated to just `Output`).

##### Typography / paragraph optimisation

- **`Break.Optimal` (Knuth-Plass DP backend)** added alongside the legacy best-first
  `Break.Make`.  Switch with `PATOLINE_BREAK_ALGO=optimal|legacy`.  The new module
  re-uses `Badness.compute_line_demerits`; the legacy best-first search is preserved
  and remains the default.
- **`Badness.ml` extended**:
  - New `Demerit` module with named constants (`consecutive_hyphen`, `hyphen`,
    `page_underfull`, `compression_jitter`, `glue_floor`, `glue_stretch`,
    `fitness_demerit_loose`, `fitness_demerit_very_loose`, `line_penalty`).
  - New `fit_class` type with **six** variants: `Very_tight | Tight | Normal | Loose |
    Very_loose | Overfull` (upstream had five: no `Overfull`).
  - New `rank : fit_class -> int`, `classify : float -> fit_class`,
    `line_demerits ~ratio ~penalty`, `fitness_demerits : fit_class -> float`,
    `_compute_line_demerits`.  These back the Knuth-Plass port.
  - `Badness.h_badness` now reads the demerit constants from `Demerit` instead of magic
    literals `10e6` / `20.`.
- **`Break.ml` extended** with the new `algorithm` ref (reads
  `PATOLINE_BREAK_ALGO`), and a `module Optimal` exposing `break_paragraph ~measure`.

##### Fonts / CJK

- **TTC face selection** in `patfonts/Opentype/Opentype.ml`: `.ttc` is added to the
  recognised extension list; `ttcf` (TrueType collection) fonts load the **first face**
  by default.
- **`typography/ConfigFindFont.legacy.ml`**: when `pat_to_name` returns an **absolute
  path** and the file exists, that path is used directly for font lookup.
- **WenQuanYi Zen Hei** is resolved through common Linux paths
  (`/usr/share/fonts/truetype/wqy`, `/usr/local/share/fonts/truetype/wqy`) to
  `wqy-zenhei.ttc`.
- **`formats/DefaultFormat.ml`** adds:
  - `wenQuanYiZenHei` family (`Regular/Bold/Caps`, italic and bold-italic faces via
    `Fonts.loadFont`).
  - `fallback_chain ()` returns `[wenQuanYiZenHei; alegreya]` when WQY is installed,
    else `[alegreya]` with a stderr hint pointing at `fonts-wqy-zenhei` /
    `--extra-fonts-dir`.
  - **CJK paragraph indentation** helpers `mostly_cjk`, `cjk_par_indent`, and
    `cjkParagraph` paragraph constructor (uses two full-width characters instead of the
    English 4·phi indent).
  - **GSUB-driven ligature substitution** through `gsub_ligatures` /
    `ligatures_with_fallback`; falls back to the historical hard-coded ligature list
    when the font has no GSUB or no ligature lookup.
  - **Punctuation spacing helpers** (`thin_space`, `punct_subst`, `french_subst`,
    `spanish_subst`, `german_subst`).
  - **MathML entry point** `mathml = Mathml.to_patoline` (uses new
    `typography/Mathml.ml`).
  - **Hyphenation language** ref `hyphen_lang` and `--hyphen-lang` CLI option.

##### Hyphenation

- **TeX hyphenation dictionaries vendored** under `hyphs/` for `cs`, `de-1901`, `de`,
  `es`, `fi`, `hu`, `it`, `nl`, `pl`, `pt`, `ru`, `sv` (in addition to the historical
  `en-us` and `fr`).  `hyphs/dune` adds `(rule …) + (install …)` entries for each
  language.  Some TeX files (`hyph-de.tex`, `hyph-de-1901.tex`, `hyph-pt.tex`,
  `hyph-ru.tex`) still contain constructs `cesure` cannot parse (`\message{…}`,
  terminal dashes, etc.); they are kept here for future preprocessing work.
- **Hyphenation CLI option** `--hyphen-lang <lang>` is forwarded to the produced
  binary via `patobuild/patoline.ml`.

##### `typography/Document.ml`

- **`gl_of_str` per-glyph fallback** across the `env.fontFamily` chain.  A `T` whose
  current font lacks a glyph walks down `fontFamily` until one matches, instead of
  failing with `Glyph_not_found`.  This handles missing-glyph transitions inside
  `[wenQuanYiZenHei; alegreya]` without an explicit font switch.
- **`nfkc`** is now backed by `UChar.nfkc_string` (using the `uucp` opam package)
  instead of an identity stub.
- **`makeGlue`** retains the historical Unicode-space -> glue mapping but now
  delegates to `UChar.is_space` / `UChar.is_break_opportunity` helpers (so
  break-opportunity logic actually fires).

##### `patobuild`

- **`patobuild/serve.ml` (new)** — minimal HTTP/1.1 static file server used by
  `patoline --serve`.  No external deps; serves `patobook.pdf` / `index.pdf` /
  directory contents.  Wired into `patobuild/dune`.
- **CLI additions in `patobuild/patoline.ml`**: `--serve`, `--port <i>`,
  `--root <dir>`, `--clean-cache`, `--rebuild`, `-o <file>` / `--output <file>`,
  `--hyphen-lang <lang>`, `--bin-args`, `--pp-args`, `--opt-args`.  Verbosity is now
  controlled by `--verbose <i>` (was `-v` upstream).
- **Hash-based incremental build cache**: `patobuild/build.ml` adds a `Hash` module,
  reads/writes `<build-dir>/cache.bin` (SHA-1 of inputs + patobuild binary mtime),
  and a `compile_targets` `up_to_date` / `mark_built` integration.  `--clean-cache`
  and `--rebuild` manage this cache.
- **`patobuild/parallel.ml`**: thread-safe `fprintf` / `printf` / `eprintf` helpers
  and a `TaskBag` work-stealing helper used by the rebuild loop.
- **`Thread.exit ()`** (deprecated in OCaml 5.4) is replaced by `raise Thread.Exit`
  in both `patobuild/parallel.ml` and `patobuild/build.ml`.

##### Drivers / Patonet

- **`drivers/SVG/HtmlFonts.ml`**: `Filename.temp_file "patfonts" ""` (whose signature
  changed in OCaml 5.x) is replaced by `Printf.sprintf "%s/patfonts_%d" …` building a
  unique temporary directory name.
- **`drivers/Patonet/dune`**, **`drivers/DriverGL/dune`**, **`drivers/DriverImage/dune`**:
  marked `(optional)` so they pick up `cryptokit` / `lablgl` when installed without
  breaking the build otherwise.
- **`drivers/DriverHarfbuzz/` (new)**, **`drivers/DriverPdfium/` (new)**, optional
  drivers added as `(optional)` stanzas.  Registered with `patoline drivers`.

##### Misc fixes / cleanups

- **`typography/Document.ml`**: the misplaced `[@@ocaml.deprecated …]` attribute on
  `up` is folded back into the `let` binding to match OCaml 5 attribute context rules.
- **`editors/emacs/unicode_scripts.ml`**: two unused values are renamed with a leading
  underscore to silence warning 32, which OCaml 5.4 promotes to error by default.
- **`packages/Giac.ml`**: `Pow`, `Inv`, `Opp`, `Ind` are implemented with plain OCaml
  + `Maths` constructors to avoid malformed structures from the `(<$ … $>)` expander
  on superscripts.
- **`patoraw/DynDriver.ml`** and **`drivers/DriverGL/DriverGL.ml`** use
  `Stdlib.stderr` instead of `Pervasives.stderr`.
- **`patobook/Extending.txp`** / **`patobook/FirstDocuments.txp`** content expanded.
- **`patobook/dune` (new)** orchestrates `patoline patobook.txp -o patobook.pdf`.
- **`patodb/patodb_mysql.ml`** marked `(optional)` in `patodb/dune`.
- **`unicodelib/UChar.ml`** / **`unicodelib/pa_convert.ml`** updated for OCaml 5
  compatibility (NFKC wired through `uucp`).
- **`patconfig/patDefault.ml`** exposes a `uucp`/`uunf`-aware default font config.

##### Build / install changes (selected)

- **`fonts/dune`**: drops `lmodern/`, adds a comment block explaining the
  `lmodern_fallback` in `typography/ConfigFindFont.legacy.ml`.  Latin Modern falls
  back to DejaVu (text) + Asana-Math (math).
- **`bibi/dune`**, **`patobook/dune`**, **`hyphs/dune`**, **`drivers/Pdf/dune`**,
  **`packages/dune`**, **`hyphs/dune`** updated for OCaml 5 / dune 3 compatibility.
- **`tests/` (new)**: `tests/dune`, `tests/test_lib.ml`, `tests/test_runner.ml`,
  `tests/baselines/hello_world.txt`, `tests/test_break_optimal.txp`,
  `tests/test_cjk_mixed.txp`, `tests/test_ttc_faces.txp` (smoke tests for the new
  Knuth-Plass DP, CJK fallback chain, and TTC face selection).
- **`typography/Mathml.ml` (new)**: minimal MathML -> Patoline translator used by
  `DefaultFormat.mathml`.

##### User action

```bash
opam switch create 5.4.1
eval $(opam env)
opam install dune earley.3.0.0 stdlib-shims camlzip sqlite3 imagelib uucp uunf
# Optional, depending on which drivers you want
opam install cairo2 cryptokit lablgl mysql
git clone https://github.com/mobtgzhang/patoline
cd patoline
dune build && dune install
```

No user-visible migration is required beyond a clean rebuild against a 5.2+ switch.

---

### 0.2 (2018-11-22)

First reasonable release of Patoline, although it is still hard to use.
Here are some of the latest changes:
 - Patoline is now built using dune,
 - the package structure has been cleaned,
 - GNU make wizardry has disappeared,
 - libraries are wrapped (use Typography.Document, Patoraw.RawContents, ...),
 - many things have been cleaned.

### 0.1 (2013-11-21)

First release of Patoline by Pierre-Étienne Meunier.