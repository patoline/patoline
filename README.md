Patoline - A modern digital typesetting system
==============================================

This repository contains the source code of Patoline, which homepage is hosted at
https://patoline.github.io.

This is the **2026 OCaml 5 fork** maintained at
https://github.com/mobtgzhang/patoline.  The upstream project lives at
https://github.com/patoline/patoline.  See `CHANGES.md` for the full list of
changes versus upstream.

Patoline is a typesetting system built around an OCaml-embedded document
description language.  It targets modern OCaml (5.2 and newer, with 5.4.1 as the
principal supported compiler), Earley 3.0, and Dune 3.0+.

---

## Dependencies

The minimal dependencies are:

- OCaml (5.2.0 or newer, with 5.4.1 as the principal target)
- Opam (2.5.0 or higher recommended)
- Dune (3.0.0 or higher)
- Ocamlfind
- Earley (3.0.0)
- stdlib-shims
- Camlzip
- Sqlite3
- Imagelib
- uucp, uunf (Unicode normalisation, used by `unicodelib`)
- GNU make

To set up a suitable OCaml environment, the simplest possible solution is to
use the Opam package manager.  It can be installed by following instructions at
http://opam.ocaml.org/doc/Install.html.  You can then install an OCaml compiler
and the required libraries as follows:

```bash
opam switch create 5.4.1
eval $(opam env)
opam install dune earley.3.0.0 stdlib-shims camlzip sqlite3 imagelib uucp uunf
```

You can optionally install more Opam packages (OCaml libraries) depending on the
Patoline drivers that you want to use.  For instance, you will need to install
`cairo2` for the Cairo driver, `cryptokit` for the `Patonet` driver, `lablgl`
for the OpenGL driver, `mysql` for the MySQL patodb backend, and either
`libharfbuzz-dev` / `libpdfium-dev` (Debian/Ubuntu system packages) for the
optional Harfbuzz and PDFium drivers.

> **Note (earley 3.0):** Upstream earley 3.0.0 removed the `earley.ocaml`
> library from Opam; this tree ships the equivalent in `pa_patoline/` (using
> `compiler-libs.common`).  The earley `parser` syntax in `pa_patoline/`,
> `unicodelib/`, `cesure/`, and `patobuild/` is checked in **already expanded**
> to plain OCaml, so `dune build` does not invoke the `pa_ocaml` preprocessor.
> To edit the original BNF-style sources, install earley, run `pa_ocaml` on the
> file, and replace the corresponding `.ml`.

---

## Compilation from source

Patoline can be compiled from source as follows:

```bash
git clone https://github.com/mobtgzhang/patoline.git
cd patoline
dune build
dune install
```

(`make && make install` still works as an alias for the two `dune` commands.)

After the build, `patoline drivers` lists every backend that compiled
successfully.  `patoline --serve` will start a small HTTP/1.1 server (no extra
dependencies) and serve the freshly-built PDF / current directory on
`http://localhost:8080/`.

---

## Optional drivers and backends

Patoline ships several optional drivers and storage backends that depend on
third-party packages.  They are wired as dune `(optional)` stanzas, so the
build works whether or not the package is installed:

| Component               | Optional dep               | Enable with                  |
|-------------------------|----------------------------|------------------------------|
| `DriverCairo`           | `cairo2`                   | `opam install cairo2`        |
| `Patonet`               | `cryptokit`                | `opam install cryptokit`     |
| `DriverGL`              | `lablgl lablgl.glut`       | `opam install lablgl`        |
| `DriverImage`           | `DriverGL` (→ `lablgl`)    | `opam install lablgl`        |
| `DriverHarfbuzz`        | `libharfbuzz-dev`          | system package + `opam install conf-pkg-config` |
| `DriverPdfium`          | `libpdfium-dev`            | system package + `opam install conf-pkg-config` |
| `patodb.memory`         | —                          | always built                 |
| `patodb.sqlite3`        | `sqlite3`                  | `opam install sqlite3`       |
| `patodb.mysql`          | `mysql`                    | `opam install mysql`         |

To pull in every optional backend at once:

```bash
opam install cairo2 cryptokit lablgl mysql
opam install patoline   # picks up the new deps
dune build              # rebuilds the optional stanzas
```

---

## OCaml 5 upgrade notes (2026)

- **Floor**: `ocaml (>= 5.2.0)`.
- **Build system**: `dune-project` is bumped to `(lang dune 3.0)`;
  `(generate_opam_files true)` is retained.  `camlzip`, `sqlite3`, `imagelib`
  are now declared with `(>= ...)` instead of hard pins.
- **CI**: `.github/workflows/ci.yml` is rewritten on top of
  `actions/checkout@v4`, `ocaml/setup-ocaml@v2`, and `actions/cache@v4`.  Each
  matrix entry runs `opam lint patoline.opam` and a regression on
  `examples/{minimal,hello_world,xkcd,genealogy}.txp`.
- **`pa_patoline/` Parsetree layer**: the AST helpers were adapted to the 5.2/5.4
  Parsetree shape:
  - `Pexp_function` replaces the removed `Pexp_fun` (5.2+).
  - `Pexp_constant` is now a record `{ pconst_desc; pconst_loc }`.
  - `Pexp_tuple` / `Ppat_tuple` accept `(string option * _) list`.
  - `Longident.Ldot` / `Longident.Lapply` carry per-part `with_loc` payloads in
    5.4, and `Typ.package` wraps its payload as a `package_type`.  New helpers
    (`longident_loc`, `string_loc`, `dot2`, `dot3`, `lid_wrap`,
    `ldot_append_safe`) keep the earley-generated grammars compatible.
  - `loc_expr` / `loc_pat` / `loc_typ` populate `*_loc_stack = []` (the
    OCaml 5.0+ minimum).
- **Optional drivers stay**: `DriverGL` and `DriverImage` (built on `lablgl`)
  and `patodb.mysql` (built on `mysql`) are kept in `dune-project` /
  `patoline.opam` and compile against OCaml 5.x once their upstream libraries
  are available.  They are wired as `(optional)` stanzas in
  `drivers/DriverGL/dune`, `drivers/DriverImage/dune`, and `patodb/dune`,
  with `lablgl` and `mysql` added back to `libraries` so that they opt in via
  opam.  `DriverHarfbuzz` and `DriverPdfium` join them in the same style.
- **Opam metadata**: `(depends (ocaml (>= 5.2.0)))` is set, and
  `(tags "latex" "typesetting" "documentation" "typography")` is added to ease
  discovery in the opam index.

### User action

```bash
opam upgrade patoline           # or rebuild from a fresh switch
opam install cairo2 cryptokit   # still-supported depopts
```

No user-visible migration is required beyond a clean rebuild against a 5.2+
switch.

### Upgrading from an older OCaml switch

If you were on Patoline before the 2026 OCaml 5 upgrade, drop your old switch
and recreate it on 5.4.1:

```bash
opam switch remove patoline-old
opam switch create 5.4.1
eval $(opam env)
opam install patoline
```

---

## First Patoline document

Here is a "hello, world" Patoline document (more examples live in the
`examples/` folder).

```
======================
Hello, World!
----------------------
Patoline newbies
======================

-> Section with text

This is my //first// document! That's so **cool**!

-<

-> Section with maths

Did you know that $(a + b)² = a² + 2ab + b²$?

-<
```

If you save this as `hello_world.txp`, you can compile it with
`patoline hello_world.txp`.  This produces a PDF file called `hello_world.pdf`.
Note that you can target other format drivers such as the SVG/HTML driver or
the OpenGL driver; pick a driver with `patoline --driver DRIVER hello_world.txp`.
The list of available drivers is available via `patoline drivers` (some may not
be installed).

Other useful flags:

- `--serve --port 8080` to start an HTTP server for the generated document
  (defaults to serving `patobook.pdf`, `index.pdf`, or the current directory).
- `--hyphen-lang <lang>` to switch hyphenation language at compile time
  (one of `en-us`, `fr`, `de`, `de-1901`, `es`, `it`, `nl`, `pl`, `pt`, `ru`,
  `cs`, `sv`, `fi`, `hu`).
- `--extra-fonts-dir <dir>` / `--extra-hyph-dir <dir>` to point at additional
  fonts / hyphenation dictionaries.
- `-o <file>` / `--output <file>` to set the output path.
- `-j <n>` for parallel compilation.

To switch the paragraph-breaking algorithm at run time, set the environment
variable `PATOLINE_BREAK_ALGO=optimal` (Knuth-Plass DP) or
`PATOLINE_BREAK_ALGO=legacy` (best-first, the default).

---

## List of developers and contributors

Main developers and contributors:

- Pierre-Étienne Meunier
- Christophe Raffalli
- Rodolphe Lepigre
- Tom Hirschowitz
- Florian Hatat
- Pierre Hyvernat
- Guillaume Theyssier
- Vincent Laporte
- Alan Schmitt

The 2026 OCaml 5 fork and the layout / CJK / hyphenation improvements in this
repository are maintained by `mobtgzhang` and contributors.

---

## License

GPL-2.0.  See `LICENSE`.