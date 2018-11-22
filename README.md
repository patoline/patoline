Patoline - A modern digital typesetting system
==============================================

This repository contains the source code of Patoline, which homepage is hosted
at https://patoline.github.io.

### Dependencies

The minimal dependencies are
 - OCaml (version 4.03.0 or higher)
 - Opam
 - Dune
 - Ocamlfind
 - Earley (version 2.0.0 or higher)
 - Camlzip
 - Sqlite3
 - Imagelib
 - GNU make

To setup a suitable OCaml environment, the simplest possible solution is to
use the Opam package manager. It can be installed by following instructions
at http://opam.ocaml.org/doc/Install.html. You can then install an OCaml
compiler and the required libraries as follows.

```bash
opam switch 4.05.0
eval $(opam env)
opam install dune earley.2.0.0 camlzip sqlite3 imagelib
```

You can optionally install more Opam packages (OCaml libraries) depending
on the Patoline drivers that you want to use. For instance, you will need
to install ``lablgl`` for our OpenGL driver, ``cairo2`` for our Cairo
driver, ``kryptokit`` for our ``Patonet`` driver.

### Compilation from source

Patoline can be compiled from source as follows:

```bash
wget https://github.com/patoline/patoline/archive/master.zip
unzip master.zip
cd patoline-master
make
make install
```

### First Patoline document

Here is a hello world patoline document (other examples can be found in the
`examples` folder).

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

Did you known that $(a + b)² = a² + 2ab + b²$?

-<
```
If you copy-paste it in a file ``hello_world.txp``, then you can compile it
with the command ``patoline hello_world.txp``. This will produce a PDF file
called ``hello_world.pdf``. Note that you can compile to other format drivers
such as a webpage using SVG format or an OpenGL window. To do that, select
a driver with the command ``patoline --driver DRIVER hello_world.txp``. You
can obtain the list of the existing drivers with ``patoline drivers`` (they
may not all be installed).

### List of developpers and contributors

Main developpers and contributors:
 - Pierre-Étienne Meunier
 - Christophe Raffalli
 - Rodolphe Lepigre
 - Tom Hirschowitz
 - Florian Hatat
 - Pierre Hyvernat
 - Guillaume Theyssier
 - Vincent Laporte
 - Alan Schmitt
