# Maintainer: Rodolphe Lepigre <rodolphe.lepigre@univ-savoie.fr>
pkgname=patoline-current
pkgver=0
pkgrel=1
pkgdesc="Modern typesetting system written in OCaml, alternative to TeX/LaTex"
arch=('any')
url="http://www.patoline.org"
license=('GPL3')
depends=('ocaml' 'ocaml-findlib' 'dypgen' 'ocaml-sqlite3' 'ocaml-zip')
optdepends=('ocaml-cairo-git: cairo driver'
            'ocaml-lablgl: openGL driver'
            'ocamlnet: presentation control unsing websockets')
makedepends=('wget')
replaces=('patoline')
options=(!strip staticlibs)
source=()
md5sums=()

build() {
  cd "$srcdir"
  wget http://patoline.org/patoline-current.tar.gz
  tar -xvf patoline-current.tar.gz
  cd "$pkgname"
  make configure
  ./configure --prefix /usr
  make
}

package() {
	cd "$srcdir/$pkgname"
	make DESTDIR="$pkgdir/" install
}
