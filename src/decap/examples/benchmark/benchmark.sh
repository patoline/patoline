#!/bin/sh

cd ..
make examples/calc examples/calc_strict
cd benchmark

echo glr test3.txt
time ../examples/calc < test3.txt
echo glr test2.txt
time ../examples/calc < test2.txt
echo glr test1.txt
time ../examples/calc < test1.txt
echo glr test5.txt
time ../examples/calc < test5.txt
echo glr test4.txt
time ../examples/calc < test4.txt

echo lr test3.txt
time ../examples/calc_strict < test3.txt
echo lr test2.txt
time ../examples/calc_strict < test2.txt
echo lr test1.txt
time ../examples/calc_strict < test1.txt
echo lr test5.txt
time ../examples/calc_strict < test5.txt
echo lr test4.txt
time ../examples/calc_strict < test4.txt

cd yacccalc
source make.sh
source make_menhir.sh
cd ..

echo ocamlyacc test3.txt
time yacccalc/calc < test3.txt
echo ocamlyacc test2.txt
time yacccalc/calc < test2.txt
echo ocamlyacc test1.txt
time yacccalc/calc < test1.txt
echo ocamlyacc test5.txt
time yacccalc/calc < test5.txt
echo ocamlyacc test4.txt
time yacccalc/calc < test4.txt

echo menhir test3.txt
time yacccalc/mcalc < test3.txt
echo menhir test2.txt
time yacccalc/mcalc < test2.txt
echo menhir test1.txt
time yacccalc/mcalc < test1.txt
echo menhir test5.txt
time yacccalc/mcalc < test5.txt
echo menhir test4.txt
time yacccalc/mcalc < test4.txt

cd dypcalc1
make
cd ../dypcalc2
make
cd ../dypcalc3
make
cd ..

echo dypgen v1 test3.txt
time timeout 120 dypcalc1/calc < test3.txt
echo dypgen v1 test2.txt
time timeout 120 dypcalc1/calc < test2.txt
echo dypgen v1 test1.txt
time timeout 120 dypcalc1/calc < test1.txt
echo dypgen v1 test5.txt
time timeout 120 dypcalc1/calc < test5.txt
echo dypgen v1 test4.txt
time timeout 120 dypcalc1/calc < test4.txt
echo dypgen v1 test3.txt

time timeout 120 dypcalc2/calc < test3.txt
echo dypgen v2 test2.txt
time timeout 120 dypcalc2/calc < test2.txt
echo dypgen v2 test1.txt
time timeout 120 dypcalc2/calc < test1.txt
echo dypgen v2 test5.txt
time timeout 120 dypcalc2/calc < test5.txt
echo dypgen v2 test4.txt
time timeout 120 dypcalc2/calc < test4.txt

echo dypgen v3 test3.txt
time timeout 120 dypcalc3/calc < test3.txt
echo dypgen v3 test2.txt
time timeout 120 dypcalc3/calc < test2.txt
echo dypgen v3 test1.txt
time timeout 120 dypcalc3/calc < test1.txt
echo dypgen v3 test5.txt
time timeout 120 dypcalc3/calc < test5.txt
echo dypgen v3 test4.txt
time timeout 120 dypcalc3/calc < test4.txt
