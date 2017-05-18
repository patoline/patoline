#!/bin/bash

make configure
./configure
make -j 8 all
make -j 8 packages
