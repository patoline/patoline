#!/bin/bash

make configure && ./configure && make -j 8  && make -j 8 packages && make install
