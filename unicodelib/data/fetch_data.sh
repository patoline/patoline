#!/bin/bash
# Fetch the unicode database

VERSION=7.0.0
URL=ftp://www.unicode.org/Public/zipped/$VERSION/


wget -r -nd $URL
