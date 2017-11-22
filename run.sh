#!/usr/bin/env bash

sbcl --noinform --load fake-files.lisp --eval "(ff:main)" --quit
