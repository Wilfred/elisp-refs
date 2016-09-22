#!/bin/bash

set -ex

cask eval "(progn (require 'refs-bench) (refs-bench))"
