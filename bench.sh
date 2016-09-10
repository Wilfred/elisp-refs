#!/bin/bash

set -ex

cask eval "(progn (require 'refs) (refs--bench))"
