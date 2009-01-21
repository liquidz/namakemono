#!/bin/sh

pwd > .nmk_setup_tmp
dir=`sed -e 's/\//\\\\\//g' .nmk_setup_tmp`

sed -e "s/@NMK_PATH@/$dir/g" namakemono.scm.base > namakemono.scm

rm -f .nmk_setup_tmp
