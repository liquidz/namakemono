#!/bin/sh

pwd > .nmk_setup_tmp
dir=`sed -e 's/\//\\\\\//g' .nmk_setup_tmp`

if [ -e "namakemono.scm" ]; then
	rm -f namakemono.scm
fi

sed -e "s/@NMK_PATH@/$dir/g" namakemono.scm.base > namakemono.scm
env gosh scm2exe.scm -o nmk namakemono.scm
#sed -e "s/@NMK_PATH@/$dir/g" nmk.base > nmk

chmod 444 namakemono.scm
#chmod 755 nmk

here=`pwd`
ln -s $here/nmk /usr/local/bin

rm -f .nmk_setup_tmp
