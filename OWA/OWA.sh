#! /bin/bash
# Usage:
#  OWA.sh input_dir output_dir

# Adapt if needed.  R v3.x is required
# Use the full path here
R3=/usr/bin/R

if [ ! -x "${R3}" ]; then
  echo "Please edit '$0': R exec not found" >&2
  exit 1
fi
if [ ! "version 3" = "$(${R3} --version | head -1 | grep -o 'version [0-9]')" ]; then
  echo "Please edit '$0': $R3 is not R version 3.x" >&2
  exit 2
fi

# -- You normally do not need to change anything beyond this point --

if [ ! $# == 2 ]; then
  echo "Usage: $0 input_dir output_dir" >&2
  exit 3
else
  R --slave --vanilla --file=src/OWA_XMCDAv3.R --args $1 $2
  ret=$?
fi


exit $ret
