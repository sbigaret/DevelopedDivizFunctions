#! /bin/bash

CMD="./OWA_Descriptors.sh"

if [ $# != 0 ]; then
  echo "Usage: ${0}" >&2
  exit 1
fi

version=3

NB_TESTS=$(find tests -maxdepth 1 -type d -regex '.*/in[0-9]*\.v'"${version}"'$' | wc -l)

for i in $(seq 1 ${NB_TESTS}); do
    IN="tests/in${i}.v${version}"
    OUT="tests/out${i}.v${version}"
    ${CMD} "${IN}" "${OUT}"
done
