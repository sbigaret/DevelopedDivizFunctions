#!/usr/bin/env bash

CMD="./FuzzyLabelsDescriptors.sh"

version="3"
NB_TESTS=$(find tests -maxdepth 1 -type d -regex '.*/in[0-9]*\.v'"${version}"'$' | wc -l)

c14n_dir()
{
    find "${1:?}" -type f -name '*.xml' -print0 | \
        while IFS= read -r -d $'\0' xmlfile
    do
        xmllint --c14n "$xmlfile" > "${xmlfile}".c14n
        mv "${xmlfile}".c14n "${xmlfile}"
    done
}

mkdir -p tests_tmp

for i in $(seq 1 "${NB_TESTS}"); do
    IN="tests/in${i}.v${version}"
    REFERENCE_OUT="tests/out${i}.v${version}"
    OUT=$(mktemp --tmpdir=. -d tests_tmp/out.XXX)
    mkdir "${OUT}"/expected "${OUT}"/real
    cp -Rp "${REFERENCE_OUT}"/. "${OUT}"/expected

    REFERENCE_OUT="${OUT}"/expected
    OUT="${OUT}"/real

    echo "${IN}"
    ${CMD} "${IN}" "${OUT}"

    c14n_dir $REFERENCE_OUT
    c14n_dir $OUT

    diff -x README -ruBw "${REFERENCE_OUT}" "${OUT}"
    ret_diff=$?
    if [ $ret_diff -ne 0 ]; then
        echo -e "\033[91;1mFAILED\033[0m: ${IN}"
    else
        echo -e "\033[32mSUCCESS\033[0m: ${IN}"
        rm -r "${OUT}"
    fi
done
