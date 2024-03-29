#/bin/sh
set -o errexit
set -o nounset

GREEN=$1
YELL=$2
GREY=$3
PATT=${4:-}

REGEX=$(echo ${GREEN} | sed s/^/\^/ | sed s/$/\$/ | sed s/\\./\[\^${GREY}\]/g)
grep -i ${REGEX} /usr/share/dict/words >wordle.tmp

echo ${YELL} | grep -o . | while read ch
do
  grep -i ${ch} wordle.tmp >wordle.tmp2
  mv wordle.tmp2 wordle.tmp
done

if [ -n "${PATT}" ]; then
  grep -i "${PATT}" wordle.tmp >wordle.tmp2
  mv wordle.tmp2 wordle.tmp
fi

column <wordle.tmp
rm wordle.tmp
