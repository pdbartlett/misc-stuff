#/bin/sh
PATT=$1
YELL=$2
GREY=$3

NPAT=$(echo ${PATT} | sed s/^/\^/ | sed s/$/\$/ | sed s/\\./\[\^${GREY}\]/g)
grep -i ${NPAT} /usr/share/dict/words >wordle.tmp

echo ${YELL} | grep -o . | while read ch
do
  grep -i ${ch} wordle.tmp >wordle.tmp2
  mv wordle.tmp2 wordle.tmp
done

cat wordle.tmp
rm wordle.tmp
