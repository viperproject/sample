#!/usr/bin/bash

ARG=$1
TMPDIR=/tmp/$ARG

if [ -d "$TMPDIR" ]; then
	ls -1 $TMPDIR
	xdg-open $TMPDIR/index.html
	exit 0
fi

mkdir -p $TMPDIR

echo "$TMPDIR/${ARG}.html"
curl -s https://www.touchdevelop.com/api/$ARG/webast | python -m json.tool > $TMPDIR/$ARG.json
echo "<a href='$ARG.html'>$ARG.html</a>" >> $TMPDIR/index.html
echo "$TMPDIR/${ARG}.json"
curl -s https://www.touchdevelop.com/api/$ARG/text > $TMPDIR/$ARG.txt
echo "<a href='$ARG.json'>$ARG.json</a>" >> $TMPDIR/index.html
echo "$TMPDIR/${ARG}.txt"
curl -s https://www.touchdevelop.com/api/$ARG/pretty > $TMPDIR/$ARG.html
echo "<a href='$ARG.txt'>$ARG.txt</a>" >> $TMPDIR/index.html
echo "<br>" >> $TMPDIR/index.html

LIBS=`grep -Po 'pub "\K[^"]*' $TMPDIR/$ARG.txt`

for i in $LIBS
do
	echo "$TMPDIR/$i.html"
	curl -s https://www.touchdevelop.com/api/$i/webast | python -m json.tool > $TMPDIR/$i.json
	echo "<a href='$i.html'>$i.html</a>" >> $TMPDIR/index.html
	echo "$TMPDIR/$i.json"
	curl -s https://www.touchdevelop.com/api/$i/text > $TMPDIR/$i.txt
	echo "<a href='$i.json'>$i.json</a>" >> $TMPDIR/index.html
	echo "$TMPDIR/$i.txt"
	curl -s https://www.touchdevelop.com/api/$i/pretty > $TMPDIR/$i.html
	echo "<a href='$i.txt'>$i.txt</a>" >> $TMPDIR/index.html
	echo "<br>" >> $TMPDIR/index.html
done

xdg-open $TMPDIR/index.html
