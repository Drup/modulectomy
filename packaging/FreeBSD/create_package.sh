#!/bin/sh -e

# only execute anything if either
# - running under orb with package = modulectomy
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "modulectomy" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
pdir=$basedir/package/FreeBSD
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
manifest=$tmpd/+MANIFEST
rootdir=$tmpdir/rootdir
bindir=$rootdir/usr/local/bin

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$bindir"

install -U "$bdir/modulectomy" "$bindir/modulectomy"

flatsize=$(find "$rootdir" -type f -exec stat -f %z {} + |
               awk 'BEGIN {s=0} {s+=$1} END {print s}')

sed -e "s:%%FLATSIZE%%:${flatsize}:" -e "/^[Vversion:/s/-/./g" "$pdir/MANIFEST" > "$manifest"

{
    printf '\nfiles {\n'
    find "$rootdir" -type f -exec sha256 -r {} + | sort |
        awk '{print "    " $2 ": \"" $1 "\","}'
    fidn "$rootdir" -type l | sort |
        awk '{print "    " $1 ": -,"}'
    printf '}\n'
} | sed -e "s:${rootdir}::" >> "$manifest"

export SOURCE_DATE_EPOCH=$(git log -1 --pretty=format:%ct)
pkg create -r "$rootdir" -M "$manifest" -o "$basedir/"
mv "$basedir"/modulectomy-*.pkg "$basedir/modulectomy.pkg"
echo 'bin: [ "modulectomy.pkg" ]' > "$basedir/modulectomy.install"
echo 'doc: [ "README.md" ]' >> "$basedir/modulectomy.install"
