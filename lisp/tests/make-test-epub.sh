#!/usr/bin/env bash
# Build a minimal, valid EPUB 2 for the stage-5 checks.
# Usage: make-test-epub.sh /path/to/out.epub
set -euo pipefail

OUT="${1:?usage: make-test-epub.sh OUT.epub}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$WORK/META-INF" "$WORK/OEBPS"

printf 'application/epub+zip' > "$WORK/mimetype"

cat > "$WORK/META-INF/container.xml" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>
XML

cat > "$WORK/OEBPS/content.opf" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<package xmlns="http://www.idpf.org/2007/opf" version="2.0" unique-identifier="bookid">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:identifier id="bookid">urn:uuid:00000000-0000-4000-8000-00000000test</dc:identifier>
    <dc:title>Test Book On Retrieval</dc:title>
    <dc:creator>Fixture Author</dc:creator>
    <dc:language>en</dc:language>
  </metadata>
  <manifest>
    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>
    <item id="ch1" href="chapter1.xhtml" media-type="application/xhtml+xml"/>
    <item id="ch2" href="chapter2.xhtml" media-type="application/xhtml+xml"/>
  </manifest>
  <spine toc="ncx">
    <itemref idref="ch1"/>
    <itemref idref="ch2"/>
  </spine>
</package>
XML

cat > "$WORK/OEBPS/toc.ncx" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
  <head><meta name="dtb:uid" content="urn:uuid:00000000-0000-4000-8000-00000000test"/></head>
  <docTitle><text>Test Book On Retrieval</text></docTitle>
  <navMap>
    <navPoint id="n1" playOrder="1">
      <navLabel><text>What retrieval is</text></navLabel>
      <content src="chapter1.xhtml"/>
    </navPoint>
    <navPoint id="n2" playOrder="2">
      <navLabel><text>Chunking tradeoffs</text></navLabel>
      <content src="chapter2.xhtml"/>
    </navPoint>
  </navMap>
</ncx>
XML

cat > "$WORK/OEBPS/chapter1.xhtml" <<'XHTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head><title>What retrieval is</title></head>
  <body>
    <h1>What retrieval is</h1>
    <p>Retrieval finds the passages that answer a question.</p>
    <p>Chunking decides what a passage is in the first place.</p>
  </body>
</html>
XHTML

cat > "$WORK/OEBPS/chapter2.xhtml" <<'XHTML'
<?xml version="1.0" encoding="UTF-8"?>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head><title>Chunking tradeoffs</title></head>
  <body>
    <h1>Chunking tradeoffs</h1>
    <p>Small chunks retrieve precisely but lose context.</p>
  </body>
</html>
XHTML

rm -f "$OUT"
( cd "$WORK" && zip -q0 "$OUT" mimetype && zip -q9r "$OUT" META-INF OEBPS )
printf 'wrote %s\n' "$OUT"
