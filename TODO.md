Features
========
Proper testing framework

Laserscanner formats

Need withBinaryFile and readBinaryFile ?

Support other off formats ( http://www.geomview.org/docs/html/OFF.html )

Disallow transform to same type?

Determine input format automatically

Only require bin flag for output and determine type from name?

Give entire lib a module name (Streamconv)?

Remove prefixes / suffixes such as sources and sinks and simply include qualified

Should be possible to implement 'triple' transformer in such a way that it only writes a tmp file for the position data



Bugs
====
Can currently not call --list without providing required arguments



Other
=====
profiling via
stack build --profile
stack exec streamconv -- +RTS -p