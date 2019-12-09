streamconv
==========
Memory efficient conversion between pointcloud and mesh data formats.


Conversions
-----------
Please note that some data might not be transfered due to missing support within the target file format or limitations of `streamconv`.  
An up-to-data version of this list can be produced via `streamconv --list`.  
```
StlAscii -> StlAscii
StlAscii -> StlBinary
StlAscii -> Obj
StlAscii -> PlyAscii
StlAscii -> PlyBinary
StlAscii -> Xy
StlAscii -> Xyz
StlBinary -> StlAscii
StlBinary -> StlBinary
StlBinary -> Obj
StlBinary -> PlyAscii
StlBinary -> PlyBinary
StlBinary -> Xy
StlBinary -> Xyz
Obj -> StlAscii
Obj -> StlBinary
Obj -> Obj
Obj -> Off
Obj -> PlyAscii
Obj -> PlyBinary
Off -> StlAscii
Off -> StlBinary
Off -> Obj
Off -> Off
Off -> PlyAscii
Off -> PlyBinary
PlyAscii -> StlAscii
PlyAscii -> StlBinary
PlyAscii -> Obj
PlyAscii -> Off
PlyAscii -> PlyAscii
PlyAscii -> PlyBinary
Xyz -> Obj
Xyz -> PlyAscii
Xyz -> PlyBinary
Xyz -> Xy
Xyz -> Xyz
```

Build and Install
-----------------
Get [Git](https://en.wikipedia.org/wiki/Git) and [Haskell Stack](https://en.wikipedia.org/wiki/Stack_(Haskell)).  
```
$ git clone https://github.com/I3ck/streamconv
$ cd streamconv
$ stack install
```


Usage
-----
Run with the `--help` parameter for the most up-to-date information.  
```
$ streamconv --help

streamconv 0.0.2 (c) Martin Buck

Usage: streamconv --pin STRING --pout STRING --fin STRING --fout STRING
                  [--tmp1 STRING] [--tmp2 STRING] [--xyzval STRING]
                  [--xyzline STRING] [--list]
  streamconv 0.0.2 - Memory efficient conversion between pointcloud and mesh
  data formats. Use --help for more information (c) Martin Buck

Available options:
  -h,--help                Show this help text
  --pin STRING             Path to the input file
  --pout STRING            Path to write to
  --fin STRING             Input format [StlAscii, StlBinary, Obj, Off,
                           PlyAscii, PlyBinary, Xy, Xyz ]
  --fout STRING            Output format [StlAscii, StlBinary, Obj, Off,
                           PlyAscii, PlyBinary, Xy, Xyz ]
  --tmp1 STRING            Path that shall be used to write temporary
                           data (default: "streamconvtmp1.tmp")
  --tmp2 STRING            Path that shall be used to write temporary
                           data (default: "streamconvtmp2.tmp")
  --xyzval STRING          Delimiter to be used between xyz
                           values (default: ";")
  --xyzline STRING         Delimiter to be used between xyz
                           lines (default: "\n")
  --list                   List available combinations
```
You can use the `--list` command to see whether the conversion you'd like to use is supported. (See Conversions above)  
Assuming you'd want to convert `mesh.stl` which is `ascii` encoded to a binary `mesh.ply`:  
```
$ streamconv --pin "mesh.stl" --pout "mesh.ply" --fin "StlAscii" --fout "PlyBinary"
```


Temporary Files
---------------
Some conversions can currently only be done by utilizing temporary files.  
Please use the `--tmp1` and `--tmp2` parameters if you don't want `streamconv` to use the default paths and names.  


License
-------
`MIT` (also see `LICENSE`)


Contribution
------------
Feel free to create issues if you find any bugs or want to request features / conversions.  
You can also contribute code via `PR`s, but consider contacting me first, since my local version might be ahead or I might be working on the topic myself already.  