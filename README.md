# obigeo
R package for pattern extraction from occurrence-based biogeography data

This package is still in intense development. It will be uploaded to the CRAN servers in several months.

After installing, type in
```r
?bgpart
```
to see the main function for biogeographic partitioning. 

## installing and update
As the package now uses compiled code to make things run faster, there are two ways to install.
1. I have built binaries for Windows x86 and x64.First you have to download the archive:``
https://github.com/adamkocsis/obigeo/raw/master/_archive/bin/Win_x64_x86/obigeo_0.2.0.zip
``

Save it somewhere, and then run 
`install.packages("<yourpath>obigeo_0.2.0.zip/", repos=NULL, type="win.binary")`


2. You have to compile the code for yourself. For this:
- Install a compiler. For Windows, this would be included in Rtools.
- Make sure that the devtools package is installed
- Download the archive:
``
https://github.com/adamkocsis/obigeo/raw/master/_archive/source/obigeo_0.2.0.tar.gz
``

Save it somewhere, and then run 
`install.packages("<yourpath>obigeo_0.2.0.tar.gz/", repos=NULL)`


