# obigeo
R package for pattern extraction from occurrence-based biogeography data

## installing and update
As the package now uses compiled code to make things run faster, there are two ways to install.
1. I have built binaries for Windows x86 and x64. As this is a private repository, you cannot install directly from here . First you have to download the archive:``
https://github.com/adamkocsis/obigeo/raw/master/_archive/bin/Win_x64_x86/obigeo_0.2.0-86.zip
``

Save it somewhere, and then run 
`install.packages("<yourpath>obigeo_0.2.0-86.zip/", repos=NULL)`


2. You have to compile the code for yourself. For this:
- Install a compiler. For Windows, this would be included in Rtools.
- Make sure that the devtools package is installed
- Download the archive:
``
https://github.com/adamkocsis/obigeo/raw/master/_archive/source/obigeo_0.2.0-86.tar.gz
``

Save it somewhere, and then run 
`install.packages("<yourpath>obigeo_0.2.0-86.tar.gz/", repos=NULL)`


