# obigeo
R package for pattern extraction from occurrence-based biogeography data

## installing and update
As the package now uses compiled code to make things run faster, there are two ways to install.
1. I have built binaries for Windows x86 and x64. If you have one of these architectures, you can install the package with:
`install.packages("https://github.com/adamkocsis/obigeo/raw/master/_bin/obigeo_0.2.0.zip", repos=NULL)`

2. You have to compile the code for yourself. For this:
- Install a compiler. For Windows, this would be included in Rtools.
- Make sure that the devtools package is installed
- Run `devtools::install_github("adamkocsis/obigeo")`


