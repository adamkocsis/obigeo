# Change log of the R package 'obigeo'

# obigeo 0.3.0 - 2024-10-04 

A fresh beginning.

### Changed
- Accommodated to the new dependencies. 

* * *

## [0.2.2] (build 96) - 2021-05-04
### Added
- exportPajekUndirected() function to export a pajek file that can be read in with the infomap program.
- Rewritten the infomapConsole() function to work with version 1.x.

* * *

## [0.2.1] (build 95 - Uploaded to Zenodo without build n.) - 2020-08-01
### Added
- the 'propsing' argument of the bgstats() function. Setting this to FALSE will return emergence and disintegration proportions that do not include the single-interval regions. Earlier this defaulted to TRUE. 

* * *

## [0.2.0] (build 94) - 2020-07-31
### Added
- The submodularity() function
- initial omission of missing values in bgpart()

### Fixed
- an R internal-related bug that made the tracing method return values for cells that did not meet the ocq quota. This is an issue with unexpected malfunction, possibly because of the igraph package. I implemented a bypass, somewhat slower, but more reliable. 

* * *

## [0.2.0] (build 93) - 2020-07-30
### Fixed
- bgpart() method=NULL , when tracing is enabled

* * *

## [0.2.0] (build 92) - 2020-07-30
### Added
- bidensity() function

## [0.2.0] (build 91 - Version without build number, uploaded to Zenodo.) - 2020-07-30
### Added
- documentation for the datasets and some examples to bgpart(), bplot() and endemism()
- imports from chronosphere

## [0.2.0] (build 90) - 2020-05-07
### Added
- The add and asp arguments to bgplot(). Aspect ratio is now constrained by default.

## [0.2.0] (build 89) - 2020-04-13
### Changed
- Bug fix for bgstats(). NA entries in PIE calculation are omitted in all cases. 

## [0.2.0] (build 88) - 2020-04-08
### Changed
- Bug fix for bgstats(). The function had issues with slicewise-input (list of dataframes) , which is solved. The mjc argument of the function now can be set to NULL, that swtiches the joint cell lookup for slicewise-input. The noNAStart argument can also be set to NULL now, which turns of the NA insertion completely.

## [0.2.0] (build 87) - 2020-04-08
### Added
- axes argument of bgplot()

## [0.2.0] (build 86) - 2019-08-23
### Added
- Tracing method for the bgpart()-distance method

### Changed
- R 3.5.0 dependency

## [0.2.0] (build 78) - 2018-02-01
### Deleted
- The Paleomap paleorasters and the DEMs were moved to earthhist.
- mapplot() and mapindex() are also in earthhist.
- uploaded to github for proper version tracking
- compiled first binaries and source
