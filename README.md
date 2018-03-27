# georeference: R package for georeferencing places using GeoNames and Pelagios

## Description: 

**georeference** finds latitude and longitude of places using the Pelagios, GeoNames, and Wikipedia (georeferenced articles stored in the GeoNames database), gazetteers more suitable for historical and literary texts.

This package bases on the [geocode()](https://github.com/dkahle/ggmap/blob/master/R/geocode.R) function from the [ggmap package](https://github.com/dkahle/ggmap) by David Kahle.

## Installation 

You can install the development version from Github, via the package `devtools`

```
install.packages("devtools") # Unless you don't have it already installed.
library(devtools)
install_github("editio/georeference")
```

## Instructions

```
library(georeference)
georef("Rome")
```
```
# The function is set as default with Pelagios, so it is the same as:
georef("Rome", source = "pelagios")
```


```
# output
  lon     lat     name url 				                      searched_name
1 12.4843 41.8926 Roma http://pleiades.stoa.org/places/423025 Rome
```

For more options please see the usual R package documentation.


### License:

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License, version 2, as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose.  See the GNU General Public License for more details.

A copy of the GNU General Public License, version 2, is available at <https://www.r-project.org/Licenses/GPL-2>

