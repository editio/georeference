# georeference: R package for georeferencing places using GeoNames ~~and Pelagios~~.

**Update**: The Peripleo API from Pelagios seems to be discontinued, so the default function will not work.  The package still works for Geonames

## Description: 

**georeference** finds latitude and longitude of places using the [Pelagios](http://commons.pelagios.org), [GeoNames](http://www.geonames.org), and [Wikipedia](http://www.geonames.org/wikipedia/) (georeferenced articles stored in the GeoNames database), gazetteers more suitable for historical and literary texts. 
Pelagios is not actually a gazetteer per se, but an infrastructure of Linked Open Geodata in the Humanities, that enables, for instance, searching across different gazetteers; **georeference** limits the Pelagios datasets to the gazetteers Pleiades, ToposText, DARE, iDAI, and GeoNames. 

This package bases on the [geocode()](https://github.com/dkahle/ggmap/blob/master/R/geocode.R) function from the [ggmap package](https://github.com/dkahle/ggmap) by David Kahle.

## Installation: 

You can install the development version from Github, via the package `devtools`

```
install.packages("devtools") # Unless you don't have it already installed.
library(devtools)
install_github("editio/georeference")
```


## Instructions:

```
library(georeference)
georef("Rome")
```
```
# The function is set as default with Pelagios, so it is the same as:
g

# output
  lon     lat     name url                                    searched_name
1 12.4843 41.8926 Roma http://pleiades.stoa.org/places/423025 Rome
```

For more options please see the usual R package documentation. I wrote a [short post](http://editio.github.io/2018/03/27/georeference-geolocation-r-package.html) with an explanation on how to use it together with the leaflet package for R. 


### License:

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License, version 2, as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose.  See the GNU General Public License for more details.

A copy of the GNU General Public License, version 2, is available at <https://www.r-project.org/Licenses/GPL-2>

