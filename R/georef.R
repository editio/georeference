#' Geocoding with two gazzetters: Pelagios and GeoNames. 
#'
#' The function returns the first hit into a data frame with latitude, longitude, location name, url/id, and searched query from the gazetteers Pelagios (default) and Geonames (as well as from the georeferenced Wikipedia articles stored in the GeoNames database). To get everything, not just the first hit, set the parameter to output = "all". GeoNames requires a user account (free) to use their API services.
#'
#'    
#' See Geonames at 
#' \url{http://www.geonames.org};
#' See Pelagios at 
#' \url{http://commons.pelagios.org}
#' 
#' 
#' @param location a character vector of place names (e.g. "Rome")
#'   
#' @param source "pelagios" for Pelagios (default); "geonames" for a simple search in GeoNames; "geowiki_title" for search just the title in Wikipedia database from GeoNames; "geowiki_all" for search in Wikipedia database from GeoNames.
#' @param output amount of output: "latlon" (default), "all" (gives a list of everything not just a selected dataframe). 
#' @param inject character string to add to the url. It is mandatory for "geonames", "geowiki_all", "geowiki_title": inject = username="your_geoname_username" (without blankspaces).
#' @param language character string to add to the language for "geowiki_all", "geowiki_title": language = "lang=es" (without blankspaces). Default is English.
#' @param place.pelagios character string to add to the search in Pelagios to limit the search just to place:  place.pelagios = "types=place"
#' 
#' @return If \code{output} is "latlon" it returns a dataframe; If "all", a list.
#'   
#' @author Jose Luis Losada \email{losadajoseluis@gmail.com}, mostly reusing and directly based on the geocode function (ggmap) by David Kahle.
#' @export
#' @examples \dontrun{
#' 
#' georef("Rome"). The function is set as default with pelagios, so it is the same as georef("Rome", source = "pelagios").
#' 
#' georef(c("Rome", "Madrid"))
#' 
#' places = c("Valladolid", "Madrid", "Rome")
#' georef(places)
#' 
#' georef(c("Rome", "Complutum"), place.pelagios = "types=place")
#' 
#' georef(c("Valladolid", "Complutum"), source = "geonames", inject = "username=your_geonames_username")
#' 
#' georef("Rome", source = "geowiki_all", inject = "username=your_geonames_username") 
#' 
#' georef("Rome", source = "geowiki_title", inject = "username=your_geonames_username")
#' 
#' }
#'
#'
#'

georef <- function(location, output = c("latlon", "all"), source = c("pelagios", "geowiki_all", "geowiki_title", "geonames"), messaging = FALSE, urlonly = FALSE, inject = "", language = "", place.pelagios = ""
){

  # basic parameter check
  stopifnot(is.character(location))
  output   <- match.arg(output)
  source   <- match.arg(source)

  # vectorize for many locations
  if(length(location) > 1){
    # set limit
     { # Not sure about the limits in geonames and pelagios, but set in order to make it work
      limit <- "3000"
    }

    # message/stop as neeeded
    s <- sprintf("limit of request is set to 3000", limit)
    if(length(location) > as.numeric(limit)) stop(s, call. = FALSE)
    if(length(location) > 200 && messaging) message(paste("Reminder", s, sep = " : "))

    # geocode ply and out
    if(output == "latlon"){
      return(ldply(as.list(location), georef, output = output, source = source, inject = inject, language = language, place.pelagios = place.pelagios))
    } else { # output = all
      return(llply(as.list(location), georef, output = output, source = source, inject = inject, language = language, place.pelagios = place.pelagios))
    }
  }

  # return NA for location == ""
  if(location == "") return(failedGeocodeReturn(output))

  # start constructing the url
  posturl <- URLencode(location, reserved = TRUE)

  # old API: http://pelagios.org/peripleo
  # new api http://peripleo.pelagios.org/peripleo
  
   if(source == "pelagios"){
    url_string <- paste0(
      sprintf("http://pelagios.org/peripleo/search?query="),
      posturl
    )
  }
  
  else if (source == "geowiki_all"){
    url_string <- paste0(
      sprintf("http://api.geonames.org/wikipediaSearchJSON?q="),
      posturl
        )
      }

  else if (source == "geowiki_title"){
    url_string <- paste0(
      sprintf("http://api.geonames.org/wikipediaSearchJSON?title="),
      posturl
    )
  }
  
  else if (source == "geonames"){
    url_string <- paste0(
      sprintf("http://api.geonames.org/searchJSON?q="),
      posturl
    )
  }
  
  
  # inject any remaining stuff
  if(inject != "") url_string <- paste(url_string, inject, sep = "&")
  if(language != "") url_string <- paste(url_string, language, sep = "&")
  if(place.pelagios != "") url_string <- paste(url_string, place.pelagios, sep = "&")
  
  # encode
  url_string <- URLencode( enc2utf8(url_string) )
  if(urlonly) return(url_string)
  url_hash   <- digest::digest(url_string)

  
  # lookup info if on file
  if(isGeocodedInformationOnFile(url_hash)){

  	if(messaging) message("Using stored information.")
    gc <- get(".GeocodedInformation", envir = .GlobalEnv)[[url_hash]]

  } else {

    if(messaging) message(paste("contacting ", url_string, "...", sep = ""), appendLF = F)

    # message user
    message("Gazzetter Source: ", url_string)

    # georef
    connect <- url(url_string); on.exit(close(connect), add = TRUE)
    lines <- try(readLines(connect, warn = FALSE), silent = TRUE)

    if(class(lines) == "try-error"){
      warning(
        "  geocoding failed for \"", location, "\".\n",
        "try other gazzetter"
      )
      return(failedGeocodeReturn(output))
    }

    gc <- fromJSON(paste(lines, collapse = ""))
    if(messaging) message(" done.")


    # temporarily save it
    storeGeocodedInformation(url_hash, gc)

  }



  # return if you want full output
  if(output == "all") return(gc)
  
  
  # Results for PELAGIOS
  
  if(source == "pelagios") {
    
    if(gc$total == "0") {
      warning(paste("geocode failed with status (total) ", gc$total, ", location = \"",
                    location, "\"", sep = ""), call. = FALSE)
      return(data.frame(lon = NA, lat = NA))
    }
  }
  
  else if(length(gc$geonames) == 0)    {
    warning(paste("geocode failed for location = \"", location, "\"", sep = ""), call. = FALSE) 
    return(data.frame(lon = NA, lat = NA)) }
  
  # more than one location found. To do...

  if(length(gc$geonames) > 1) {
    warning(paste(
      "more than one location found for \"",location, "\", returning the first ocurrence: \"",
      tolower(gc$geonames[[1]]$title),"\"\n"
    ), call. = FALSE)
  }

  
  if(source == "google") {
    
    # format geocoded data
    NULLtoNA <- function(x){
      if(is.null(x)) return(NA)
      x
    }
    
    gcdf <- with(gc$results[[1]], {
      data.frame(
        lon = NULLtoNA(geometry$location$lng),
        lat = NULLtoNA(geometry$location$lat),
        name = NULLtoNA(address_components[[1]]$short_name), 
        searched_name = location
      )
    })
  }
  
  if(source == "pelagios"){
    
    # format geocoded data
    NULLtoNA <- function(x){
      if(is.null(x)) return(NA)
      x
    }
    
    # Pelagios seems to return only geobounds, not lat&long points. When maxlon = minlon, maxlat = minlat, it results a point, otherways it necessary to convert the geobounds to centroid points (I am just averaging the two points to calculate the midpoint, as in a flat surface...); not nice, for examples like Spain o Russia, but for now...  
    
    
    gcdf <- with(gc$items[[1]], {
      data.frame(
        lon = NULLtoNA((geo_bounds$max_lon + geo_bounds$min_lon)/2),
        lat = NULLtoNA((geo_bounds$max_lat + geo_bounds$min_lat)/2),
        name = NULLtoNA(title),
        url = NULLtoNA(identifier),
        searched_name = location
        # Possible to add more: 
      )
    })
  }
  
  if(source == "geowiki_title"){
    
    # format geocoded data
    NULLtoNA <- function(x){
      if(is.null(x)) return(NA)
      x
    }
    
    gcdf <- with(gc$geonames[[1]], {
      data.frame(
        lon = NULLtoNA(lng),
        lat = NULLtoNA(lat),
        name = NULLtoNA(title),
        url = NULLtoNA(wikipediaUrl),
        searched_name = location
        # Possible to add more: 
      )
    })}
  
  if(source == "geowiki_all"){
  
    # format geocoded data
    NULLtoNA <- function(x){
      if(is.null(x)) return(NA)
      x
    }
    
   gcdf <- with(gc$geonames[[1]], {
  	data.frame(
  	  lon = NULLtoNA(lng),
      lat = NULLtoNA(lat),
      name = NULLtoNA(title),
  	  url = NULLtoNA(wikipediaUrl),
  	  searched_name = location
  	  # Possible to add more: 
  	      )
    })
   }

  if(source == "geonames"){ 
    
    # format geocoded data
    NULLtoNA <- function(x){
      if(is.null(x)) return(NA)
      x
    }
    
    gcdf <- with(gc$geonames[[1]], {
    data.frame(
      lon = as.numeric(NULLtoNA(lng)), #returns factor? The rest returns num. Adding as. numeric.
      lat = as.numeric(NULLtoNA(lat)), #returns factor? The rest returns num. Adding as. numeric.
      name = NULLtoNA(name),
      searched_name = location,
      geonameid = NULLtoNA(paste("http://sws.geonames.org/",sep = "",geonameId))
      # it is possible to add more: 
    )
  })
  }

  # add address
  if(source == "google"){
    gcdf$address <- tolower(NULLtoNA(gc$results[[1]]$formatted_address))
  }

  if(output == "latlon" && source == "geonames") return(gcdf[,c("lon","lat", "name", "searched_name", "geonameid")])
  if(output == "latlon" && source == "geowiki_all") return(gcdf[,c("lon","lat", "name", "url", "searched_name")])
  if(output == "latlon" && source == "geowiki_title") return(gcdf[,c("lon","lat", "name", "url", "searched_name")])
  if(output == "latlon" && source == "pelagios") return(gcdf[,c("lon","lat", "name", "url", "searched_name")])
  if(output == "latlon" && source == "google") return(gcdf[,c("lon","lat", "name", "searched_name")])
  
  if(output == "latlona" && source == "google") return(gcdf[,c("lon","lat","address")])


  # parse json when output == "more". To clean up...
  nameToGrab   <- `if`(nameType == "long", "long_name", "short_name")
  outputVals  <- vapply(gc$results[[1]]$address_components, function(x) x[[nameToGrab]], character(1))
  outputNames <- vapply(gc$results[[1]]$address_components, function(x){
      if(length(x$types) == 0) return("query")
      x$types[1]
    },
    character(1)
  )
  gcdfMore <- as.data.frame(as.list(outputVals))
  names(gcdfMore) <- outputNames

  data.frame(gcdf, gcdfMore)
}


checkGeocodeQueryLimit <- function(url_hash, elems, override, messaging){

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick

  if(exists(".GoogleGeocodeQueryCount", .GlobalEnv)){

    .GoogleGeocodeQueryCount <<- dplyr::filter(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)

    # limit per 24 hours
    dayQueriesUsed <- sum(.GoogleGeocodeQueryCount$elements)
    if(dayQueriesUsed + elems > goog_day_limit()){
      message("query max exceeded, see ?geocode.  current total = ", dayQueriesUsed)
      if(!override) return("stop")
    }

    # limit per second
    secondQueriesUsed <- with(.GoogleGeocodeQueryCount, sum(elements[time >= Sys.time() - 1]))
    if(secondQueriesUsed + elems > goog_second_limit()){
      message(".", appendLF = FALSE)
      Sys.sleep(.2) # can do better
    }

    # append to .GoogleGeocodeQueryCount
    .GoogleGeocodeQueryCount <<- rbind(
      .GoogleGeocodeQueryCount,
      data.frame(
        time = Sys.time(),
        url = url_hash,
        elements = elems,
        stringsAsFactors = FALSE
      )
    )

  } else { # no geocodes on file

    .GoogleGeocodeQueryCount <<- data.frame(
      time = Sys.time(),
      url = url_hash,
      elements = elems,
      stringsAsFactors = FALSE
    )

  }

  invisible("go")
}

geocodeQueryCheck <- function() {

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount);

  if(exists(".GoogleGeocodeQueryCount", .GlobalEnv)){

  	remaining <- goog_day_limit() - sum(
  	  dplyr::filter(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)$elements
  	)
    message(remaining, " geocoding queries remaining.")

  } else {

  	remaining <- goog_day_limit()
    message(remaining, " geocoding queries remaining.")

  }

  invisible(remaining)
}





geoInfoDoesntExist <- function(){
  ".GeocodedInformation" %notin% ls(envir = .GlobalEnv, all.names =  TRUE)
}





storeGeocodedInformation <- function(url_hash, data){
  .GeocodedInformation <- NULL; rm(.GeocodedInformation)

  if(geoInfoDoesntExist()) .GeocodedInformation <<- list()

  db <- get(".GeocodedInformation", envir = .GlobalEnv)

  placesOnFile <- names(db)
  db <- c(db, list(data))
  names(db) <- c(placesOnFile, url_hash)

  .GeocodedInformation <<- db

  invisible()
}


retrieveGeocodedInformation <- function(url_hash){
  if(geoInfoDoesntExist()) return(NA)
  get(".GeocodedInformation", envir = .GlobalEnv)[[url_hash]]
}

isGeocodedInformationOnFile <- function(url_hash){
  if(geoInfoDoesntExist()) return(FALSE)
  if(url_hash %notin% names(get(".GeocodedInformation", envir = .GlobalEnv))) return(FALSE)
  TRUE
}


clearGeocodedInformation <- function(){
  # suppress in case it doesn't exist
  suppressWarnings(rm(".GeocodedInformation", envir = .GlobalEnv))
  invisible()
}

failedGeocodeReturn <- function(output){
  if(output == "latlon"){
    return(data.frame(lon = NA_real_, lat = NA_real_))
  } else if(output == "latlona"){
    return(c(lon = NA_real_, lat = NA_real_, address = NA_character_))
  } else if(output == "more") {
    return(c(
      lon = NA_real_, lat = NA_real_, type = NA_character_, loctype = NA_character_,
      address = NA_character_,
      north = NA_real_, south = NA_real_, east = NA_real_, west = NA_real_,
      locality = NA_character_, country = NA_character_
    ))
  } else {
    return(NA_real_)
  }
}