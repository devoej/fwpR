#' Convert collar point locations to an sf line geometry object
#'
#' This function converts a dataframe or sf point object containing collar locations for individual animals to an sf object containing line geometry features for each individual.
#' Note:
#' - Coordinates must be in WGS84 - Lat/Longs
#' - Assumes collar data has been "cleaned"
#'
#' @param x object of data.frame or sf point geometry object containing collar locations
#' @param indID name of the field identifying individual animals
#' @param datetime name of the field identifying DateTime
#' @param lat if data.frame, name of the field containing the latitude
#' @param long if data.frame, name of the field containing the longitude
#' @return A sf line geometry layer by individual
#' @export

# Function to convert collar locs to lines
toLines <- function(x,
                    indID,
                    datetime,
                    lat,
                    long) {

  # If a dataframe, convert to sf
  if(is.data.frame(x)){

    sf <- x %>%
      sf::st_as_sf(coords = c({{long}}, {{lat}}), remove = F, crs = "+init=EPSG:4326")

    # otherwise proceed with the sf object
    } else {

    sf <- x

    }

  # Loop thru individual animals to create line features for each
  uniqueInds <- unique(sf[[indID]])
  linesList <- list()

  for(i in 1:length(uniqueInds)) {

    # Filter to individual
    sfInd <- sf %>%
      dplyr::filter(.data[[indID]] == uniqueInds[i]) %>%
      dplyr::arrange(.data[[datetime]]) # ensure arranged properly

    # Retain all the attributes as a dataframe
    #   (joined back onto sf object later)
    dfInd <- sfInd %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::mutate(DateTimeStart = .data[[datetime]],
                    DateTimeEnd = dplyr::lead(DateTimeStart)) %>%
      dplyr::do(head(., nrow(.)-1)) # remove last row for matching to lines geometry (no. lines = no. pts-1)

    # Create line geometries
    gInd <- sf::st_geometry(sfInd) # get geometry only
    Linestrings <- lapply(X = 1:(length(gInd)-1), FUN = function(x) { # create individual line segments & stuff in list
      pair <- sf::st_combine(c(gInd[x], gInd[x+1]))
      line <- sf::st_cast(pair, "LINESTRING")
      return(line)
      })

     sfIndLns <- do.call(c, Linestrings) %>% # bind line segments into a linestring geometry set
      sf::st_sf() %>%
      dplyr::bind_cols(dfInd) # bind original attributes

    linesList[[i]] <- sfIndLns

    print(paste(which(uniqueInds == uniqueInds[i]), "OF", length(uniqueInds), "INDIVIDUALS COMPLETED", sep = " "))

  }

  # Bind together all individual lines
  linesFinal <- do.call(rbind, linesList)
  return(linesFinal)
}
