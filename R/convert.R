## Functions to help with spatial object output
## Contours, transects, permanent plots, other random plots

##' Convert contour data to SpatialLines
##'
##' Only supports making SpatialLines for contour plots and transects.
##'
##' @param data Input data
##' @param type Type of plot ("CONT"=contour, "TRAN"=transec, "PERM"=permanent)
##' @export
make_lines <- function(data, type=c("CONT", "TRAN", "PERM")) {
  if (!requireNamespace('sp', quietly = TRUE)) {
    warning("This function requires package 'sp'")
    return(invisible())
  }
  type <- match.arg(type, c("CONT", "TRAN", "PERM"))
  pdat <- switch(type, "CONT"=contour, "TRAN"=transect, "PERM"=permanent)
  dat <- data[PLOT == type, ][pdat, on="PID"]  # merge plot-level info

  if (type %in% c("CONT", "TRAN")) {
    main <- switch(type, "CONT"="CONTNAM", "TRAN"="TRAN")
    minor <- switch(type, "CONT"="STPACE", "TRAN"="TPLOT")
    ps <- dat[, .(lng=LNG, lat=LAT, main=get(main), minor=get(minor), 
      label=paste0(get(main), '_', get(minor)))]
    slines <- sp::SpatialLines(ps[,
      .(lines=list(sp::Lines(list(sp::Line(cbind(lng, lat))), ID=.BY[[1]]))),
      by=main][, lines])
    return( slines )
  } else if (type == "PERM") {
    warning("Not implemented")
    return()
  }
}

