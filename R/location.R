##' Location data for Moosilauke plots
##'
##' Location data for Moosilauke plots of all types bundled together.  This includes
##' locations for permanent plots, transect plots, contour segment plots, and other
##' assorted plots.  Some of the transect and contour plot locations were interpolated
##' where field GPS data wasn't available.  It doesn't contain plot identifiers specific
##' to the different plot types.  To retrieve that information, use \code{PID} to 
##' access it from the accompanying datasets.
##'
##' @format An object of class \code{data.table} (inherits from
##'   \code{data.frame}) with 888 rows and 8 columns.
##' \itemize{
##'   \item PID: Plot ID to link to other datasets in this package.
##'   \item PLOT: Type of plot
##'   \itemize{
##'     \item PERM: Permanent plot 
##'     \item TRAN: Transect plot
##'     \item CONT: Contour plot
##'     \item OTHER: Other type
##'   }
##'   \item LNG/LAT: Longitude/Latitude, includes interpolated values when no 
##' field measurements were available.  This is indicated by \code{INTERP}.
##'   \item DATE: Date of measurement when available.
##'   \item INTERP: 1 if location was interpolated.
##'   \item DEM_LNG/DEM_LAT: Interpolated longitude/latitude values.
##' }
##'
"location"
