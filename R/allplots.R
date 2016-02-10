##' Location data for Moosilauke plots and plot-type information bundled together.
##'
##' Location data for Moosilauke plots of all types bundled together.  This includes
##' locations for permanent plots, transect plots, contour segment plots, and other
##' assorted plots.  Some of the transect and contour plot locations were interpolated
##' where field GPS data wasn't available.  This dataset also includes the variables
##' specific to each of the plot types.  So, it is all the other datasets bundled into 
##' one.
##'
##' @format An object of class \code{data.table} (inherits from
##'   \code{data.frame}) with 888 rows and 15 columns.
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
##'   \item PPLOT: Permanent plot ID.
##'   \item DEMSLOPE: Slope of permanent plots determined from DEM data.
##'   \item TRAN: Transect id
##'   \item TPLOT: Transect plot number
##'   \item CONTNAM: Contour name
##'   \item STPACE: Starting pace of contour plot.
##'   \item LABEL: More information about a plot location.  Mostly just relevant
##' for plots in the 'OTHER' category.
##' }
##'
"allplots"


