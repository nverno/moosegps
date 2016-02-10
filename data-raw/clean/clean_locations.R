##/* setup.R --- 
## Filename: setup.R
## Description: Retrieve, setup GPS files
## Author: Noah Peart
## Created: Mon Feb  8 19:28:53 2016 (-0500)
## Last-Updated: Tue Feb  9 18:34:30 2016 (-0500)
##           By: Noah Peart
## */

## /* yaml */
##' ---
##' title: "Location Data"
##' output_format:
##'   html_document:
##'     toc: true
##' ---
##'
##+setup, echo=FALSE, include=FALSE
library(knitr)
opts_chunk$set(echo=TRUE, message=FALSE, fig.path='Figures/')
## /* end setup */

library(data.table)
library(readxl)       # GPS_ELEV is xlsx
library(DT)
library(leaflet)
library(htmltools)    # HTML
library(sp)           # SpatialLines
library(RColorBrewer) # brewer.pal
source('load_raw.R')  # to pull from afs if wanted
source('utils.R')     # 'see' and 'prettify'

## Any global options
dtopts <- list(scrollX=TRUE)

## /* end setup */
##'
##'
##' Pull the GPS related data from AFS and package.  None of the permanent plot locations
##' were interpolated.  Some of the transects and contour locations were, and the
##' indicator column `INTERP` is 1 in those cases.
##'
##' Note:
##' To get the docs this requires AFS access (lib 'nverno/sync.afs').
##'
##' ## Datasets
##' Location related-data:
##'
##' + __AFS Directory__: *GPS_ELEV*
##' + __Documentation__: *GPS data history.docx*
##' + __Data__: *PP_DEMSLOPE.csv*, *GPS_ALL.xlsx* (not on AFS)
##' *PP_DEMSLOPE.csv* has the same GPS locations, with additional slope estimates from 
##' the DEM (not accurate on our scale, but going to keep them for now anyway).
##'
##' Load data, take a look at the head of it:
##+load-raw
loc <- "../raw"

docs <- c('gps data history')
dats <- c('PP_DEMSLOPE.csv', 'GPS_ALL.xlsx')

ppdem <- fread(file.path(loc, dats[[1]]))
allDat <- read_excel(file.path(loc, dats[[2]]),
  col_types=c('numeric', 'text', 'numeric', 'text', 'numeric', 'text',
    'numeric', 'numeric', 'date', 'numeric', 'numeric', 'numeric'))
location <- as.data.table(allDat)

## Peek
datatable(location, options=dtopts)

## /* end load-raw */
##'
##' ## Get documentation from AFS
##' Skip this for now, already have the recent copies.
##+pull-docs
## load_docs(docs=docs)
## /* end pull-docs */
##'
##' --------------------------------------------------------------------------------
##'
##' ## Checks
##' Check that the pp_demslope locations are the same as those in gps_all.  The 
##' `POINT_X`/`POINT_Y` columns have the interpolated values, the `LAT`/`LONG` are 
##' recorded from the field.  According to the documentation, the point x and y columns
##' should match the lat/long columns where applicable. 
##'
##'
##+checks
## Check pplot locations in pp_demslope match those in gps_elev
chek <- location[ppdem, on=c('PPLOT')][, all.equal(POINT_X, i.LONG) & 
                                           all.equal(POINT_Y, i.LAT)]
chek2 <- location[!is.na(PPLOT), all.equal(POINT_X, ppdem$LONG)]
if (!chek || !chek2) stop('oops, locations dont match')

## Permanent plots should be all the same, no interpolation
pplots <- location[!is.na(PPLOT), ]
if (!(all.equal(pplots$LAT, pplots$POINT_Y) && 
        all.equal(pplots$LONG, pplots$POINT_X)))
  stop('Permanent plot locations should match interpolated values.')
if (!nrow(pplots) == 27)  # have a location for all the PPLOTS
  stop('Missing pplot locations in gps_all')

## Transects
tplots <- location[!is.na(TRAN), ]
chek1 <- nrow(tplots[is.na(TPLOT), ]) == 0L
ninterptp <- tplots[is.na(LAT), .N]  # 78 interpolated
ckek2 <- tplots[!is.na(LAT),][, all.equal(LAT, POINT_Y) && 
                                  all.equal(LONG, POINT_X)]
if (!chek1 || !chek2)
  stop('Transect location data not matching.')

## Others
others <- location[is.na(PPLOT) & is.na(TRAN) & is.na(CONTNAM), ]
nother <- nrow(others)  # 112 other locations
ninterpother <- others[is.na(LAT), .N]  # none interpolated

## Contours
cplots <- location[!is.na(CONTNAM), ]
ninterpcp <- cplots[is.na(LAT), .N]  # 335 interpolated

## Check all sampled contours have a location
load("~/work/seedlings/seedsub/data-raw/temp/cseed.rda")  # this will change
chek1 <- nrow(unique(cseed[,.(CONTNAM, STPACE)])[
  !unique(cplots[,.(CONTNAM, STPACE)]), on=c('CONTNAM', 'STPACE')])
if (chek1 > 0L)
  stop('Missing locations for some contours plots in the seedling data')

## Check if we have field sampled GPS locations for all of the 
## contour plots with seedling data:
## nope ->  56 interpolated
interped_cps <- unique(cseed[, .(CONTNAM, STPACE)])[
  !unique(cplots[!is.na(LAT), .(CONTNAM, STPACE)]), on=c('CONTNAM', 'STPACE')]

## /* end checks */
##'
##' ## Interpolated Plots
##' For each plot category the following number of plots were interpolated:
##'
##' + Permanent Plots: none
##' + Transect Plots: 
{{prettify(ninterptp)}}
##' + Contour Plots:
{{prettify(ninterpcp)}}
##' + Contour Plots where seedling data was collected:
{{prettify(nrow(interped_cps))}}
##' + Other locations:
{{prettify(ninterpother)}}
##'
##' There is a slight discrepancy in the documentation.  It says that all of the
##' location data from the field was incorporated into `POINT_X`/`POINT_Y`, however,
##' this isn't the case for the contour data, where the field-collected GPS points
##' weren't incorporated at all.
##'
##' ## Contour Map
##'
##' The following map shows the locations of the field-collected and interpolated 
##' contour data.
##'
##+contour-map
## Look at them with leaflet
ps <- cplots[!is.na(LAT), .(lng=LONG, lat=LAT, CONTNAM, STPACE,
  label=paste0(CONTNAM, '_', STPACE))]
interp <- cplots[, .(lng=POINT_X, lat=POINT_Y, CONTNAM, STPACE,
  label=paste0(CONTNAM, '_', STPACE))]

cols <- brewer.pal(3, name='Set1')
icons1 <- makeAwesomeIcon(icon = 'flag', markerColor = 'red',
  iconColor = 'black', library = 'fa')
icons2 <- makeAwesomeIcon(icon = '', markerColor = 'lightgreen',
  iconColor = 'black', library = 'fa')

## Make some sp::SpatialLines for contours
sls <- ps[, .(lines=list(Lines(list(Line(cbind(lng, lat))), ID=.BY[[1]]))), 
  by=c('CONTNAM')]
slines <- SpatialLines(sls[, lines])  # lines from collected data
ilines <- SpatialLines(interp[, 
  .(lines=list(Lines(list(Line(cbind(lng, lat))), ID=.BY[[1]]))), 
  by=c('CONTNAM')][, lines])          # interpolated lines

## Moosilauke coordinates
mooseView <- c('lng1'=-71.7548, 'lng2'=-71.90912, 
  'lat1'=43.95909, 'lat2'=44.05786)
mid <- c('lng'=mean(mooseView[1:2]), 'lat'=mean(mooseView[3:4]))
sl1names <- row.names(slines)
sl2names <- row.names(ilines)

## Make a map
m <- leaflet(interp) %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lng=mid[1], lat=mid[2], zoom=14) %>%
  addAwesomeMarkers(lng=~lng, lat=~lat, labelOptions(opacity=0.5),
    icon=icons2, group='Interp Points', label=~lapply(label, HTML)) %>%
  addAwesomeMarkers(data=ps, lng=~lng, lat=~lat, labelOptions(opacity=0.5),
    icon=icons1, group='GPS Points', label=~lapply(label, HTML)) %>%
  addPolylines(data=slines, group='GPS Lines', color=cols[1],
    popup=sl1names) %>%   # raw data lines
  addPolylines(data=ilines, group='Interp Lines', color=cols[3],
    popup=paste('Interp.', sl2names)) %>% # interpolated lines
  addLegend('topright', title='Data type', colors=cols[c(1, 3)], # colors=c('red', ''), 
    labels=c('GPS', 'Interpolated'), opacity=0.8) %>%
  hideGroup('Interp Points') %>%
  hideGroup('GPS Points') %>%
  addLayersControl(
    overlayGroups = c('GPS Points', 'GPS Lines', 'Interp Points', 'Interp Lines'),
    options=layersControlOptions(collapsed=FALSE)
  )
m

## /* end contour-map */
##'
##' ## Incorporate contour GPS
##'
##' Incorporate the interpolated points from `POINT_X`/`POINT_Y` into `LONG`/`LAT`,
##' and rename columns to `LAT`/`LNG`. Set the `INTERP` values for the contours with GPS 
##' data to NA.  Renaming `POINT_X`/`POINT_Y` to `DEM_LNG`/`DEM_LAT`.
##'
##+add-contour
location[!is.na(LONG), INTERP := NA]  # fix the INTERP values
location[is.na(LAT), LAT := POINT_Y]  # fill in LAT/LONG
location[is.na(LONG), LONG := POINT_X]
setnames(location, 'LONG', 'LNG')     # rename long
setnames(location, c('POINT_X', 'POINT_Y'), c('DEM_LNG', 'DEM_LAT'))
## /* end add-contour */
##'

##' ## Demslope
##' Adding demslope for now, even though it's pretty worthless at our scale.
##+demslope
res <- ppdem[, .(PPLOT, DEMSLOPE)][location, on='PPLOT']

## /* end demslope */
##'

##'
##' ## Plot types
##'
##' Add plot type variable called `PLOT`, with values:
##'
##' + `PERM`: Permanent Plot
##' + `TRAN`: Transect Plot
##' + `CONT`: Contour Plot
##' + `OTHER`: Other
##'
##+plot-type

res[, PLOT := ifelse(!is.na(PPLOT), 'PERM',
  ifelse(!is.na(TRAN), 'TRAN',
    ifelse(!is.na(CONTNAM), 'CONT', 'OTHER')))]

## /* end plot-type */

##'
##' ## Result Tables
##'
##' Some table of plots by types and numbers of interpolated locations.
##+result
datatable(res[, .(
  Total=.N, 
  `Num Interp`=sum(!is.na(INTERP)))
, by=PLOT], options=dtopts, caption='Table: Plots by type/Number interpolated.')
  
## Contours
datatable(res[PLOT == 'CONT', .(`Num Interp`=sum(!is.na(INTERP))), by=CONTNAM],
  options=dtopts, caption='Table: Interpolations by contour.')

## Transects
datatable(res[PLOT=='TRAN', .(`Num Interp`=sum(!is.na(INTERP))), by=TRAN],
  options=dtopts, caption='Table: Interpolations by transect.')

## /* end result */
##'

##'
##' ## Save
##'
##+save
## res <- res[order(PPLOT, TRAN, TPLOT, CONTNAM, STPACE, LABEL)]
## res[, PID := 1:.N]
## setcolorder(res, c(
##   'PID', 'PLOT', 'LNG', 'LAT', 'DATE', 'INTERP', 'DEM_LNG', 'DEM_LAT', # plot-type indep.
##   'PPLOT', 'DEMSLOPE', 'TRAN', 'TPLOT', 'CONTNAM', 'STPACE', 'LABEL'   # plot-type specific
## ))


## ## Outputs
## outs <- list2env(list(
##   allplots  = res,                                               # all locations bundled
##   location  = res[, 
##     .(PID, PLOT, LNG, LAT, DATE, INTERP, DEM_LNG, DEM_LAT)],     # plot-type indep.
##   permanent = res[PLOT=='PERM', .(PID, PPLOT, DEMSLOPE, LABEL)], # permanent plots
##   transect  = res[PLOT=='TRAN', .(PID, TRAN, TPLOT, LABEL)],     # transects
##   contour   = res[PLOT=='CONT', .(PID, CONTNAM, STPACE, LABEL)], # contour segments
##   other     = res[PLOT=='OTHER', .(PID, LABEL)]                  # others
## ))

## ## Save
## dir.create('temp')
## for (n in names(outs)) {
##   save(list=n, file=file.path('temp', paste0(n, '.rda')),
##     compress='bzip2', envir=outs)
## }

## /* end save */
