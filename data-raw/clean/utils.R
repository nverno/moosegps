### utils.R --- 
## Filename: utils.R
## Description: thingy things
## Author: Noah Peart
## Created: Mon Feb  8 21:14:31 2016 (-0500)
## Last-Updated: Mon Feb  8 22:44:02 2016 (-0500)
##           By: Noah Peart
######################################################################
##' Make nice for markdown
##' 
##' @param x input vector
##' @param type 'each' wraps each element in ``, 'all' wraps the whole vector in ``
##' @export
prettify <- function(x, type=c('each', 'all')) {
  tt <- match.arg(type, c('each', 'all'))
  if (tt == 'each') {
    gsub('([[:alnum:]]+)', '`\\1`', toString(x))
  } else if (tt == 'all') {
    sprintf('`%s`', toString(x))
  }
}

##' Render and browse the file
##' 
##' @param file File name
##' @param format Optional output format
##' @export
see <- function(file, format) {
  if (!file.exists(file)) stop('Cant find file')
  out <- if (missing(format)) {
    rmarkdown::render(file)
  } else rmarkdown::render(file, output_format=format)
  browseURL(out)
}
