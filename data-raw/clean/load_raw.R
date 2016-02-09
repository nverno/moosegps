### load_raw.R --- 
## Filename: load_raw.R
## Description: 
## Author: Noah Peart
## Created: Mon Feb  8 19:31:01 2016 (-0500)
## Last-Updated: Mon Feb  8 19:31:11 2016 (-0500)
##           By: Noah Peart
######################################################################
load_data <- function(files) {
  res <- lapply(files, sync.afs::get_data)
  names(res) <- files
  res
}

setup_data <- function(files, basedir='.') {
  dat <- load_data(files)
  
  ## Normalize master files to all uppercase variables
  for(d in dat) setnames(d, names(d), toupper(names(d)))
  e <- new.env()
  list2env(dat, e)

  ## Add boilerplate descriptions
  descs <- paste(rep(c('Transect', 'Contour'), each=2),
    rep(c('seedling', 'substrate'), times=2), 'data')
  for(i in 1:4) {
    n <- names(dat)[[i]]
    ## Add documentation file for roxygen
    datadoc::describe(n, text=descs[i], 
      outfile=file.path(basedir, 'R', paste0(n, '.R')), 
      add_tables=TRUE, envir=e)
    
    ## save in data
    save(list=n, envir=e,
      file=file.path(basedir, 'data', paste0(n, '.rda')), 
      compress='bzip2')
  }
  invisible()
}

load_docs <- function(docs) {
  if (!exists('afs')) afs <- sync.afs::AFS$new()
  if (!afs$connected()) afs$signin()
  if (!file.exists('inst'))
    dir.create('inst')
  if (!file.exists('inst/docs'))
    dir.create('inst/docs')
  paths <- sync.afs::get_key()[list(rname=docs), afs_path, on='rname']
  docnames <- basename(paths)
  paths <- file.path(afs$path, paths)
  res <- for (i in seq_along(docs)) {
    if (!file.exists(paths[[i]])) stop(sprintf("Can't find %s", paths[[i]]))
    file.copy(from=paths[[i]], to=file.path('inst', 'docs', docnames[[i]]))
  }
}
