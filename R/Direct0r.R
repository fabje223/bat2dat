#' @title Direct0r
#'
#' @description Locate folder with meta data and raw data
#'
#' @return returns a data.frame with metadata on samples and an input & output directory
#' @export
#'
#' @include Report0r.R Process0r.R
#' @importFrom utils read.csv
#'
#' @examples
#'
metaDir <- function() {

  #pop-up window
  file.dir <- file.choose()

  #fetch dir
  dir <- dirname(file.dir)

  #output folder
  outdir = paste0(dir, "/Rprocessed")
  if(!dir.exists(outdir)) dir.create(outdir)

  #read-in meta data file
  meta <- read.csv(file.dir,
                   header=TRUE,
                   sep='\t', quote="\"", dec=".",
                   stringsAsFactors = F,
                   fill=TRUE)

  #sort out any sample that is not included in the data-raw folder
  l.raw <- list.files(dir)
  boo <- c()
  for(x in 1:nrow(meta)){

    a <- any(startsWith(l.raw, c(meta$sample.name[x])) == TRUE)
    boo <- c(boo, a)

    if(a == FALSE){
      print(paste0("Sample: ", meta$sample.name[x], " not found in raw data folder"))
    }
  }
  meta <- meta[boo,]
  meta$dir <- dir
  meta$outdir <- outdir

  return(meta)
}
