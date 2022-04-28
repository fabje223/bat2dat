#Load meta data and raw data files

meta.dir <- function() {

          #pop-up window
          file.dir <- file.choose()

          #get dir
          dir <- dirname(file.dir)

          #read-in meta data file
          meta <- read.csv(paste0(dir, "/", 'meta.csv'), header=TRUE, sep=',', quote="\"", dec=".", fill=TRUE)

          #output folder
          outdir = paste0(dir, "/Rprocessed")

          dir.info <- list(
                            "dir" = dir,
                            "meta" = meta,
                            "outdir" = outdir
                            )

          return(dir.info)
}
