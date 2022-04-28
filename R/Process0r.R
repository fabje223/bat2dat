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

process0r <- function() {

              #Select (optional)
              #for voltage profiles: which cycles shall be extracted?
              cycles <- c(0,1,2,4,9,14,19,24,49,74,99) #c(0, seq(1,100,5))
              #cycles <- c(0,1,2,4,9,12,17,20,25,27) # Rate Capability Test
              #cycles <- c(0:27)

              #locate experimental data
              dir.info <- meta.dir()
              dir <- dir.info$dir
              meta <- dir.info$meta

              #start analysis
              eval.raw <- analysis(dir, cycles, meta)

              return(eval.raw)
}

