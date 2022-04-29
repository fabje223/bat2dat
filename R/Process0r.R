#Load meta data and raw data files

meta.dir <- function() {

          #pop-up window
          file.dir <- file.choose()

          #get dir
          dir <- dirname(file.dir)

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
          }
          meta <- meta[boo,]

          #output folder
          outdir = paste0(dir, "/Rprocessed")

          sample.info <- list(
                            "dir" = dir,
                            "meta" = meta,
                            "outdir" = outdir
                            )

          return(sample.info)
}

process0r <- function() {

              #Select (optional)
              #for voltage profiles: which cycles shall be extracted?
              cycles <- c(0,1,2,4,9,14,19,24,49,74,99) #c(0, seq(1,100,5))
              #cycles <- c(0,1,2,4,9,12,17,20,25,27) # Rate Capability Test
              #cycles <- c(0:27)

              #locate experimental data
              sample.origin <- meta.dir()
              dir <- sample.origin$dir
              meta <- sample.origin$meta

              #convert AM.mass [mg] into g
              meta$AM.loading <- meta$AM.loading/1000

              sampleSUMMARY <- lapply(1:nrow(meta), function(i) {

                          #read-in raw data from folder
                          if(meta$instrument[i] == "Biologic BCS"){

                            print("Reading BCS raw data file")
                            raw <- BCSraw(dir, meta$sample.name[i])

                          }else if(meta$instrument[i] == "Biologic VMP"){

                            print("Reading VMP raw data file")
                            raw <- VMPraw(dir, meta$sample.name[i])

                          }else if(meta$instrument[i] == "Arbin") {

                            print("Reading Arbin raw data file")

                            #check if file has .res ending; if so, rename them to .accdb
                            if(file.exists(paste0(dir, "/", meta$sample.name[i], ".res"))){

                                  res <- paste0(meta$sample.name[i], ".res")
                                  newfile <- gsub(".res$", ".accdb", res)
                                  file.rename(res, newfile)
                            }

                            raw <- ARBINraw(dir, meta$sample.name)

                          }else{

                            print("cycler not found - check directory")

                            raw <- NULL
                          }

                l.sample <- list("metadata" = meta[i,], "rawdata" = raw)
                return(l.sample)
              })


              #start analysis
              eval.raw <- analysis(dir, cycles, meta)

              #++++ under construction ++++
              #calculate capacities for each cycle
              capacity <- Biologic.CAP(raw, AM.mass, type)
              #extract voltage profiles for selected cycles
              VPprofiles <- Biologic.VP(raw, AM.mass, cycles, type)
              #detailed analysis of CC-CV steps
              CCCV <- Biologic.CCCV(raw, AM.mass, type)

              cyc.dat <- list('cell.data' = meta[i,],
                              'capacity' = capacity,
                              'VoltageProfiles' = VPprofiles,
                              'CCCV' = CCCV,
                              'raw' = raw)




              return(eval.raw)
}

