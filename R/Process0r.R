#Load meta data and raw data files

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

process0r <- function(cycles = c(seq(0, 100, 10)), cccv = FALSE) {

              #Select (optional)
              #for voltage profiles: which cycles shall be extracted?
              #cycles <- c(0,1,2,4,9,14,19,24,49,74,99) #c(0, seq(1,100,5))
              #cycles <- c(0,1,2,4,9,12,17,20,25,27) # Rate Capability Test
              #cycles <- c(0:27)

              #locate experimental data
              meta <- metaDir()

              #convert AM.mass [mg] into g
              meta$AM.loading <- meta$AM.loading/1000

              #creates log for warning messages
              warningsLOG <- data.frame(
                              "script" = character(),
                              "section" = character(),
                              "message" = character()
              )

              #read-in raw data from folder
              sampleSUMMARY <- lapply(1:nrow(meta), function(i) {

                          if(meta$instrument[i] == "Biologic BCS"){

                            print("Reading BCS raw data file")
                            raw <- BCSraw(meta$dir[i], meta$sample.name[i])

                            rawEval <- BiologicEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i],
                                                         cycles, cccv, warningsLOG)

                          }else if(meta$instrument[i] == "Biologic VMP"){

                            print("Reading VMP raw data file")
                            raw <- VMPraw(meta$dir[i], meta$sample.name[i])

                            rawEval <- BiologicEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i], cycles, cccv)

                          }else if(meta$instrument[i] == "Arbin") {

                            print("Reading Arbin raw data file")

                            #check if file has .res ending; if so, rename them to .accdb
                            if(file.exists(paste0(meta$dir[i], "/", meta$sample.name[i], ".res"))){

                                  res <- paste0(meta$sample.name[i], ".res")
                                  newfile <- gsub(".res$", ".accdb", res)
                                  file.rename(res, newfile)
                            }

                            raw <- ARBINraw(dir, meta$sample.name)

                            rawEval <- ArbinEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i], cycles)

                          }else{

                            print("cycler not found - check directory")

                            raw <- NULL
                          }

                l.sample <- list("metadata" = meta[i,],
                                 "rawdata" = raw,
                                 "capacity" = rawEval$capacity,
                                 "VoltageProfiles" = rawEval$VoltageProfiles,
                                 "CCCV" = rawEval$CCCV)

                print(paste0('Data analysis of file ', meta$sample.name[i], " finished"))

                return(l.sample)
              })

              return(sampleSUMMARY)
}

