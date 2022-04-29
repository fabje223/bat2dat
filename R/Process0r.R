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

              #filename <- as.character(meta[i,2])
              #AM.mass <- as.numeric(meta[i,5])/1000
              #cycler <- as.character(meta[i,3])
              #type <- meta[i,4]

              #read-in raw data from folder
              if(meta$cycler == "Biologic BCS"){

                print("Reading BCS raw data")
                raw <- BCSraw(dir, meta)

              }else if(meta$cycler == "Biologic VMP"){

                print("Reading VMP raw data")
                raw <- VMPraw(dir, meta)

              }else if(meta$cycler == "Arbin") {

                print("Reading Arbin raw data")

                #check if .res files are in directory; if so, rename them to .accdb
                files = list.files(pattern = ".*.res")
                if(length(files) != 0 && cycler == 'Arbin'){

                      res <- paste0(filename, ".res")
                      newfiles <- gsub(".res$", ".accdb", res)
                      file.rename(res, newfiles)
                }

                raw <- ARBINraw(dir, meta)

              }else{

                print("cycler not found - check directory")

                cyc.dat <- list('cell.data' = NULL,
                                'capacity' = NULL,
                                'VoltageProfiles' = NULL,
                                'CCCV' = NULL,
                                'raw' = NULL)
              }


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

