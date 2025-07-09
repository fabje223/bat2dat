#' @title Process0r
#'
#' @description Initiates import and analysis of a curated dataset
#'
#' @param cycles cycles to be extracted for voltage profiles
#' @param cccv was cccv cycling conducted? TRUE/FALSE
#'
#' @return returns a summary of all sample data, including metadata and raw file.
#' sampleSUMMARY is a list of one or more samples stated in meta.csv file (=cell log).
#' @export
#'
#' @include Report0r.R Direct0r.R Read0r2.R
#'
#' @examples
#' \dontrun{
#' l <- process0r()
#' }
process0r <- function(cccv = FALSE, cycles) {

              #Select (optional)
              #for voltage profiles: which cycles shall be extracted?
              #cycles <- c(0,1,2,4,9,14,19,24,49,74,99) #c(0, seq(1,100,5))
              #cycles <- c(0,1,2,4,9,12,17,20,25,27) # Rate Capability Test
              #cycles <- c(0:27)

              #locate experimental data
              meta <- metaDir()

              #creates log for warning messages
              warningsLOG <- data.frame(
                              "script" = character(),
                              "section" = character(),
                              "message" = character()
              )

              #read-in raw data from folder
              sampleSUMMARY <- lapply(1:nrow(meta), function(i) {

                          #initialize/empty l.sample list()
                          l.samples <- list("metadata"=NULL,
                                            "rawdata"=NULL,
                                            "capacity"=NULL,
                                            "VoltageProfiles"=NULL,
                                            "CCCV"=NULL)

                          #initializing data.frames() for raw and processed raw data
                          raw <- data.frame()
                          rawEval <- data.frame()

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
                            #path/to/file/filename.res --> check is .res file in directory
                            res <- paste0(meta$dir[i], "/", meta$sample.name[i], ".res")
                            accdb <- paste0(meta$dir[i], "/", meta$sample.name[i], ".accdb")
                            xlsx <- paste0(meta$dir[i], "/", meta$sample.name[i], ".xlsx")

                            if(file.exists(xlsx)){

                                  f.path <- paste0(meta$dir[i], "/", meta$sample.name[i], ".xlsx")
                                  raw <- ARBINrawXLSX(dir, f.path)
                                  rawEval <- ArbinEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i], cycles)

                            #check if file has .res ending; if so, rename them to .accdb
                            }else if(file.exists(res)){

                                  print('.res interpreter currently not functional')
                                  raw <- NULL

                                  #newfile <- gsub(".res$", ".accdb", res)
                                  #file.rename(res, newfile)
                                  #raw <- ARBINrawACCDB(newfile)

                            }else if(file.exists(accdb)){

                                  raw <- NULL
                                  rawEval <- NULL
                                  #raw <- ARBINrawACCDB(accdb)
                                  #rawEval <- ArbinEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i], cycles)
                            }


                          }else{

                            print("cycler not found - check directory")

                            raw <- NULL
                            rawEval <- NULL
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

