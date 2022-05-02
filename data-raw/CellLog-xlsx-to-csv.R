#' @title convertLabNotes
#'
#' @description A routine to convert lab notes from .xlsx or digital lab notebooks
#' into a metadata (.csv) file for import into 'bat2dat'
#'
#' @return writes a new data table in .csv format
#' @export
#'
#' @importFrom utils write.table
#' @importFrom magrittr %>%
#' @name %>%
#' @import dplyr readxl
#'
#' @examples
#' \dontrun{
#' convertLabNotes(example=FALSE)
#' }
convertLabNotes <- function(example=FALSE) {

                    #select directory
                    fileDir <- file.choose()
                    dirName <- dirname(fileDir)

                    #read CellLog
                    cellLog <- read_excel(fileDir,
                                          sheet = 1, range="A16:Z56")

                    #size depends on number of samples
                    cellLog <- cellLog[,1:17]

                    #select parameters for R analysis
                    col <- c("Identifier", "sample name", "instrument", "cell config", "AM loading")
                    meta <- cellLog %>%
                              filter(metadata %in% col)

                    #modify transposed data.frame before convertion into .csv
                    meta.t <- t(meta)
                    meta.t <- meta.t[5:nrow(meta.t), 1:ncol(meta.t)]
                    row.names(meta.t) <- 1:nrow(meta.t)
                    colnames(meta.t) <- c(col)
                    meta.t[,5] <- as.numeric(meta.t[,5])

                    #write .csv file
                    write.table(meta.t, paste0(dirName, '/exampleMeta.csv'), dec=",", sep="\t", row.names=FALSE)

                    if(example == TRUE){
                          #create example metadata file in /data
                          exampleMeta <- meta.t
                          usethis::use_data(exampleMeta)
                    }
}
