#' @title Report0r
#'
#' @description initiate data processing with:
#' analyz0r <- report0r(htmlReport = TRUE, exportCap = TRUE)
#'
#' @param htmlReport create a html report? TRUE/FALSE
#' @param exportCap export data.frames to .txt for further processing? TRUE/FALSE
#'
#' @return no return; saves a html report, .txt files of analysed data or both
#' @export
#'
#' @include reportGenerat0r.R Process0r.R
#'
#' @examples
#' \dontrun{
#'  analyz0r <- report0r(htmlReport = TRUE, exportCap = TRUE)
#'  }

report0r <- function(htmlReport = FALSE, exportCap = TRUE) {

                  if(htmlReport == FALSE && exportCap == FALSE) {
                    stop(
                      "Select an export format",
                      call. = FALSE
                    )
                  }

                  processedData <- process0r()

                  #export data as data report and/or txt files
                  for(i in 1:length(processedData)){

                    exp <- processedData[[i]]

                    if (!requireNamespace("viridis", quietly = TRUE) ||
                        !requireNamespace("rmarkdown", quietly = TRUE)) {
                      stop(
                        "Package \"viridis\" must be installed to use this function.",
                        call. = FALSE
                      )
                    }

                    #Generate html report using RMarkdown
                    if(htmlReport == TRUE){
                        reportGenerat0r(exp)
                    }

                    #Export data as .txt (Origin compatible)
                    if(exportCap == TRUE){
                        SaveStatsToOrigin(exp)
                    }
                      #SaveToOrigin.CCCV(outdir, exp)
                      #SaveToOrigin.VP(outdir, exp, cycles)

                  }

                  return(processedData)
              }
