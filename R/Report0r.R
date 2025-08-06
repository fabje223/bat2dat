#' @title Report0r
#'
#' @description initiate data processing with:
#' analyz0r <- report0r(htmlReport = TRUE, exportCap = TRUE)
#'
#' @param cccv perform a CC-CV step analysis? yes(=TRUE)/no(=FALSE)
#' @param cycles cycle numbers to be extracted to plot voltage profiles
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

report0r <- function(cycles = c(0,1,4,seq(9,199, 10)), htmlReport = TRUE, exportCap = TRUE) {

            print('Hello there! Let us analyse some data, shall we? Show the way to your experimental data...')

                  if(htmlReport == FALSE && exportCap == FALSE) {
                    stop(
                      "Select an export format",
                      call. = FALSE
                    )
                  }

                  processedData <- process0r(cycles)

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

                      #no data no report
                      if(is.null(exp$rawdata)) {
                        print(paste0('no report generated for cell ', exp$metadata[2], ', as raw data could not be obtained'))
                      }else{
                        reportGenerat0r(exp)
                      }
                    }

                    #Export data as .txt (Origin compatible)
                    if(exportCap == TRUE){
                        SaveStatsToOrigin(exp)
                        SaveVPToOrigin(exp)
                    }
                      #SaveToOrigin.CCCV(outdir, exp)


                  }

                  print('all jobs done!')
        }
