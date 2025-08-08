#' @title renderReport
#'
#' @description Interface to plot data and pass it to report.Rdm to
#' generate data report
#'
#' @param exp sample info
#' @param sample cell data of ONE cell (list); can only process one cell at a time
#' @param RmdFile name of RMarkdown Report template
#'
#' @return initiates data report function report-new.Rmd. Style of RMarkdown Report could be adjusted as needed: edit RmdFile in renderReport(RmdFile = 'filename') and add RMarkdown file to inst > rmd folder of package
#' @export
#'
#' @include Report0r.R theme1.R theme2.R plot0r.R
#'
#' @examples
#' \dontrun{
#' exp <- list("metadata" <- base.info,
#'             "capacity" <- data.frame,
#'             "VoltageProfiles" <- list(df1, df2, ...)
#'             )
#' renderReport(exp)
#' }
#'
renderReport <- function(sample = exp, RmdFile = 'report-new.Rmd'){

  meta <- sample$metadata

  #locate report.rmd file
  file <- system.file("rmd", RmdFile, package = "bat2dat")

  #background details
  reportName <- c(paste0("DataReport_", meta$sample.name, ".html"))
  outdir <- meta$outdir

  #Generate Report in Rmarkdown
  rmarkdown::render(
    file,
    params = list(sample = sample
                  ),
    output_file = reportName,
    output_dir = outdir
  )

}
