#' @title reportGenerat0r
#'
#' @description Interface to plot data and pass it to report.Rdm to
#' generate data report
#'
#' @param exp sample info
#'
#' @return initiates data report function report.Rmd and generates plots for report using plot0r.R
#' @export
#'
#' @include Report0r.R customThemes.R plot0r.R
#'
#' @examples
#' \dontrun{
#' exp <- list("metadata" <- base.info,
#'             "capacity" <- data.frame,
#'             "VoltageProfiles" <- list(df1, df2, ...),
#'             "CCCV" <- data.frame)
#' report <- reportGenerat0r(exp)
#' }
reportGenerat0r <- function(exp){

  meta <- exp$metadata

  #draw capacity vs. cycle number
  capacity <- exp$capacity
  if(max(capacity$cycNr) >= 1){

    pCap <- plotCapReport(capacity)
    pIRdrop <- plotIRdrop(capacity)

  }else{

    pCap <- NA
    pIRdrop <- NA

    }

  #draw voltage profiles
  l.VP <- exp$VoltageProfiles
  if(!is.null(l.VP)) {

    VP.df <- do.call(rbind, l.VP)

    pVPloop <- plotVPloop(VP.df)
    pVPlin  <- plotVPlin(VP.df)
    pVPsplit  <- plotVPsplit(VP.df, meta$cell.config)

  }else{

    pVPloop <- NA

    }

  #locate report.rmd file
  file <- system.file("rmd", "report.Rmd", package = "bat2dat")

  #background details
  reportName <- c(paste0("DataReport_", meta$sample.name, ".html"))
  outdir <- meta$outdir

  #Generate Report in Rmarkdown
  rmarkdown::render(
              file,
              params = list(metavar = meta,
                            cap = capacity,
                            plotCap = pCap,
                            plotIR = pIRdrop,
                            plotVPloop = pVPloop,
                            plotVPlin = pVPlin,
                            plotVPsplit = pVPsplit),
              output_file = reportName,
              output_dir = outdir
              )
  }
