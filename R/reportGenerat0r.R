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

  type <- NULL
  meta <- exp$metadata
  cellType <- meta$cell.config

  #draw capacity vs. cycle number
  capacity <- exp$capacity
  if(max(capacity$CycNr) >= 2){

    pCap <- plotCapa(capacity)
    pCE <- plotCE(capacity)

    #Plot IR drop
    if(cellType %in% c('halfcell-cathode', 'fullcell')){

      minmax <- c("min.y.ch" <- min(capacity$Edrop.ch),
                  "max.y.ch" <- max(capacity$Edrop.ch),
                  "min.y.dc" <- min(capacity$Edrop.dc),
                  "max.y.dc" <- max(capacity$Edrop.dc))

      # workaround: no resting step between charge and discharge
      # prevents error "from must be a finite number" in scale_y_continuous
      minmax[is.na(minmax)] <- 0
      minmax[is.infinite(minmax)] <- 0

      #print('plotting cathode/full cell data')
    }else if(cellType %in% c('halfcell-anode', 'LiS')){

      minmax <- c("min.y.ch" <- min(capacity$Edrop.ch),
                  "max.y.ch" <- max(capacity$Edrop.ch),
                  "min.y.dc" <- min(capacity$Edrop.dc),
                  "max.y.dc" <- max(capacity$Edrop.dc))

      # workaround: no resting step between charge and discharge
      # prevents error "from must be a finite number" in scale_y_continuous
      minmax[is.na(minmax)] <- 0
      minmax[is.infinite(minmax)] <- 0

      #print('plotting anode data')
    }else {
      print('unknown celltype')
      stop()
    }

      #No plot if all minmax == 0
      if(sum(minmax) == 0){

          pIRdropDC <- NA
          pIRdropCH <- NA

        }else{

        pIRdropDC <- plotIRdropDC(capacity, minmax)
        pIRdropCH <- plotIRdropCH(capacity, minmax)
        }
  }

  #Plot Internal Resistance (IntR)
  capacity <- exp$capacity
  if(max(capacity$CycNr) >= 2){

    minmax.IntR <- c("min.y.ch" <- min(capacity$IntR.ch),
                    "max.y.ch" <- max(capacity$IntR.ch),
                    "min.y.dc" <- min(capacity$IntR.dc),
                    "max.y.dc" <- max(capacity$IntR.dc))

    # workaround: no resting step between charge and discharge
    # prevents error "from must be a finite number" in scale_y_continuous
    minmax.IntR[is.na(minmax.IntR)] <- 0
    minmax.IntR[is.infinite(minmax.IntR)] <- 0

    #No plot if all minmax == 0
    if(sum(minmax.IntR) == 0){

        pIntRCH <- NA
        pIntRDC <- NA

        }else{

        pIntRDC <- plotIntRDC(capacity, minmax.IntR)
        pIntRCH <- plotIntRCH(capacity, minmax.IntR)
        }
  }

  #draw voltage profiles
  l.VP <- exp$VoltageProfiles
  if(!is.null(l.VP)) {

    VP.df <- do.call(rbind, l.VP)

    pVPloop <- plotVPloop(VP.df)
    pVPlin  <- plotVPlin(VP.df)

    #binding variables locally to function plotVPloop
    if(cellType %in% c('halfcell-cathode', 'fullcell')){

      ch <- VP.df %>%
              filter(type == 'ch')
      dc <- VP.df %>%
              filter(type == 'dc')

      min.dc.y <- round(min(dc$Ewe.V),1)-0.1
      max.dc.y <- round(max(dc$Ewe.V),1)+0.1
      min.ch.y <- round(min(ch$Ewe.V),1)-0.1
      max.ch.y <- round(max(ch$Ewe.V),1)+0.1
      #print('plotting cathode/full cell data')

    }else if (cellType %in% c('halfcell-anode', 'LiS')){

      ch <- VP.df %>%
              filter(type == 'ch')
      dc <- VP.df %>%
              filter(type == 'dc')

      min.dc.y <- round(min(dc$Ewe.V),1)-0.1
      min.ch.y <- round(min(ch$Ewe.V),1)-0.1
      max.ch.y <- round(max(dc$Ewe.V),1)+0.1
      max.dc.y <- max.ch.y
      #print('plotting anode data')

    }else {

      print('unknown celltype')
      stop()
    }

    pVPsplitCH  <- plotVPsplitCH(ch, min.ch.y, max.ch.y)
    pVPsplitDC  <- plotVPsplitDC(dc, min.dc.y, max.dc.y)

  }else{

    pVPloop <- NA
    pVPlin  <- NA
    pVPsplit  <- NA

  }

  #save all plots in plotList
  plotList <- list('capa' = pCap, #               [1]
                   'CE' = pCE, #                  [2]
                   'IR.ch' = pIRdropCH, #         [3]
                   'IR.dc' = pIRdropDC, #         [4]
                   'IntR.ch' = pIntRCH, #         [5]
                   'IntR.dc' = pIntRDC, #         [6]
                   'VPloop' = pVPloop, #          [7]
                   'VPlin' = pVPlin, #            [8]
                   'VPsplitCH' = pVPsplitCH, #    [9]
                   'VPsplitDC' = pVPsplitDC #     [10]
                    )

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
                            pList = plotList
                            ),
              output_file = reportName,
              output_dir = outdir
              )

  }
