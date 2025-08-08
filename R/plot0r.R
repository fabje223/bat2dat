#' @title plot0r
#'
#' @description Plotting Script for RMarkdown Report
#'
#' @param cell data.frame with galvanostatic cycling information (Voltage Profiles or Cycling Data)
#' @param minmax y-axis bounds
#'
#' @return returns graphs generated from expeirmental data for report.Rmd
#'
#' @include theme1.R theme2.R renderReport.R
#' @import ggplot2 viridis
#' @importFrom gridExtra grid.arrange
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' capa <- Arbin.CAP()
#' pCap <- plotCapa(capa)
#' }
#'
#' @export
#' @rdname plot0r
#' @details plots of galvanostatic cycling (GC) experiments (Q vs. CycNr & CE vs. CycNr)
plotGC <- function(cell){

      #binding variables locally to function plotCapReport
      p.cap <- p.CE <- NULL
      CycNr <- capacity <- Qdc.mAh.g <- CE <- NULL

      capacity <- cell$capacity

      #Plot capacity
      p.cap <- ggplot(capacity) +
        geom_point(aes(x=CycNr, y=Qdc.mAh.g), color='red', size=4) +
        labs(x = bquote('cycle number'),
             y = bquote('capacity / mAh/g'),
             title = "a) Galvanostatic Cycling") +
        #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
        #                  breaks = seq(0,200, 10)) +
        scale_y_continuous(limits=c(0,max(capacity$Qdc.mAh.g)),
                          #breaks = seq(0, 5000, 50)) +
                          breaks = waiver(), n.breaks = 10) +
        #     breaks = seq(0,4000, 0.2)) +
        #my.axis +
        theme2()
        #my.legend #theme(legend.position = 'none')

      #Plot Coulombic Efficiency
      p.CE <- ggplot(capacity) +
        geom_point(aes(x=CycNr, y=CE*100), color='red', size=3) +
        labs(x = bquote('cycle number'),
             y = bquote('C.E.'),
             title = "b) Coulombic Efficiency") +
        #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
        #                  breaks = seq(0,200, 10)) +
        scale_y_continuous(limits=c(40,103),
                          breaks = seq(0, 200, 5)) +
        #     breaks = seq(0,4000, 0.2)) +
        #my.axis +
        theme2() +
        theme(legend.position = 'none')

      return(list('capa' = p.cap, 'CE' = p.CE))

}

#' @export
#' @rdname plot0r
#' @details plotIRdrop plots IR drop versus cycle number (charge)
plotIRdrop <- function(cell){

  #binding variables locally to function plotCapReport
  CycNr <- cellType <- capacity <- meta <- Edrop.ch <- Edrop.dc <- NULL

  capacity <- cell$capacity
  meta <- cell$metadata
  cellType <- meta$cell.config

  if(max(capacity$CycNr) <=2) {
    print('this cell performed less than 2 cycles. Plotting aborted')
    return(list('ch.drop'= NA, 'dc.drop' = NA))
  }
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
    return(list('ch.drop'= NA, 'dc.drop' = NA))
  }

  p.IRdrop.ch <- ggplot(capacity) +
                    geom_point(aes(x=CycNr, y=Edrop.ch*1000), color='red', size=4) +
                    labs(x = bquote('cycle number'),
                         y = bquote('IR drop / mV'),
                         title = "IR drop (charge) vs. cycle number",
                         color = "Legend") +
                    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
                    #                  breaks = seq(0,200, 10)) +
                    scale_y_continuous(limits=c(minmax[1]*900, minmax[2]*1100),
                                       #breaks = round(seq(minmax[1]*1200, minmax[2]*800, length.out=50), 0)) +
                                       breaks = waiver(), n.breaks = 10) +
                    #     breaks = seq(0,4000, 0.2)) +
                    #my.axis +
                    theme2() +
                    theme(legend.position = 'right')

  p.IRdrop.dc <- ggplot(capacity) +
                    geom_point(aes(x=CycNr, y=Edrop.dc*1000), color='blue', size=4) +
                    labs(x = bquote('cycle number'),
                          y = bquote('IR drop / mV'),
                          title = "IR drop (discharge) vs. cycle number",
                          color = "Legend") +
                    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
                    #                  breaks = seq(0,200, 10)) +
                    scale_y_continuous(limits=c(minmax[3]*900, minmax[4]*1100),
                                      #breaks = round(seq(minmax[3]*1200, minmax[4]*800, length.out=10), 0)) +
                                      breaks = waiver(), n.breaks = 10) +
                    #my.axis +
                    theme2() +
                    theme(legend.position = 'right')

  return(list('ch.drop'= p.IRdrop.ch, 'dc.drop' = p.IRdrop.dc))
}


#' @export
#' @rdname plot0r
#' @details plotIntR plots internal resistance versus cycle number (charge)
  plotIntR <- function(cell, minmax=c(0, 1000, 0, 1000)){

    #binding variables locally to function plotCapReport
    CycNr <- IntR.ch <- IntR.dc <- NULL

    capacity <- cell$capacity
    if(max(capacity$CycNr) >= 2){

      minmax.IntR <- c("min.y.ch" <- min(capacity$IntR.ch),
                       "max.y.ch" <- max(capacity$IntR.ch),
                       "min.y.dc" <- min(capacity$IntR.dc),
                       "max.y.dc" <- max(capacity$IntR.dc))

      # workaround: no resting step between charge and discharge
      # prevents error from "must be a finite number" in scale_y_continuous
      minmax.IntR[is.na(minmax.IntR)] <- 0
      minmax.IntR[is.infinite(minmax.IntR)] <- 0

    }else{

      print('this cell performed less than 2 cycles. Plotting aborted')
      return(list('IntR.ch'= NA, 'IntR.dc' = NA))

    }
      #No plot if all minmax == 0
      if(sum(minmax.IntR) == 0){

        print('values returned an internal resistance of 0. Figure skipped')
        return(list('IntR.ch'= NA, 'IntR.dc' = NA))

      }

    p.IntR.ch <- ggplot(capacity) +
      geom_point(aes(x=CycNr, y=IntR.ch), color='red', size=4) +
      labs(x = bquote('cycle number'),
           y = bquote('Internal Resistance / Ohm'),
           title = "Internal Resistance (end of charge) vs. cycle number",
           color = "Legend") +
      #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
      #                  breaks = seq(0,200, 10)) +
      scale_y_continuous(limits=c(minmax[1]*0.9, minmax[2]*1.1),
                         #breaks = round(seq(minmax[1]*1200, minmax[2]*800, length.out=50), 0)) +
                         breaks = waiver(), n.breaks = 10) +
      #     breaks = seq(0,4000, 0.2)) +
      #my.axis +
      theme2() +
      theme(legend.position = 'right')

    p.IntR.dc <- ggplot(capacity) +
      geom_point(aes(x=CycNr, y=IntR.dc), color='blue', size=4) +
      labs(x = bquote('cycle number'),
           y = bquote('Internal Resistance / Ohm'),
           title = "Internal Resistance (end of discharge) vs. cycle number",
           color = "Legend") +
      #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
      #                  breaks = seq(0,200, 10)) +
      scale_y_continuous(limits=c(minmax[3]*0.9, minmax[4]*1.1),
                         #breaks = round(seq(minmax[3]*1200, minmax[4]*800, length.out=10), 0)) +
                         breaks = waiver(), n.breaks = 10) +
      #my.axis +
      theme2() +
      theme(legend.position = 'right')

    return(list('IntR.ch'= p.IntR.ch, 'IntR.dc' = p.IntR.dc))
  }

#' @export
#' @rdname plot0r
#' @details plotVP plots voltage profile in different presentations
plotVP <- function(cell){

  #binding variables locally to function plotVPloop
  CycNr <- cellType <- type <- meta <- VP.df <- ch <- dc <- Qloop.mAh.g <- Ewe.V.rnd <- Qch.mAh.g <- Qdc.mAh.g <- Ewe.V.ch <- Ewe.V.dc <- NULL

  meta <- cell$metadata
  cellType <- meta$cell.config

  #draw voltage profiles
  l.VP <- cell$VoltageProfiles
  if(!is.null(l.VP)) {

    VP.df <- do.call(rbind, l.VP)

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
      return(list('VPloop'= NA, 'VPlin' = NA, 'VPsplit.ch' = NA, 'VPsplit.dc' = NA))
    }

  }else{

    return(list('VPloop'= NA, 'VPlin' = NA, 'VPsplit.ch' = NA, 'VPsplit.dc' = NA))

  }

  #Plot VP (looped)
  p.VPloop <- ggplot(VP.df) +
    geom_path(aes(x=Qloop.mAh.g, y=Ewe.V.rnd, color=factor(CycNr)), size=1.5) +
    labs(x = bquote('q / mAh'),
         y = bquote('E / V vs. Li^+/Li'),
         title = "Voltage Profiles") +
    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
    #                  breaks = seq(0,200, 10)) +
    scale_y_continuous(limits=c(min(VP.df$Ewe.V),max(VP.df$Ewe.V)),
                      breaks = seq(-1, 5, 0.5)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    theme2() +
    theme(legend.position = "right")

  #Plot VP (linear)
  p.VPlin <- ggplot() +
    geom_path(data = ch, aes(x=(Qch.mAh.g), y=Ewe.V.ch, color=factor(CycNr)), size=1.5) +
    geom_path(data = dc, aes(x=(Qdc.mAh.g), y=Ewe.V.dc, color=factor(CycNr)), size=1.5) +
    labs(x = bquote('q / mAh'),
         y = bquote('E / V vs. Li^+/Li'),
         title = "Voltage Profiles") +
    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
    #                  breaks = seq(0,200, 10)) +
    scale_y_continuous(limits=c(min(VP.df$Ewe.V),max(VP.df$Ewe.V)),
                       breaks = seq(-5, 5, 0.25)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    theme2() +
    theme(legend.position = "right")

  #Plot VP split (dc)
  p.splitDC <- ggplot() +
    geom_path(data = dc, aes(x=(Qdc.mAh.g), y=Ewe.V.dc, color=factor(CycNr)), size=1.5) +
    labs(x = bquote('q / mAh'),
         y = bquote('E / V vs. Li^+/Li'),
         title = "Voltage Profiles") +
    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
    #                  breaks = seq(0,200, 10)) +
    scale_y_continuous(limits=c(min.dc.y,max.dc.y),
                       breaks = seq(-5, 5, 0.25)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    theme2() +
    theme(legend.position = "none")
  #my.legend + theme(legend.position = c(0,1),
  #                  legend.justification = c("left", "top"))

  #Plot VP split (ch)
  p.splitCH <- ggplot() +
    geom_path(data = ch, aes(x=(Qch.mAh.g), y=Ewe.V.ch, color=factor(CycNr)), size=1.5) +
    labs(x = bquote('q / mAh'),
         y = bquote('E / V vs. Li^+/Li'),
         title = "Voltage Profiles") +
    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
    #                  breaks = seq(0,200, 10)) +
    scale_y_continuous(limits=c(min.ch.y, max.ch.y),
                       breaks = seq(-5, 5, 0.25)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    theme2() +
    theme(legend.position = "none")

  return(list('VPloop'= p.VPloop, 'VPlin' = p.VPlin, 'VPsplit.ch' = p.splitCH, 'VPsplit.dc' = p.splitDC))

}


#' @export
#' @rdname plot0r
#' @details plotdQdV plots differential capacity plots (dQ/dV) - uses a moving average to smooth the data!
plotdQdV <- function(cell){

  CycNr <- l.VP <- VP.df <- dqdv <- Ewe <- NULL

  l.VP <- cell$VoltageProfiles
  VP.df <- do.call(rbind, l.VP)

  dqdv.df <- data.frame('CycNr'=VP.df$CycNr, 'Ewe'=VP.df$Ewe.V.rnd, 'dqdv'=VP.df$dQdV.mav3)
  dqdv.df <- na.omit(dqdv.df)

  min.y <- min(dqdv.df$dqdv, na.rm=TRUE)
  max.y <- max(dqdv.df$dqdv, na.rm=TRUE)

  #Plot capacity
  p.dQdV <- ggplot(dqdv.df) +
    geom_path(aes(x=Ewe, y=dqdv, color=factor(CycNr)), size=1.5) +
    labs(x = bquote('E / V vs. Li^+/Li'),
         y = bquote('dQ/dV [mAh/V]'),
         title = "Voltage Profiles") +
    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
    #                  breaks = seq(0,200, 10)) +
    scale_y_continuous(limits=c(round(min.y*1.1, 1), round(max.y*1.1, 1)),
                       breaks = seq(round(min.y,1), round(max.y,1), 1)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    theme2() +
    theme(legend.position = "right")

  return(p.dQdV)

}

#' @export
#' @rdname plot0r
#' @details plotdVdQ plots differential voltage plots (dV/dQ) - uses a moving average to smooth the data!
plotdVdQ <- function(cell){

  CycNr <- l.VP <- VP.df <- dvdq <- type <- NULL

  l.VP <- cell$VoltageProfiles
  VP.df <- do.call(rbind, l.VP)

  dvdq.df <- data.frame('CycNr'=VP.df$CycNr, 'q'=VP.df$Qloop, 'dvdq'=VP.df$dVdQ.mav3, 'type'=VP.df$type)
  dvdq.df <- na.omit(dvdq.df)

  dvdq.df <- dvdq.df %>%
              filter(type == 'ch')

  dvdq.df$log <- log10(abs(dvdq.df$dvdq))
  dvdq.df <- dvdq.df[!dvdq.df$log > 2.2, ]

  min.y <- min(dvdq.df$dvdq, na.rm=TRUE)
  max.y <- max(dvdq.df$dvdq, na.rm=TRUE)

  #Plot capacity
  p.dVdQ <- ggplot(dvdq.df) +
    geom_path(aes(x=q, y=abs(dvdq), color=factor(CycNr)), linewidth=1.5) +
    labs(x = bquote('q / mAh'),
         y = bquote('abs(dV/dQ) [V/mAh]'),
         title = "Voltage Profiles") +
    scale_x_continuous(limits=c(0, max(dvdq.df$q)),
                      breaks = seq(0, round(max(dvdq.df$q)), round(max(dvdq.df$q))/10)) +
    scale_y_continuous(limits=c(0, 50),
                       breaks = seq(0, 50, 10)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    theme2() +
    theme(legend.position = "right")

  return(p.dVdQ)

}
