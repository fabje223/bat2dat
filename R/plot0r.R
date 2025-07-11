#' @title plot0r
#'
#' @description Plotting Script for RMarkdown Report
#'
#' @param capacity data.frame with galvanostatic cycling information
#' @param minmax axis range
#' @param vp.dat voltage profile information
#' @param dc data.frame with discharge data for selected cycles
#' @param ch data.frame with charge data for selected cycles
#' @param min.dc.y y-axis limit - min
#' @param max.dc.y y-axis limit - max
#' @param min.ch.y y-axis limit - min
#' @param max.ch.y y-axis limit - max
#'
#' @return returns graphs generated from expeirmental data for report.Rmd
#'
#' @include customThemes.R reportGenerat0r.R
#' @import ggplot2 viridis
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' \dontrun{
#' capa <- Arbin.CAP()
#' pCap <- plotCapa(capa)
#' }

#' @rdname plot0r
#' @details plotCapa plots coulombic efficiency versus cycle number
plotCapa <- function(capacity){

      #binding variables locally to function plotCapReport
      p.cap <- p.CE <- NULL
      CycNr <- Qdc.mAh.g <- CE <- NULL

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
        customTheme()
        #my.legend #theme(legend.position = 'none')

      return(p.cap)
}

#' @rdname plot0r
#' @details plotCE plots coulombic efficiency versus cycle number
plotCE <- function(capacity){

      CycNr <- CE <- NULL

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
        customTheme() +
        theme(legend.position = 'none')

      return(p.CE)

}

#' @rdname plot0r
#' @details plotIRdropCH plots IR drop versus cycle number (charge)
plotIRdropCH <- function(capacity, minmax){

  #binding variables locally to function plotCapReport
  CycNr <- Edrop.ch <- NULL

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
                    customTheme() +
                    theme(legend.position = 'right')

  return(p.IRdrop.ch)
}

#' @rdname plot0r
#' @details plotIRdropDC plots IR drop versus cycle number (discharge)
  plotIRdropDC <- function(capacity, minmax){

    #binding variables locally to function plotCapReport
    CycNr <- Edrop.dc <- NULL

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
                  customTheme() +
                  theme(legend.position = 'right')

  return(p.IRdrop.dc)
}

#' @rdname plot0r
#' @details plotIntRCH plots internal resistance versus cycle number (charge)
  plotIntRCH <- function(capacity, minmax=c(0, 1000, 0, 1000)){

    #binding variables locally to function plotCapReport
    CycNr <- IntR.ch <- NULL

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
      customTheme() +
      theme(legend.position = 'right')

    return(p.IntR.ch)
  }

#' @rdname plot0r
#' @details plotIntRDC plots internal resistance versus cycle number (discharge)
plotIntRDC <- function(capacity, minmax=c(0, 1000, 0, 1000)){

    #binding variables locally to function plotCapReport
    CycNr <- IntR.dc <- NULL

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
      customTheme() +
      theme(legend.position = 'right')

    return(p.IntR.dc)
  }


#' @rdname plot0r
#' @details plotVPloop plots voltage profile vs Qloop
plotVPloop <- function(vp.dat){

  #binding variables locally to function plotVPloop
  CycNr <- Ewe.V <- Ewe.V.rnd <- Qloop.mAh.g <- NULL

  #Plot capacity
  p.vp.loop <- ggplot(vp.dat) +
    geom_path(aes(x=Qloop.mAh.g, y=Ewe.V.rnd, color=factor(CycNr)), size=1.5) +
    labs(x = bquote('q / mAh'),
         y = bquote('E / V vs. Li^+/Li'),
         title = "Voltage Profiles") +
    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
    #                  breaks = seq(0,200, 10)) +
    scale_y_continuous(limits=c(min(vp.dat$Ewe.V),max(vp.dat$Ewe.V)),
                      breaks = seq(-1, 5, 0.5)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    customTheme() +
    theme(legend.position = "right")

  return(p.vp.loop)

}

#' @rdname plot0r
#' @details plotVPlin plots voltage profile vs Q
plotVPlin <- function(vp.dat){

    #binding variables locally to function plotVPloop
    CycNr <- Ewe.V.ch <- Ewe.V.dc <- Qch.mAh.g <- Qdc.mAh.g <- type <- NULL

    ch <- vp.dat %>%
          filter(type == 'ch')
    dc <- vp.dat %>%
          filter(type == 'dc')
    min.y <- round(min(dc$Ewe.V),1)-0.1
    max.y <- round(max(dc$Ewe.V),1)+0.1

  #Plot capacity
  p.vp.lin <- ggplot() +
    geom_path(data = ch, aes(x=(Qch.mAh.g), y=Ewe.V.ch, color=factor(CycNr)), size=1.5) +
    geom_path(data = dc, aes(x=(Qdc.mAh.g), y=Ewe.V.dc, color=factor(CycNr)), size=1.5) +
    labs(x = bquote('q / mAh'),
         y = bquote('E / V vs. Li^+/Li'),
         title = "Voltage Profiles") +
    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
    #                  breaks = seq(0,200, 10)) +
    scale_y_continuous(limits=c(min.y,max.y),
                      breaks = seq(-5, 5, 0.25)) +
    #     breaks = seq(0,4000, 0.2)) +
    scale_color_viridis("Cycle Number", discrete=TRUE) +
    #my.axis +
    customTheme() +
    theme(legend.position = "right")

  return(p.vp.lin)

}

#' @rdname plot0r
#' @details plotVPsplitDC plots voltage profile vs Q (discharge)
plotVPsplitDC <- function(dc, min.dc.y, max.dc.y){

  Qdc.mAh.g <- Ewe.V.dc <- CycNr <- NULL

  #Plot capacity
  p.vp1 <- ggplot() +
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
    customTheme() +
    theme(legend.position = "none")
    #my.legend + theme(legend.position = c(0,1),
    #                  legend.justification = c("left", "top"))

  return(p.vp1)
}

#' @rdname plot0r
#' @details plotVPsplitCH plots voltage profile vs Q (charge)
plotVPsplitCH <- function(ch, min.ch.y, max.ch.y){

  Qch.mAh.g <- Ewe.V.ch <- CycNr <- NULL

  #Plot capacity
  p.vp2 <- ggplot() +
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
    customTheme() +
    theme(legend.position = "none")

  return(p.vp2)

}

#' @rdname plot0r
#' @details plotVPloop plots voltage profile vs Qloop
plotdQdV <- function(vp.dat){

  dqdv.df <- data.frame('CycNr'=vp.dat$CycNr, 'Ewe'=vp.dat$Ewe.V.rnd, 'dqdv'=vp.dat$dQdV.mav3)
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
    customTheme() +
    theme(legend.position = "right")

  return(p.dQdV)

}

#' @rdname plot0r
#' @details plotVPloop plots voltage profile vs Qloop
plotdVdQ <- function(vp.dat){

  dvdq.df <- data.frame('CycNr'=vp.dat$CycNr, 'q'=vp.dat$Qloop, 'dvdq'=vp.dat$dVdQ.mav3, 'type'=vp.dat$type)
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
    customTheme() +
    theme(legend.position = "right")

  return(p.dVdQ)

}
