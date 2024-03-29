#' @title plot0r
#'
#' @description Plotting Script for RMarkdown Report
#'
#' @param capacity data.frame with galvanostatic cycling information
#'
#' @return returns graphs generated from expeirmental data for report.Rmd
#' @export
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

#' @describeIn plotCEReport plots IR drop versus cycle number
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

#' @describeIn plotCapReport plots IR drop versus cycle number
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

  #' @describeIn plotCapReport plots IR drop versus cycle number
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

#' @describeIn plotCapReport plots voltage profile vs Qloop
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

#' @describeIn plotCapReport plots voltage profile vs Q
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

#' @describeIn plotCapReport plots voltage profile vs Q
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


#' @describeIn plotCapReport plots voltage profile vs Q
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
