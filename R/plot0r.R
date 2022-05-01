#' @title plot0r
#'
#' @description Plotting Script for RMarkdown Report
#'
#' @param capacity data.frame with galvanostatic cycling information
#' @param vp.dat data.frame with voltage profile information on selected cycles
#' @param cell cell configuration of experiment (halfcell-anode, halfcell-cathode, fullcell, LiS)
#'
#' @return returns graphs generated from expeirmental data for report.Rmd
#' @export
#'
#' @include customThemes.R reportGenerat0r.R
#' @import ggplot2 viridis
#' @importFrom gridExtra grid.arrange
#'
#' @examples
plotCapReport <- function(capacity){

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
                          breaks = seq(0, 5000, 50)) +
        #     breaks = seq(0,4000, 0.2)) +
        #my.axis +
        customTheme()
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
        customTheme() +
        theme(legend.position = 'none')

    #merge into one plot
    grid.arrange(p.cap,
                 p.CE,
                 nrow=1, ncol=2
                 #top = textGrob("0.75 M KPF6, EC:DEC (v/v = 1:1)",gp=gpar(fontsize=18,font=3), just=c(0.85,0), vjust=0.3)
                )

      #return(p.cap, p.CE)
}

#' @describeIn plotCapReport plots IR drop versus cycle number
plotIRdrop <- function(capacity){

  p.IR.drop.ch <- p.IR.drop.dc <- NULL

  #binding variables locally to function plotCapReport
  CycNr <- Edrop.ch <- Edrop.dc <- NULL

  #Plot capacity
  max.y.ch <- max(capacity$Edrop.ch)
  min.y.ch <- min(capacity$Edrop.ch)
  p.IRdrop.ch <- ggplot(capacity) +
                    geom_point(aes(x=CycNr, y=Edrop.ch*1000), color='red', size=4) +
                    labs(x = bquote('cycle number'),
                         y = bquote('IR drop / mV'),
                         title = "IR drop (charge) vs. cycle number",
                         color = "Legend") +
                    #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
                    #                  breaks = seq(0,200, 10)) +
                    scale_y_continuous(limits=c(min.y.ch*1000, max.y.ch*1000),
                                       breaks = seq(min.y.ch*900, max.y.ch*1100, length.out=5)) +
                    #     breaks = seq(0,4000, 0.2)) +
                    #my.axis +
                    customTheme() +
                    theme(legend.position = 'right')

  max.y.dc <- max(capacity$Edrop.dc)
  min.y.dc <- min(capacity$Edrop.dc)
  p.IRdrop.dc <- ggplot(capacity) +
                  geom_point(aes(x=CycNr, y=Edrop.dc*1000), color='blue', size=4) +
                  labs(x = bquote('cycle number'),
                       y = bquote('IR drop / mV'),
                       title = "IR drop (discharge) vs. cycle number",
                       color = "Legend") +
                  #scale_x_continuous(limits=c(0,max(tmp$CycNr)),
                  #                  breaks = seq(0,200, 10)) +
                  scale_y_continuous(limits=c(min.y.dc*900, max.y.dc*1000),
                                     breaks = seq(min.y.dc*900, max.y.dc*1100, length.out=5)) +
                  #     breaks = seq(0,4000, 0.2)) +
                  #my.axis +
                  customTheme() +
                  theme(legend.position = 'right')

  #merge into one plot
  grid.arrange(p.IRdrop.ch,
               p.IRdrop.dc,
               nrow=1, ncol=2
               #top = textGrob("0.75 M KPF6, EC:DEC (v/v = 1:1)",gp=gpar(fontsize=18,font=3), just=c(0.85,0), vjust=0.3)
  )
  #return(p.IRdrop)

}

#' @describeIn plotCapReport plots voltage profile vs Qloop
plotVPloop <- function(vp.dat){

  p.vp <- NULL

  #binding variables locally to function plotVPloop
  CycNr <- Ewe.V <- Ewe.V.rnd <- Qloop.mAh.g <- NULL

  #Plot capacity
  p.vp <- ggplot(vp.dat) +
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

  return(p.vp)

}

#' @describeIn plotCapReport plots voltage profile vs Q
plotVPlin <- function(vp.dat){

    p.vp <- NULL

    #binding variables locally to function plotVPloop
    CycNr <- Ewe.V.ch <- Ewe.V.dc <- Qch.mAh.g <- Qdc.mAh.g <- type <- NULL

    ch <- vp.dat %>%
          filter(type == 'ch')
    dc <- vp.dat %>%
          filter(type == 'dc')
    min.y <- round(min(dc$Ewe.V),1)-0.1
    max.y <- round(max(dc$Ewe.V),1)+0.1

  #Plot capacity
  p.vp <- ggplot() +
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

  return(p.vp)

}

#' @describeIn plotCapReport plots voltage profile vs Q
plotVPsplit <- function(vp.dat, cell){

  p.vp1 <- p.vp2 <- NULL

  #binding variables locally to function plotVPloop
  CycNr <- Ewe.V.ch <- Ewe.V.dc <- Qch.mAh.g <- Qdc.mAh.g <- type <- NULL

  if(cell %in% c('halfcell-cathode', 'fullcell')){
    ch <- vp.dat %>%
      filter(type == 'ch')
    dc <- vp.dat %>%
      filter(type == 'dc')

    min.dc.y <- round(min(dc$Ewe.V),1)-0.1
    max.dc.y <- round(max(dc$Ewe.V),1)+0.1
    min.ch.y <- round(min(ch$Ewe.V),1)-0.1
    max.ch.y <- round(max(ch$Ewe.V),1)+0.1
    #print('plotting cathode/full cell data')
  }else if (cell %in% c('halfcell-anode', 'LiS')){
    ch <- vp.dat %>%
      filter(type == 'ch')
    dc <- vp.dat %>%
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

  #merge into one plot
  grid.arrange(p.vp1,
               p.vp2,
               nrow=1, ncol=2
               #top = textGrob("0.75 M KPF6, EC:DEC (v/v = 1:1)",gp=gpar(fontsize=18,font=3), just=c(0.85,0), vjust=0.3)
  )

}
