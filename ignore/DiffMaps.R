library(ggplot2)
library(grid)
library(scales)
library(gridExtra)
library(RColorBrewer)

# Plots...
pal <- colorRampPalette(rev(brewer.pal(9, "YlOrRd")), space="Lab")
#pal2 <- colorRampPalette(brewer.pal(9, "YlGnBu"), space="Lab")
pal2 <- colorRampPalette(rev(brewer.pal(9, "YlGnBu")), space="Lab")

HeatMap_Theme <- theme_bw() +
  theme(text = element_text(face="bold", size=16)) +
  theme(axis.title.x = element_text(vjust=-0.3, size=16)) +
  theme(axis.title.y = element_text(vjust=0.8, size=16)) +
  theme(panel.border = element_rect(size=1,color = "black")) +
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm")) +
  theme(panel.grid.major = element_line(linewidth=0.5))

samples <- process0r(cycles=c(0:100))
VPprofiles <- cells[[4]]$VoltageProfiles

dqdv.df <- do.call(rbind, VPprofiles)
dqdv.df <- data.frame('CycNr'=dqdv.df$CycNr, 'Ewe'=dqdv.df$Ewe.V.rnd, 'dqdv'=dqdv.df$dQdV.mav3, 'type'=dqdv.df$type)
dqdv.df <- na.omit(dqdv.df)

max.CycNr <- max(dqdv.df$CycNr, na.rm=TRUE)
min.E <- min(dqdv.df$Ewe, na.rm=TRUE)
max.E <- max(dqdv.df$Ewe, na.rm=TRUE)

dqdv.df.dc <- dqdv.df %>%
                filter(type == 'dc')

min.dQdV <- min(dqdv.df.dc$dqdv, na.rm=TRUE)
max.dQdV <- max(dqdv.df.dc$dqdv, na.rm=TRUE)

p1 <- ggplot(dqdv.df.dc) +
  geom_tile(aes(x=CycNr, y=Ewe, fill=dqdv), height=0.1) +
  scale_y_continuous(limits=c(min.E, max.E)) +
  #  scale_colour_gradientn("La la la", colours=pal(100), limits=c(-25000,0), na.value = "white") +
  scale_fill_gradientn("dQ/dV", colours=pal(200), limits=c(0, 0.5), na.value = alpha(0.9)) +
  labs(x= "cycle number", y="E vs K+/K / V", title="Graphite/KVP, EC:DEC (v/v=1:1)")+
  xlim(0, max.CycNr) +
  theme.HeatMap
p1

dqdv.df.ch <- dqdv.df %>%
                filter(type == 'ch')

min.dQdV <- min(dqdv.df.ch$dqdv, na.rm=TRUE)
max.dQdV <- max(dqdv.df.ch$dqdv, na.rm=TRUE)

#manual scaling handle
min.E = 2.75
max.E = 4.55

p2 <- ggplot(dqdv.df.ch) +
  geom_tile(aes(x=CycNr, y=Ewe, fill=dqdv), height=0.025) +
  scale_y_continuous(limits=c(min.E, max.E)) +
  #  scale_colour_gradientn("La la la", colours=pal(100), limits=c(-25000,0), na.value = "white") +
  scale_fill_gradientn("dQ/dV", colours=pal2(250), limits=c(-0.4,0), na.value = alpha(0.9)) +
  #scale_fill_viridis(alpha=0.95, #transparancy
   #                  begin = 0.0,
    #                 end = 0.97,
     #                direction = 1, #+1 or -1
      #               discrete = FALSE,
       #               )+
  labs(x= "cycle number", y="E vs K+/K / V", title="Graphite/KVP, EC:DEC (v/v=1:1)") +
  xlim(0,max.CycNr) +
  theme.HeatMap
p2
