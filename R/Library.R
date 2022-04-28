
#R.packages
library(dplyr)
library(zoo)
library(readxl)
library(ggplot2)
library(gridExtra)
library(rmarkdown)
library("optparse")
library(RODBC)

#Plot layout
my.panel <- theme(
  panel.grid.major = element_line(colour="black", size=0.4),
  panel.grid.minor = element_line(colour="black", linetype="dashed", size=0.2),
  panel.background = element_blank(),
  panel.border = element_rect(colour="black", fill=NA, size=2))
my.axis <- theme(
  axis.text.x = element_text(colour="black", size=20, face="bold"),
  axis.title.x = element_text(colour="black", size=20, face="bold"),
  axis.text.y = element_text(colour="black", size=20, face="bold"),
  axis.title.y = element_text(colour="black", size=20, angle=90, face="bold"),
  plot.title = element_text(colour="black", size=22, face="bold"),
  strip.text = element_text(colour='black', size=18, face="bold")) 
my.legend <- theme(
  legend.background = element_rect(fill="grey90", colour="black", size=0.5),
  legend.title = element_text(colour="black", face="bold", size=18),
  legend.text = element_text(colour="black", face="bold", size=16),
  legend.key = element_rect(colour="black", size=0.25),
  legend.position= c(0.99,0.75), #'top'
  legend.direction="vertical",
  legend.justification= c(1,0))
