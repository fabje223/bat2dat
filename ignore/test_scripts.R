library(dplyr)

l <- process0r(CCCV=TRUE)

cell <- 2

raw <- l[[cell]]$rawdata
raw1 <- raw %>% filter(cyc.nr %in% c(0,1))

VP1 <- l[[cell]]$VoltageProfiles[[1]]
stats1 <- l[[cell]]$capacity

VPchk <- ggplot() +
          geom_line(data=raw1, aes(x=time.s, y=Ewe.V), color='red', size=2) +
          geom_line(data=VP1, aes(x=time.s, y=Ewe.V), color='blue', size=1)
VPchk


VP.df <- l[[cell]]$VoltageProfiles[[3]]
dqdv.df <- data.frame('CycNr'=VP.df$CycNr, 'Ewe'=VP.df$Ewe.V.rnd, 'dqdv'=VP.df$dQdV.mav3)
dqdv.df <- na.omit(dqdv.df)

VP <- ggplot() +
          geom_line(data=VP.df, aes(x=time.s, y=Ewe.V), color='red', size=2)
VP

dqdv <- ggplot() +
          geom_path(data=dqdv.df, aes(x=Ewe, y=dqdv), color='red', size=1)
dqdv

dqdv <- plotdQdV(l[[cell]]) #+ ylim(-30,130) + xlim(0,1.5)
dqdv

dvdq <- plotdVdQ(l[[cell]]) + ylim(0,80)
dvdq
