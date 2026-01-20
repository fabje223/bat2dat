library(dplyr)
library(gridExtra)

l <- process0r(cycles=c(0), CCCV=TRUE)

anode <- plotdQdV(l[[1]]) +
          ylim(-30,50) + xlim(0,1.0)+
          labs(x = bquote('E / V'),
               y = bquote('dQ/dV [mAh/V]'),
               title = "Differential Capacity") +
          theme1() + theme(legend.position = "none") + labs(title="Graphite vs. K") +
          scale_color_manual(values=c('#287C8EFF'))
cathode <- plotdQdV(l[[2]]) +
            ylim(-2,2) + xlim(3,4.5) +
            labs(x = bquote('E / V'),
                 y = bquote('dQ/dV [mAh/V]'),
                 title = "Differential Capacity") +
           theme1() + theme(legend.position = "none") + labs(title="KVP vs. K") +
           scale_color_manual(values=c('#404688FF'))
full <- plotdQdV(l[[3]]) +
          ylim(-2,2) + xlim(2,4.5) +
          labs(x = bquote('E / V'),
               y = bquote('dQ/dV [mAh/V]'),
               title = "Differential Capacity") +
          theme1() + theme(legend.position = "none") + labs(title="KVP vs. Graphite") +
          scale_color_manual(values=c('#440154FF'))

grid.arrange(anode, cathode, full, ncol=1)

