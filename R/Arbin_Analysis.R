#' @title Arbin.analysis
#'
#' @description Analysis Arbin data to produce data.frames for voltage profiles of selected cycles
#' and capacity (vs. cycle number) data
#'
#' @param raw raw data file
#' @param cycles selected cycles to extract for voltage profiles
#'
#' @return returns data.frames (for Arbin.VP as list)
#' @export
#'
#' @include Evaluat0r.R Process0r.R Read0r2.R Report0r.R
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @name %>%
#' @import dplyr
#'
#' @examples
Arbin.VP <- function(raw, cycles){

  #binding variables locally to function Arbin.VP
  cyc.nr <- time.s <- Qch.Ah <- Qdc.Ah <- Ewe.V <- Ewe.V.rnd <- diff.Q <- diff.E <- NULL

  VP.list <- list()
  k = 1

  #exclude values in selected cycles to those that are acually available in the specific data.frame
  idx <- cycles[cycles %in% c(1:max(raw$cyc.nr)-1)]

  for(i in cycles+1){
    VP.df <- raw %>%
                filter(cyc.nr == i) %>%
                mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                       'diff.Q' = c(0, diff(Qdc.Ah)),
                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                       'diff.cap' = sqrt((diff.Q/diff.E)^2)*-1,
                       'type' = '--') %>%
                arrange(time.s)

    #remove inf values in diff.cap
    #VP.df <- VP.df[!(dat$diff.Q == 0 | dat$diff.E == 0),] #is.infinite(dat$diff.cap) |

    #create a column where capacity is counted up during charge until the sequence changes. On discharge the capacity is substracted from end-of-charge capacity
    VP.df$Qloop <- VP.df$Qdc.Ah - VP.df$Qch.Ah

    #Reset time of each sequence to 0
    VP.df$time.s <- VP.df$time.s - min(VP.df$time.s)

    # create new data.frame
    VPprofiles <- data.frame("CycNr" = VP.df$cyc.nr, "time.s" = VP.df$time.s, "I.A"=VP.df$I.A, "Qch.mAh" = VP.df$Qch.Ah, "Qdc.mAh" = VP.df$Qdc.Ah,
                             "Qloop" = VP.df$Qloop, "Ewe.V" = VP.df$Ewe.V, "Ewe.V.rnd" = VP.df$Ewe.V.rnd, "diffcap" = VP.df$diff.cap,
                             "type" = VP.df$type)

    VP.list[[k]] <- VPprofiles
    k = k+1
  }

  return(VP.list)
}

#' @describeIn Arbin.VP data from galvanostatic cycling experiments
Arbin.CAP <- function(raw, AM.mass){

  #binding variables locally to function Arbin.CAP
  cyc.nr <- I.A <- NULL

  #raw <- ldat[[1]]$raw
   mass <- AM.mass/1000 #in g

   #determine cell type: anode-half cell/ cathode-half cell or full cell
   x <- raw %>%
          filter(cyc.nr == 1 & I.A != 0)
   x <- head(x, 1)

   if(x$I.A > 0){ cell = 'cathode/full'
   }else if(x$I.A < 0){ cell = 'anode'}

    ##Analyse Arbin files
    stats <- lapply(1:max(raw$cyc.nr)-1, function(i){

      tmp <- raw %>%
              filter(cyc.nr == i)
      #take last line of data.frame to obtain total capacity of that sequence
      tmp <- tail(tmp, 1)

      return(tmp)
    })
    dc.df <- do.call(rbind, stats)
    dc.df$Qdc.Ah <- dc.df$Qdc.Ah/mass
    dc.df$Qch.Ah <- dc.df$Qch.Ah/mass

    #write results in new data.frame "cap"
    #at the moment: no half cycles
    if(nrow(dc.df) == 0) {
      return(cap)
    } else if(cell == 'cathode/full'){
      # fill result data.frame
      cap <- data.frame("CycNr" = dc.df$cyc.nr,
                        #"time.s.ch" = ch.df$time.s,
                        "time.s.dc" = dc.df$time.s,
                        "Qch.mAh" = dc.df$Qch.Ah,
                        "Qdc.mAh" = dc.df$Qdc.Ah,
                        "CE" = (dc.df$Qdc.Ah/dc.df$Qch.Ah)*100,
                        #"Ewe.endCH" = ch.df$Ewe.V,
                        "Ewe.endDC" = dc.df$Ewe.V)

      return(cap)

    }else if(cell == 'anode'){
      # fill result data.frame
      cap <- data.frame("CycNr" = dc.df$cyc.nr,
                        #"time.s.ch" = ch.df$time.s,
                        "time.s.dc" = dc.df$time.s,
                        "Qch.mAh" = dc.df$Qdc.Ah,
                        "Qdc.mAh" = dc.df$Qch.Ah,
                        "CE" = (dc.df$Qch.Ah/dc.df$Qdc.Ah)*100,
                        #"Ewe.endCH" = ch.df$Ewe.V,
                        "Ewe.endDC" = dc.df$Ewe.V)

      return(cap)
  }
}
