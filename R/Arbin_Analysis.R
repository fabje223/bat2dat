#' @title Arbin_analysis
#'
#' @description Analysis Arbin data to produce data.frames for voltage profiles of selected cycles
#' and capacity (vs. cycle number) data
#'
#' @param raw raw data file
#' @param cycles selected cycles to extract for voltage profiles
#' @param AMmass - object of Biologic.CAPA
#' @param cellType - object of Biologic.CAPA
#'
#' @return returns data.frames (for Arbin.VP as list)
#'
#' @include Evaluat0r.R Process0r.R Read0r2.R Report0r.R
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#'
#' @examples
#' \dontrun{
#' capa <- Arbin.CAPA()
#'
#' VP <- Arbin.VP()
#' }

#' @export
#' @rdname Arbin_analysis
#' @details Extract voltage profiles for selected cycles
Arbin.VP <- function(raw, AMmass, cellType, cycles=c(1,5,10)){

  #binding variables locally to function Arbin.VP
  cyc.nr <- time.s <- Qch.Ah <- Qdc.Ah <- Ewe.V <- Ewe.V.rnd <- I.A <- diff.Q <- diff.E <- NULL

  VP.list <- list()
  k = 1

  mass <- AMmass/1000 #in g

  #exclude values in selected cycles to those that are actually available in the specific data.frame
  idx <- cycles[cycles %in% c(1:max(raw$cyc.nr)-1)]

  if(cellType %in% c('halfcell-anode', 'LiS')){

      for(i in cycles+1){
        VP.df <- raw %>%
          filter(cyc.nr == i & I.A != 0) %>%
          mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                 'diff.Q' = c(0, diff(Qdc.Ah)),
                 'diff.E' = c(0, diff(Ewe.V.rnd)),
                 'diff.cap' = sqrt((diff.Q/diff.E)^2)*-1,
                 'type' = '--') %>%
          arrange(time.s)

        # New column
        # Qloop: create a column where capacity is counted up until the sequence changes and then counted back down.
        VP.df$Qloop <- VP.df$Qdc.Ah - VP.df$Qch.Ah

        # modify Q.ch and Q.dc columns for plotting, e.g. in Origin
        VP.df$Ewe.V.dc <- VP.df$Ewe.V
        VP.df$Qdc.Ah[VP.df$I.A > 0] <- NA
        VP.df$Ewe.V.dc[VP.df$I.A > 0] <- NA

        VP.df$Ewe.V.ch <- VP.df$Ewe.V
        VP.df$Qch.Ah[VP.df$I.A < 0] <- NA
        VP.df$Ewe.V.ch[VP.df$I.A < 0] <- NA


        #add identifier for plotting
        VP.df$type[VP.df$I.A < 0] <- 'ch'
        VP.df$type[VP.df$I.A > 0] <- 'dc'

        # create new data.frame
        VPprofiles <- data.frame("CycNr" = VP.df$cyc.nr,
                                 "time.s" = VP.df$time.s,
                                 "I.A"=VP.df$I.A,
                                 "Qch.mAh" = VP.df$Qdc.Ah,
                                 "Qdc.mAh" = VP.df$Qch.Ah,
                                 "Qch.mAh.g" = VP.df$Qdc.Ah/mass,
                                 "Qdc.mAh.g" = VP.df$Qch.Ah/mass,
                                 "Qloop" = VP.df$Qloop,
                                 "Qloop.mAh.g" = VP.df$Qloop/mass,
                                 "Ewe.V" = VP.df$Ewe.V,
                                 "Ewe.V.rnd" = VP.df$Ewe.V.rnd,
                                 "Ewe.V.ch" = VP.df$Ewe.V.dc,
                                 "Ewe.V.dc" = VP.df$Ewe.V.ch,
                                 "diffcap" = VP.df$diff.cap,
                                 "type" = VP.df$type)

        VP.list[[k]] <- VPprofiles
        k = k+1
      }
  }else if(cellType %in% c('halfcell-cathode', 'fullcell')){

      for(i in cycles+1){
        VP.df <- raw %>%
                    filter(cyc.nr == i) %>%
                    mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                           'diff.Q' = c(0, diff(Qdc.Ah)),
                           'diff.E' = c(0, diff(Ewe.V.rnd)),
                           'diff.cap' = sqrt((diff.Q/diff.E)^2)*-1,
                           'type' = '--') %>%
                    arrange(time.s)

        # New column
        # Qloop: create a column where capacity is counted up until the sequence changes and then counted back down.
        VP.df$Qloop <- VP.df$Qch.Ah - VP.df$Qdc.Ah

        # modify Q.ch and Q.dc columns for plotting, e.g. in Origin
        VP.df$Ewe.V.dc <- VP.df$Ewe.V
        VP.df$Qdc.Ah[VP.df$I.A > 0] <- NA
        VP.df$Ewe.V.dc[VP.df$I.A > 0] <- NA

        VP.df$Ewe.V.ch <- VP.df$Ewe.V
        VP.df$Qch.Ah[VP.df$I.A < 0] <- NA
        VP.df$Ewe.V.ch[VP.df$I.A < 0] <- NA


        #add identifier for plotting
        VP.df$type[VP.df$I.A < 0] <- 'ch'
        VP.df$type[VP.df$I.A > 0] <- 'dc'

        # create new data.frame
        VPprofiles <- data.frame("CycNr" = VP.df$cyc.nr,
                                 "time.s" = VP.df$time.s,
                                 "I.A"=VP.df$I.A,
                                 "Qch.mAh" = VP.df$Qch.Ah,
                                 "Qdc.mAh" = VP.df$Qdc.Ah,
                                 "Qch.mAh.g" = VP.df$Qch.Ah/mass,
                                 "Qdc.mAh.g" = VP.df$Qdc.Ah/mass,
                                 "Qloop" = VP.df$Qloop,
                                 "Qloop.mAh.g" = VP.df$Qloop/mass,
                                 "Ewe.V" = VP.df$Ewe.V,
                                 "Ewe.V.rnd" = VP.df$Ewe.V.rnd,
                                 "Ewe.V.ch" = VP.df$Ewe.V.ch,
                                 "Ewe.V.dc" = VP.df$Ewe.V.dc,
                                 "diffcap" = VP.df$diff.cap,
                                 "type" = VP.df$type)

        VP.list[[k]] <- VPprofiles
        k = k+1
      }
  }

  return(VP.list)
}

#' @export
#' @rdname Arbin_analysis
#' @details Arbin.CAPA extracts cycling data from galvanostatic cycling experiments
Arbin.CAPA <- function(raw, AMmass, cellType){

    cyc.nr <- I.A <- NULL

    cap <- data.frame('cyc.nr'= numeric(), 'time.s'= numeric(), 'Ns'= numeric(), 'Ewe.V'= numeric(),
                      'I.A'= numeric(), 'Qdc.Ah'= numeric(), 'Qch.Ah'= numeric())

    #raw <- ldat[[1]]$raw
    mass <- AMmass/1000 #in g

    ##Analyse Arbin files
    maxcyc <- max(raw$cyc.nr)

    if(maxcyc == 1) return(cap)

    stats <- lapply(1:(maxcyc-1), function(i){

      tmp <- raw %>%
              filter(cyc.nr == i)
      #take last line of data.frame to obtain total capacity of that sequence
      tmp <- tail(tmp, 1)

      return(tmp)
    })
    dc.df <- do.call(rbind, stats)

    #write results in new data.frame "cap"
    #at the moment: no half cycles
    if(nrow(dc.df) == 0) {
      return(cap)
    } else if(cellType %in% c('halfcell-cathode', 'fullcell')){
      # fill result data.frame
      cap <- data.frame("CycNr" = dc.df$cyc.nr,
                        #"time.s.ch" = ch.df$time.s,
                        "time.s.dc" = dc.df$time.s,
                        "Qch.mAh" = dc.df$Qch.Ah,
                        "Qdc.mAh" = dc.df$Qdc.Ah,
                        "Qch.mAh.g" = dc.df$Qch.Ah/mass,
                        "Qdc.mAh.g" = dc.df$Qdc.Ah/mass,
                        "CE" = (dc.df$Qdc.Ah/dc.df$Qch.Ah)*100,
                        #"Ewe.endCH" = ch.df$Ewe.V,
                        "Ewe.endDC" = dc.df$Ewe.V)

      return(cap)

    }else if(cellType == 'halfcell-anode'){
      # fill result data.frame
      cap <- data.frame("CycNr" = dc.df$cyc.nr,
                        #"time.s.ch" = ch.df$time.s,
                        "time.s.dc" = dc.df$time.s,
                        "Qch.mAh" = dc.df$Qdc.Ah,
                        "Qdc.mAh" = dc.df$Qch.Ah,
                        "Qch.mAh.g" = dc.df$Qch.Ah/mass,
                        "Qdc.mAh.g" = dc.df$Qdc.Ah/mass,
                        "CE" = (dc.df$Qch.Ah/dc.df$Qdc.Ah)*100,
                        #"Ewe.endCH" = ch.df$Ewe.V,
                        "Ewe.endDC" = dc.df$Ewe.V)

      return(cap)
  }
}
