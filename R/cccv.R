#' @title CCCV.Analysis
#'
#' @description Biologic.CAP Evaluates CCCV-steps in galvanostatic cycling data
#'
#' @param raw raw data
#' @param AMmass active material mass in mg
#' @param cellType cell configuration (halfcell, fullcell, etc.)
#'
#' @return revised capacity/cccv table
#' @export
#'
#' @include Read0r2.R Evaluat0r.R Process0r.R Report0r.R
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom utils head
#' @importFrom utils tail
#'
#' @examples
Biologic.CCCV <- function(raw, AMmass, cellType){

  #binding variables to function Biologic.CAP
  cyc.nr <- Ns <- I.mA <- dI <- Ewe.V <- dE <- time.s <- type <- NULL

  cccv.i <- data.frame("CycNr" = numeric(), "Qch.mAh" = numeric(), "chk.sum1" = numeric(), "Qch.mAh.g" = numeric(), "Qdc.mAh" = numeric(), "chk.sum2" = numeric(), "Qdc.mAh.g" = numeric(), "CE" = numeric(), "LowerCutoff" = numeric(), "UpperCutoff" = numeric(),
                     "time.s.ch" = numeric(), "CCstep.t.ch" = numeric(), "CCstep.Qch.mAh" = numeric(), "CCstep.Qch.mAh.g" = numeric(), "CVstep.t.ch" = numeric(), "CVstep.Qch.mAh" = numeric(), "CVstep.Qch.mAh.g" = numeric(),
                     "time.s.dc" = numeric(), "CCstep.t.dc" = numeric(), "CCstep.Qdc.mAh" = numeric(), "CCstep.Qdc.mAh.g" = numeric(), "CVstep.t.dc" = numeric(), "CVstep.Qdc.mAh" = numeric(), "CVstep.Qdc.mAh.g" = numeric()
  )

  cccv.list <- list()
  k = 1

  cccv <- raw %>%
      filter(I.mA != 0)

      cccv$Ewe.V <- round(cccv$Ewe.V,3)
      cccv$I.mA <- round(cccv$I.mA,5)

      cccv <- cccv %>%
        mutate(dE = c(0,diff(Ewe.V)),
               dI = c(0,diff(I.mA))
        )

    if(cellType %in% c('halfcell-anode')){

      cccv$type[cccv$I.mA > 0] <- 'ch'
      cccv$type[cccv$I.mA < 0] <- 'dc'

      cccv$cyc.nr[cccv$type == 'dc'] <- cccv$cyc.nr[cccv$type == 'dc']+1

      }else if(cellType %in% c('halfcell-cathode', 'fullcell', 'LiS')){

      cccv$type[cccv$I.mA < 0] <- 'dc'
      cccv$type[cccv$I.mA > 0] <- 'ch'

      cccv$cyc.nr <- cccv$cyc.nr + 1

      }

    for(i in 1:(max(cccv$cyc.nr)-1)){

        cccv.ch <- cccv %>%
          filter(cyc.nr %in% c(i)) %>%
          filter(type %in% c('ch'))

        t0.ch = cccv.ch$time.s[1]
        t.tot.ch =  rev(cccv.ch$time.s)[1] - t0.ch
        Qch.tot = max(cccv.ch$Qch.mAh)

        cccv.ch <- cccv.ch %>%
          filter(Ewe.V >= max(Ewe.V)*0.99 & dI !=0) %>%
          mutate(t.CV = time.s - t0.ch)

        cccv.dc <- cccv %>%
          filter(cyc.nr %in% c(i)) %>%
          filter(type %in% c('dc'))

        t0.dc = cccv.dc$time.s[1]
        t.tot.dc =  rev(cccv.dc$time.s)[1] - t0.dc
        Qdc.tot = max(cccv.dc$Qdc.mAh)

        cccv.dc <- cccv.dc %>%
          filter(Ewe.V <= min(Ewe.V)*0.99 & dI != 0) %>%
          mutate(t.CV = time.s - t0.dc)

        print(i)


        if(nrow(cccv.ch) != 0 && nrow(cccv.dc) != 0){

          cccv.i <- data.frame("CycNr" = i, "Qch.mAh" = Qch.tot, "chk.sum1" = NA, "Qch.mAh.g" = NA, "Qdc.mAh" = Qdc.tot, "chk.sum2" = NA, "Qdc.mAh.g" = NA, "CE" = Qdc.tot/Qch.tot, "LowerCutoff" = min(cccv.dc$Ewe.V), "UpperCutoff" = max(cccv.ch$Ewe.V),
                             "time.s.ch" = t.tot.ch, "CCstep.t.ch" = cccv.ch$t.CV[1], "CCstep.Qch.mAh" = cccv.ch$Qch.mAh[1], "CCstep.Qch.mAh.g" = NA,
                             "CVstep.t.ch" = rev(cccv.ch$t.CV)[1] - cccv.ch$t.CV[1], "CVstep.Qch.mAh" = rev(cccv.ch$Qch.mAh)[1] - cccv.ch$Qch.mAh[1], "CVstep.Qch.mAh.g" = NA,
                             "time.s.dc" = t.tot.dc, "CCstep.t.dc" = cccv.dc$t.CV[1], "CCstep.Qdc.mAh" = cccv.dc$Qdc.mAh[1], "CCstep.Qdc.mAh.g" = NA,
                             "CVstep.t.dc" = rev(cccv.dc$t.CV)[1] - cccv.dc$t.CV[1], "CVstep.Qdc.mAh" = rev(cccv.dc$Qdc.mAh)[1] - cccv.dc$Qdc.mAh[1], "CVstep.Qdc.mAh.g" = NA
          )

        }else if(nrow(cccv.ch) == 0 && nrow(cccv.dc) != 0){

          cccv.i <- data.frame("CycNr" = i, "Qch.mAh" = Qch.tot, "chk.sum1" = NA, "Qch.mAh.g" = NA, "Qdc.mAh" = Qdc.tot, "chk.sum2" = NA, "Qdc.mAh.g" = NA, "CE" = NA,
                               "LowerCutoff" = min(cccv.dc$Ewe.V), "UpperCutoff" = NA,
                               "time.s.ch" = t.tot.ch, "CCstep.t.ch" = NA, "CCstep.Qch.mAh" = NA, "CCstep.Qch.mAh.g" = NA, "CVstep.t.ch" = NA, "CVstep.Qch.mAh" = NA, "CVstep.Qch.mAh.g" = NA,
                               "time.s.dc" = t.tot.dc, "CCstep.t.dc" = cccv.dc$t.CV[1], "CCstep.Qdc.mAh" = cccv.dc$Qdc.mAh[1], "CCstep.Qdc.mAh.g" = NA,
                               "CVstep.t.dc" = rev(cccv.dc$t.CV)[1] - cccv.dc$t.CV[1], "CVstep.Qdc.mAh" = rev(cccv.dc$Qdc.mAh)[1] - cccv.dc$Qdc.mAh[1], "CVstep.Qdc.mAh.g" = NA
          )

        }else if(nrow(cccv.ch) != 0 && nrow(cccv.dc) == 0){

          cccv.i <- data.frame("CycNr" = i, "Qch.mAh" = Qch.tot, "chk.sum1" = NA, "Qch.mAh.g" = NA, "Qdc.mAh" = Qdc.tot, "chk.sum2" = NA, "Qdc.mAh.g" = NA, "CE" = NA,
                               "LowerCutoff" = NA, "UpperCutoff" = max(cccv.ch$Ewe.V),
                               "time.s.ch" = t.tot.ch, "CCstep.t.ch" = cccv.ch$t.CV[1], "CCstep.Qch.mAh" = cccv.ch$Qch.mAh[1], "CCstep.Qch.mAh.g" = NA,
                               "CVstep.t.ch" = rev(cccv.ch$t.CV)[1] - cccv.ch$t.CV[1], "CVstep.Qch.mAh" = rev(cccv.ch$Qch.mAh)[1] - cccv.ch$Qch.mAh[1], "CVstep.Qch.mAh.g" = NA,
                               "time.s.dc" = t.tot.dc, "CCstep.t.dc" = NA, "CCstep.Qdc.mAh" = NA, "CCstep.Qdc.mAh.g" = NA, "CVstep.t.dc" = NA, "CVstep.Qdc.mAh" = NA, "CVstep.Qdc.mAh.g" = NA
          )

          }else{

            cccv.i <- data.frame("CycNr" = i, "Qch.mAh" = Qch.tot, "chk.sum1" = NA, "Qch.mAh.g" = NA, "Qdc.mAh" = Qdc.tot, "chk.sum2" = NA, "Qdc.mAh.g" = NA, "CE" = NA,
                                 "LowerCutoff" = NA, "UpperCutoff" = NA,
                                 "time.s.ch" = t.tot.ch, "CCstep.t.ch" = cccv.ch$t.CV[1], "CCstep.Qch.mAh" = NA, "CCstep.Qch.mAh.g" = NA, "CVstep.t.ch" = NA, "CVstep.Qch.mAh" = NA, "CVstep.Qch.mAh.g" = NA,
                                 "time.s.dc" = t.tot.dc, "CCstep.t.dc" = NA, "CCstep.Qdc.mAh" = NA, "CCstep.Qdc.mAh.g" = NA, "CVstep.t.dc" = NA, "CVstep.Qdc.mAh" = NA, "CVstep.Qdc.mAh.g" = NA
            )
        }

        cccv.list[[k]] <-  cccv.i
        k <- k+1
      }

      CCCV <- do.call(rbind, cccv.list)

      CCCV$CCstep.Qch.mAh.g <- CCCV$CCstep.Qch.mAh/AMmass
      CCCV$CCstep.Qdc.mAh.g <- CCCV$CCstep.Qdc.mAh/AMmass
      CCCV$CVstep.Qch.mAh.g <- CCCV$CVstep.Qch.mAh/AMmass
      CCCV$CVstep.Qdc.mAh.g <- CCCV$CVstep.Qdc.mAh/AMmass

      CCCV$Qch.mAh.g <- CCCV$Qch.mAh/AMmass
      CCCV$Qdc.mAh.g <- CCCV$Qdc.mAh/AMmass

      CCCV$chk.sum1 <- CCCV$CCstep.Qch.mAh + CCCV$CVstep.Qch.mAh
      CCCV$chk.sum2 <- CCCV$CCstep.Qdc.mAh + CCCV$CVstep.Qdc.mAh

      print("CCCV analysis finished")

    return(CCCV)
  }
