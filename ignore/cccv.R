#' @title CCCV.Analysis
#'
#' @description Biologic.CAP Evaluates CCCV-steps in galvanostatic cycling data
#'
#' @param raw
#'
#' @param AM.mass
#' @param type
#' @param warningsLOG
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
Biologic.CCCV <- function(raw, AMmass, cellType, warningsLOG){

  #binding variables to function Biologic.CAP
  cyc.nr <- Ns <- I.mA <- Ewe.V <- dE <-NULL
  CCCV <- data.frame()

  cellType = cellType
  warningsLOG= warningsLOG

  #CV step
  tmp3 <- raw %>%
          filter(I.mA != 0) %>%
          mutate(dE = c(0,diff(Ewe.V)),
                 type = 'ch'
    )

  if(cellType %in% c('halfcell-anode')){

    tmp3$type[tmp3$I.mA > 0] <- 'dc'

    for(i in 0:(max(tmp3$cyc.nr)-1)){

      warnings <- tryCatch(
        expr = {
          cccv.ch <- tmp3 %>%
            filter(cyc.nr == i & type == c('ch') & dE %in% c(-0.001, 0, 0.001))
            filter(Ewe.V <= min(Ewe.V)*1.1)
        },

        #nnma: no non-missing arguments to min (min applied to empty vector cccv.ch)
        warning = function(nnma){

          nnma
          #print(paste('warning in Biologic.CCCV:', nnma, 'cycle nr.: ', i))
        }
      )

      #check if data.frame has values
      if(!inherits(warnings, "simpeWarning")){

        cccv.ch.fl <- head(cccv.ch, 1)
        cccv.ch.ll <- tail(cccv.ch, 1)

      }else if(inherits(warnings, "simpeWarning")){

        warningEvent <- c(
          "script" = c("Biologic.CCCV"),
          "section" = c(paste0("cccv.ch", " cyc.nr: ", i)),
          "message" = warnings$message
        )
        warningsLOG <- rbind(warningsLOG, warningEvent)

        next
      }

      warnings <- tryCatch(
          expr = {
            cccv.dc <- tmp3 %>%
              filter(cyc.nr == i & type == c('dc') & dE %in% c(-0.001,0,0.001)) %>%
              filter(Ewe.V > max(Ewe.V)*0.98)
          },

          #nnma: no non-missing arguments to max (max applied to empty vector cccv.dc)
          warning = function(nnma){

            nnma
            #print(paste('warning in Biologic.CCCV: ', nnma, 'cycle nr.: ', i))

          }
        )

      #check if data.frame has values
      if(!inherits(warnings, "simpeWarning")){
          cccv.dc.fl <- head(cccv.dc, 1)
          cccv.dc.ll <- tail(cccv.dc, 1)
          }else{
            warningEvent <- c(
              "script" = c("Biologic.CCCV"),
              "section" = c(paste0("cccv.ch", " cyc.nr: ", i)),
              "message" = warnings$message
          )
            warningsLOG <- rbind(warningsLOG, warningEvent)

            next
          }

      charge.df <- tmp3 %>%
        filter(cyc.nr == i & type == c('ch'))
      t0.ch = charge.df$time.s[1]
      t.tot.ch =  rev(charge.df$time.s)[1] - t0.ch
      Qch.tot = max(charge.df$Qdc.mAh)

      discharge.df <- tmp3 %>%
        filter(cyc.nr == i & type == c('dc'))
      t0.dc = discharge.df$time.s[1]
      t.tot.dc =  rev(discharge.df$time.s)[1] - t0.dc
      Qdc.tot = max(discharge.df$Qch.mAh)

      cccv.df <- data.frame(
        "CycNr" = i,
        "LowerCutoff" = max(cccv.ch$Ewe.V),
        "I.charge.mA" = round(charge.df$I.mA[5],6),
        "ch.time.tot" = t.tot.ch,
        "Qch.tot.mAh" = Qch.tot,
        "Qch.tot.mAh.g" = Qch.tot/AMmass,
        "CCstep.t.ch" = t.tot.ch - (cccv.ch.ll$time.s - cccv.ch.fl$time.s),
        "CCstep.Qch.mAh" = Qch.tot - (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh),
        "CCstep.Qch.mAh.g" = (Qch.tot - (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh))/AMmass,
        "CVstep.t.ch" = cccv.ch.ll$time.s - cccv.ch.fl$time.s,
        "CVstep.Qch.mAh" = cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh,
        "CVstep.Qch.mAh.g" = (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh)/AMmass,
        "UpperCutoff" = min(cccv.dc$Ewe.V),
        "I.discharge.mA" = round(discharge.df$I.mA[5],6),
        "dc.time.tot" = t.tot.dc,
        "Qdc.tot.mAh" = Qdc.tot,
        "Qdc.tot.mAh.g" = Qdc.tot/AMmass,
        "CCstep.t.dc" = t.tot.dc - (cccv.dc.ll$time.s - cccv.dc.fl$time.s),
        "CCstep.Qdc.mAh" = Qdc.tot - (cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh),
        "CCstep.Qdc.mAh.g" = (Qdc.tot - (cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh))/AMmass,
        "CVstep.t.dc" = cccv.dc.ll$time.s - cccv.dc.fl$time.s,
        "CVstep.Qdc.mAh" = cccv.dc.ll$Qdc.mAh - cccv.dc.fl$Qdc.mAh,
        "CVstep.Qdc.mAh.g" = (cccv.dc.ll$Qdc.mAh - cccv.dc.fl$Qdc.mAh)/AMmass
      )
      CCCV <- rbind(CCCV, cccv.df)
      }
    }else if(cellType %in% c('halfcell-cathode', 'fullcell', 'LiS')){

      tmp3$type[tmp3$I.mA < 0] <- 'dc'

      for(i in 0:(max(tmp3$cyc.nr)-1)){

        warnings <- tryCatch(
          expr = {
            cccv.ch <- tmp3 %>%
              filter(cyc.nr == i & type == c('ch') & dE %in% c(-0.001, 0, 0.001))
            filter(Ewe.V <= max(Ewe.V)*0.98)
          },

          #nnma: no non-missing arguments to min (min applied to empty vector cccv.ch)
          warning = function(nnma){

            nnma
            #print(paste('warning in Biologic.CCCV:', nnma, 'cycle nr.: ', i))
          }
        )

        #check if data.frame has values
        if(!inherits(warnings, "simpeWarning")){

          cccv.ch.fl <- head(cccv.ch, 1)
          cccv.ch.ll <- tail(cccv.ch, 1)

        }else if(inherits(warnings, "simpeWarning")){

            warningEvent <- c(
              "script" = c("Biologic.CCCV"),
              "section" = c(paste0("cccv.ch", " cyc.nr: ", i)),
              "message" = warnings$message
            )
            warningsLOG <- rbind(warningsLOG, warningEvent)

            next
          }

        warnings <- tryCatch(
          expr = {
            cccv.dc <- tmp3 %>%
              filter(cyc.nr == i+1 & type == c('dc') & dE %in% c(-0.001,0,0.001)) %>%
              filter(Ewe.V > min(Ewe.V)*1.1)
          },

          #nnma: no non-missing arguments to max (max applied to empty vector cccv.dc)
          warning = function(nnma){

            nnma
            #print(paste('warning in Biologic.CCCV: ', nnma, 'cycle nr.: ', i))

          }
        )

        #check if data.frame has values
        if(!inherits(warnings, "simpeWarning")){
          cccv.dc.fl <- head(cccv.dc, 1)
          cccv.dc.ll <- tail(cccv.dc, 1)
          }else{
                warningEvent <- c(
                  "script" = c("Biologic.CCCV"),
                  "section" = c(paste0("cccv.ch", " cyc.nr: ", i)),
                  "message" = warnings$message
          )
          warningsLOG <- rbind(warningsLOG, warningEvent)

          next
        }

      charge.df <- tmp3 %>%
        filter(cyc.nr == i & type == c('ch'))
      t0.ch = charge.df$time.s[1]
      t.tot.ch =  rev(charge.df$time.s)[1] - t0.ch
      Qch.tot = max(charge.df$Qdc.mAh)

      discharge.df <- tmp3 %>%
        filter(cyc.nr == i+1 & type == c('dc'))
      t0.dc = discharge.df$time.s[1]
      t.tot.dc =  rev(discharge.df$time.s)[1] - t0.dc
      Qdc.tot = max(discharge.df$Qdc.mAh)

      cccv.df <- data.frame(
        "CycNr" = i,
        "LowerCutoff" = max(cccv.ch$Ewe.V),
        "I.charge.mA" = round(charge.df$I.mA[5],6),
        "ch.time.tot" = t.tot.ch,
        "Qch.tot.mAh" = Qch.tot,
        "Qch.tot.mAh.g" = Qch.tot/AMmass,
        "CCstep.t.ch" = t.tot.ch - (cccv.ch.ll$time.s - cccv.ch.fl$time.s),
        "CCstep.Qch.mAh" = Qch.tot - (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh),
        "CCstep.Qch.mAh.g" = (Qch.tot - (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh))/AMmass,
        "CVstep.t.ch" = cccv.ch.ll$time.s - cccv.ch.fl$time.s,
        "CVstep.Qch.mAh" = cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh,
        "CVstep.Qch.mAh.g" = (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh)/AMmass,
        "UpperCutoff" = min(cccv.dc$Ewe.V),
        "I.discharge.mA" = round(discharge.df$I.mA[5],6),
        "dc.time.tot" = t.tot.dc,
        "Qdc.tot.mAh" = Qdc.tot,
        "Qdc.tot.mAh.g" = Qdc.tot/AMmass,
        "CCstep.t.dc" = t.tot.dc - (cccv.dc.ll$time.s - cccv.dc.fl$time.s),
        "CCstep.Qdc.mAh" = Qdc.tot - (cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh),
        "CCstep.Qdc.mAh.g" = (Qdc.tot - (cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh))/AMmass,
        "CVstep.t.dc" = cccv.dc.ll$time.s - cccv.dc.fl$time.s,
        "CVstep.Qdc.mAh" = cccv.dc.ll$Qdc.mAh - cccv.dc.fl$Qdc.mAh,
        "CVstep.Qdc.mAh.g" = (cccv.dc.ll$Qdc.mAh - cccv.dc.fl$Qdc.mAh)/AMmass
      )
      CCCV <- rbind(CCCV, cccv.df)
      }
    }
  return(CCCV)
}
