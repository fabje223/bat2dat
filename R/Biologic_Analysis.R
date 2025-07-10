#' @title Biologic_Analysis
#'
#' @description Analysis of Biologic potentiostat data
#'
#' @param raw - object of Biologic.CAPA
#' @param AMmass - object of Biologic.CAPA
#' @param cycles - selected cycles to extract for voltage profiles
#' @param cellType - object of Biologic.CAPA
#' @param warningsLOG - test LOG to fetch errors
#'
#' @return capacity table
#'
#' @include Read0r2.R Evaluat0r.R Process0r.R Report0r.R
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom zoo rollapply
#'
#' @examples
#' \dontrun{
#' capa <- Biologic.CAP()
#'
#' VP <- Biologic.VP()
#' }

#' @export
#' @rdname Biologic_Analysis
#' @details Extract Cycling Data for Biologic Instruments
Biologic.CAPA <- function(raw, AMmass, cellType){

        #create empty result data.frame
        cap <- data.frame("CycNr" = numeric(), "time.s.ch" = numeric(), "time.s.dc" = numeric(), "I.mA.ch" = numeric(), "I.mA.dc" = numeric(),
                          "Qch.mAh" = numeric(), "Qdc.mAh" = numeric(), "CE" = numeric(), "Ewe.endCH" = numeric(), "Ewe.endDC" = numeric(),
                          "Edrop.ch" = numeric(), "Edrop.dc" = numeric(), "IntR.ch" = numeric(), "IntR.dc" = numeric())

        ##Analyse Biologic files:
        # most common setting: half cells are started from Ns0 in first half cycle
        # split files into charge and discharge sequences
        # ! EC lab files for anode half cells start in the second half of the sequence with a negative current (--> Ns parameter)
        # ! Sorting needs to be corrected for Ns in this case (Ns should be the same for one cycle, but is shifted by half a cycle)

        #binding variables to function Biologic.CAP
        cyc.nr <- Ns <- I.mA <- time.s <- Qch.mAh <- Qdc.mAh <- Qch.mAh.g <- Qdc.mAh.g <- CE <- Ewe.endCH <- Ewe.V <- NULL

                seq1.df <- data.frame()
                seq2.df <- data.frame()

                for(i in 0:(max(raw$cyc.nr)-1)){

                  # a bit misleading: first sequence in LiS is not a charge,
                  # but for the calculation of the IRdrop it works like a anode halfcell
                  if(cellType == 'halfcell-anode' || cellType == 'LiS'){

                        #first cycling sequence
                        ch <- raw %>%
                                  filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & I.mA != 0)
                        ch.IR <- raw %>%
                                  filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & I.mA == 0)

                        #second cycling sequence
                        dc <-   raw %>%
                                filter(cyc.nr == i+1 & Ns %in% c(seq(0,50,2)) & I.mA !=0)
                        dc.IR <- raw %>%
                                filter(cyc.nr == i+1 & Ns %in% c(seq(0,50,2)) & I.mA == 0)

                        #take last line of data.frame to obtain total capacity of that sequence
                        ch.ln <- head(tail(ch, 2),1)
                        ch.IR <- head(ch.IR, 1)

                        dc.ln <- head(tail(dc, 2),1)
                        dc.IR <- head(dc.IR, 1)

                      }else if(cellType ==  "halfcell-cathode" || cellType == "fullcell"){

                        #first cycling sequence
                        ch <- raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & I.mA != 0)
                        ch.IR <- raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & I.mA == 0)

                        #second cycling sequence
                        dc <-   raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(2,50,2)) & I.mA !=0)
                        dc.IR <- raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(2,50,2)) & I.mA == 0)

                        #take last line of data.frame to obtain total capacity of that sequence
                        ch.ln <- head(tail(ch, 2),1)
                        ch.IR <- head(ch.IR, 1)

                        dc.ln <- head(tail(dc, 2),1)
                        dc.IR <- head(dc.IR, 1)
                    }

                  if(nrow(dc) == 0 || nrow(dc.IR) == 0 || nrow(ch) == 0 || nrow(ch.IR) == 0){

                          next

                    }else if(cellType == 'halfcell-anode' || cellType == 'LiS'){

                            Edrop.ch <- ch.IR$Ewe.V - ch.ln$Ewe.V
                            IntR.ch <- Edrop.ch / (ch.ln$I.mA*(-0.001))
                            ch.df <- data.frame(ch.ln, Edrop.ch, IntR.ch)
                            seq1.df <- rbind(seq1.df, ch.df)

                            Edrop.dc <- dc.ln$Ewe.V - dc.IR$Ewe.V
                            IntR.dc <- Edrop.dc / (dc.ln$I.mA*(0.001))
                            dc.df <- data.frame(dc.ln, Edrop.dc, IntR.dc)
                            seq2.df <- rbind(seq2.df, dc.df)

                    }else if(cellType ==  "halfcell-cathode" || cellType == "fullcell"){

                            Edrop.ch <- ch.ln$Ewe.V - ch.IR$Ewe.V
                            IntR.ch <- Edrop.ch / (ch.ln$I.mA*(0.001))
                            ch.df <- data.frame(ch.ln, Edrop.ch, IntR.ch)
                            seq1.df <- rbind(seq1.df, ch.df)

                            Edrop.dc <- dc.IR$Ewe.V - dc.ln$Ewe.V
                            IntR.dc <- Edrop.dc / (dc.ln$I.mA*(-0.001))
                            dc.df <- data.frame(dc.ln, Edrop.dc, IntR.dc)
                            seq2.df <- rbind(seq2.df, dc.df)
                    }
                }

                #remove trash
                rm(ch)
                rm(dc)
                rm(ch.ln)
                rm(dc.ln)
                rm(ch.IR)
                rm(dc.IR)
                rm(ch.df)
                rm(dc.df)

                # lapply loop stopped working with introduction of Edrop.ch
                #seq2 <- lapply(0:max(df.cycles$cyc.nr)-1, function(i){
                # return(tmp)
                #})
                #seq2.df <- do.call(rbind, seq2)

                #check that both data.frames have same length
                #if cell was stopped after half a cycle this is not the case!
                if(nrow(seq2.df) < nrow(seq1.df)) seq1.df <- seq1.df[1:nrow(seq2.df),]
                if(nrow(seq1.df) < nrow(seq2.df)) seq2.df <- seq2.df[1:nrow(seq1.df),]

                #write results in new data.frame "cap"
                if(nrow(seq2.df) == 0) {
                        return(cap)
                } else if(cellType == "halfcell-anode"){
                        # fill result data.frame
                        cap <- data.frame("CycNr" = seq2.df$cyc.nr,
                                          "time.s.ch" = seq1.df$time.s,
                                          "time.s.dc" = seq2.df$time.s,
                                          #"I.mA.ch" = seq1.df$I.mA,
                                          #"I.mA.dc" = seq2.df$I.mA,
                                          "Qch.mAh" = seq1.df$Qdc.mAh,
                                          "Qdc.mAh" = seq2.df$Qch.mAh,
                                          "Qch.mAh.g" = seq1.df$Qdc.mAh/AMmass,
                                          "Qdc.mAh.g" = seq2.df$Qch.mAh/AMmass,
                                          "CE" = seq2.df$Qch.mAh/seq1.df$Qdc.mAh,
                                          "Ewe.endCH" = seq1.df$Ewe.V,
                                          "Ewe.endDC" = seq2.df$Ewe.V,
                                          "Edrop.ch" = seq1.df$Edrop.ch,
                                          "Edrop.dc" = seq2.df$Edrop.dc,
                                          "IntR.ch" = seq1.df$IntR.ch,
                                          "IntR.dc" = seq2.df$IntR.dc
                                          )

                } else if(cellType == "halfcell-cathode" || cellType ==  "fullcell"){
                        # fill result data.frame
                        cap <- data.frame("CycNr" = seq2.df$cyc.nr,
                                          "time.s.ch" = seq1.df$time.s,
                                          "time.s.dc" = seq2.df$time.s,
                                          #"I.mA.ch" = seq1.df$I.mA,
                                          #"I.mA.dc" = seq2.df$I.mA,
                                          "Qch.mAh" = seq1.df$Qch.mAh,
                                          "Qdc.mAh" = seq2.df$Qdc.mAh,
                                          "Qch.mAh.g" = seq1.df$Qch.mAh/AMmass,
                                          "Qdc.mAh.g" = seq2.df$Qdc.mAh/AMmass,
                                          "CE" = seq2.df$Qdc.mAh/seq1.df$Qch.mAh,
                                          "Ewe.endCH" = seq1.df$Ewe.V,
                                          "Ewe.endDC" = seq2.df$Ewe.V,
                                          "Edrop.ch" = seq1.df$Edrop.ch,
                                          "Edrop.dc" = seq2.df$Edrop.dc,
                                          "IntR.ch" = seq1.df$IntR.ch,
                                          "IntR.dc" = seq2.df$IntR.dc
                                          )

                } else if(cellType == "LiS"){
                        # workaround: cap <- data.frame(...) kept throwing false error (no matching row number)
                        cap <- data.frame("CycNr" = seq2.df$cyc.nr,
                                          "time.s.ch" = seq2.df$time.s,
                                          "time.s.dc" = seq1.df$time.s,
                                          #"I.mA.ch" = seq1.df$I.mA,
                                          #"I.mA.dc" = seq2.df$I.mA,
                                          "Qch.mAh" = seq1.df$Qdc.mAh,
                                          "Qdc.mAh" = seq2.df$Qch.mAh,
                                          "Qch.mAh.g" = seq1.df$Qdc.mAh/AMmass,
                                          "Qdc.mAh.g" = seq2.df$Qch.mAh/AMmass,
                                          "CE" = seq1.df$Qdc.mAh/seq2.df$Qch.mAh,
                                          "Ewe.endCH" = seq2.df$Ewe.V,
                                          "Ewe.endDC" = seq1.df$Ewe.V,
                                          "Edrop.ch" = seq2.df$Edrop.dc,
                                          "Edrop.dc" = seq1.df$Edrop.ch,
                                          "IntR.ch" = seq1.df$IntR.ch,
                                          "IntR.dc" = seq2.df$IntR.dc
                                          )
                        #cap <- cap %>%
                         #       select(cyc.nr, Ns, time.s, Qch.mAh, Qdc.mAh, Qch.mAh.g, Qdc.mAh.g, CE, Ewe.endCH, Ewe.V, Edrop.ch, Edrop.dc)
                        #colnames(cap) <- c("CycNr", "time.s.ch", "time.s.dc", "Qch.mAh", "Qdc.mAh", "Qch.mAh.g", "Qdc.mAh.g", "CE", "Ewe.endCH", "Ewe.endDC", "Edrop.ch", "Edrop.dc")

                        }
                return(cap)
}

#' @export
#' @rdname Biologic_Analysis
#' @details Evaluates CCCV-steps in galvanostatic cycling data
Biologic.CCCV <- function(raw, AMmass, cellType, warningsLOG){

        #binding variables to function Biologic.CAP
        cyc.nr <- Ns <- I.mA <- Ewe.V <- dE <-NULL

        #CV step
        tmp3 <- raw %>%
                #filter(cyc.nr %in% c(0:(max(cyc.nr)-1))) %>%
                mutate( Ewe.V = round(Ewe.V, 3),
                        dE = c(0, diff(Ewe.V))
                        )

        if(cellType %in% c("halfcell-anode", 'LiS')){

                CCCV <- data.frame()

                for(i in 0:(max(tmp3$cyc.nr)-1)){
                        #print(i)
                        #i <- 0

                        warnings <- tryCatch(
                                        expr = {
                                                cccv.ch <- tmp3 %>%
                                                filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & dE %in% c(-0.001,0,0.001) & I.mA != 0) %>%
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
                                                filter(cyc.nr == i+1 & Ns %in% c(seq(0,50,2)) & dE %in% c(-0.001,0,0.001) & I.mA != 0) %>%
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
                                filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & I.mA !=0)
                        t0.ch = charge.df$time.s[1]
                        t.tot.ch =  rev(charge.df$time.s)[1] - t0.ch
                        Qch.tot = max(charge.df$Qdc.mAh)

                        discharge.df <- tmp3 %>%
                                filter(cyc.nr == i+1 & Ns %in% c(seq(0,50,2)) & I.mA !=0)
                        t0.dc = discharge.df$time.s[1]
                        t.tot.dc =  rev(discharge.df$time.s)[1] - t0.dc
                        Qdc.tot = max(discharge.df$Qch.mAh)

                        cccv.df <- data.frame(
                                "CycNr" = i+1,
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
        }else if(cellType %in% c("halfcell-cathode", "fullcell")){

                CCCV <- data.frame()

                for(i in 0:(max(tmp3$cyc.nr)-1)){

                        cccv.ch <- tmp3 %>%
                                   filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & dE %in% c(-0.001,0,0.001) & I.mA != 0) %>%
                                   filter(Ewe.V >= max(Ewe.V)*0.99)

                        #check if data.frame has values
                        if(nrow(cccv.ch) != 0){
                                cccv.ch.fl <- head(cccv.ch, 1)
                                cccv.ch.ll <- tail(cccv.ch, 1)
                        }else{  cccv.df <- NA
                                next
                                }

                        cccv.dc <- tmp3 %>%
                                filter(cyc.nr == i & Ns %in% c(seq(0,50,2)) & dE %in% c(-0.001,0,0.001) & I.mA != 0) %>%
                                filter(Ewe.V <= min(Ewe.V)*1.01)

                        #check if data.frame has values
                        if(nrow(cccv.dc) != 0){
                                cccv.dc.fl <- head(cccv.dc, 1)
                                cccv.dc.ll <- tail(cccv.dc, 1)
                        }else{  cccv.df <- NA
                                next
                                }

                        charge.df <- tmp3 %>%
                                     filter(cyc.nr == i & Ns %in% c(seq(1,50,2)) & I.mA !=0)
                        t0.ch = charge.df$time.s[1]
                        t.tot.ch =  rev(charge.df$time.s)[1] - t0.ch
                        Qch.tot = max(charge.df$Qch.mAh)

                        discharge.df <- tmp3 %>%
                                        filter(cyc.nr == i & Ns %in% c(seq(0,50,2)) & I.mA !=0)
                        t0.dc = discharge.df$time.s[1]
                        t.tot.dc =  rev(discharge.df$time.s)[1] - t0.dc
                        Qdc.tot = max(discharge.df$Qdc.mAh)

                        cccv.df <- data.frame(
                                        "CycNr" = i,
                                        "UpperCutoff" = max(cccv.ch$Ewe.V),
                                        "I.charge.mA" = round(charge.df$I.mA[5],6),
                                        "ch.time.tot" = t.tot.ch,
                                        "Qch.tot.mAh" = Qch.tot,
                                        "Qch.tot.mAh.g" = Qch.tot/AMmass,
                                        "CCstep.t.ch" = t.tot.ch - (cccv.ch.ll$time.s - cccv.ch.fl$time.s),
                                        "CCstep.Qch.mAh" = Qch.tot - (cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh),
                                        "CCstep.Qch.mAh.g" = (Qch.tot - (cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh))/AMmass,
                                        "CVstep.t.ch" = cccv.ch.ll$time.s - cccv.ch.fl$time.s,
                                        "CVstep.Qch.mAh" = cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh,
                                        "CVstep.Qch.mAh.g" = (cccv.ch.ll$Qch.mAh - cccv.ch.fl$Qch.mAh)/AMmass,
                                        "LowerCutoff" = min(cccv.dc$Ewe.V),
                                        "I.discharge.mA" = round(discharge.df$I.mA[5],6),
                                        "dc.time.tot" = t.tot.dc,
                                        "Qdc.tot.mAh" = Qdc.tot,
                                        "Qdc.tot.mAh.g" = Qdc.tot/AMmass,
                                        "CCstep.t.dc" = t.tot.dc - (cccv.dc.ll$time.s - cccv.dc.fl$time.s),
                                        "CCstep.Qdc.mAh" = Qdc.tot - (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh),
                                        "CCstep.Qdc.mAh.g" = (Qdc.tot - (cccv.ch.ll$Qdc.mAh - cccv.ch.fl$Qdc.mAh))/AMmass,
                                        "CVstep.t.dc" = cccv.dc.ll$time.s - cccv.dc.fl$time.s,
                                        "CVstep.Qdc.mAh" = cccv.dc.ll$Qdc.mAh - cccv.dc.fl$Qdc.mAh,
                                        "CVstep.Qdc.mAh.g" = (cccv.dc.ll$Qdc.mAh - cccv.dc.fl$Qdc.mAh)/AMmass
                        )
                        CCCV <- rbind(CCCV, cccv.df)
                }

        }
        return(CCCV)
}

#' @export
#' @rdname Biologic_Analysis
#' @details Biologic.VP extracts voltage profiles for selected cycles from cycling data
Biologic.VP <- function(raw, AMmass, cycles, cellType){

        #extract columns needed from result file of instrument, using piping operators
        #df.VP <- tmp %>%
        #         select('cycle.number', 'time.s', 'Ns', 'Ewe.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
        #        mutate(time.s = time.s - min(time.s))
        #colnames(df.VP) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

        #binding variables to function Biologic.CAP
        cyc.nr <- Ns <- I.mA <- Qch.mAh <- Qdc.mAh <- Ewe.V <- Ewe.V.rnd <- diff.Q <- diff.E <- diff.cap <- NULL

        #create empty result data.frame and empty VP.list (temporary storage)
        VPprofiles <- data.frame("CycNr" = numeric(), "time.s" = numeric(), "I.mA" = numeric(),
                                 "Qch.mAh" = numeric(), "Qdc.mAh" = numeric(), "Qloop" = numeric(),
                                 "Ewe.V" = numeric(), "diffcap" = numeric(), "dQdV.mav3" = numeric(),
                                 "type" = c())
        VP.list <- list()
        k = 1

        #reduce values in "cycles" vector to those that are actually available in the specific data.frame
        idx <- cycles[cycles %in% c(1:max(raw$cyc.nr)-1)]

        # select cycle
        #for(i in idx){

        if(cellType %in% c("halfcell-anode", 'LiS')){

                # loop through selected cycles
                for(i in idx){

                        VP.ch <- raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(1,50, 2))) %>%
                                #arrange(desc(Ewe.V)) %>%
                                mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                                       'diff.Q' = c(0, diff(Qdc.mAh)),
                                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                                       'diff.cap' = sqrt((diff.Q/diff.E)^2)*-1,
                                       'type' = 'ch') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA))


                        VP.dc <- raw %>%
                                filter(cyc.nr == i+1 & Ns %in% c(seq(0,50,2))) %>%
                                #arrange(Ewe.V) %>%
                                mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                                       'diff.Q' = c(0, diff(Qch.mAh)),
                                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                                       'diff.cap' = sqrt((diff.Q/diff.E)^2),#diff.Q/diff.E, #
                                       'type' = 'dc') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA))

                        VP.df <- rbind(VP.ch, VP.dc)

                        #remove infinite (+/- Inf) values for dQdV
                        VP.df$dQdV.mav[!is.finite(VP.df$dQdV.mav)] <- NA

                        #correct cycle number (is shifted by half a sequence in half cells)
                        VP.df$cyc.nr <- i

                        #remove inf values in diff.cap
                        #VP.df <- VP.df[!(VP.df$diff.Q == 0 | VP.df$diff.E == 0),] #is.infinite(dat$diff.cap) |

                        #Reset time of each sequence to 0
                        VP.df$time.s <- VP.df$time.s - min(VP.df$time.s)

                        # New column --> Qloop: create a column where capacity is counted up until the sequence changes and then counted back down.
                        VP.df$Qdc.mAh[VP.df$Qdc.mAh == 0] <- max(VP.df$Qdc.mAh)
                        VP.df$Qloop <- VP.df$Qdc.mAh - VP.df$Qch.mAh

                        # modify Q.ch and Q.dc columns for plotting, e.g. in Origin
                        VP.df$Ewe.V.ch <- VP.df$Ewe.V
                        VP.df$Qdc.mAh[VP.df$Qch.mAh != 0] <- NA
                        VP.df$Ewe.V.ch[is.na(VP.df$Qdc.mAh)] <- NA

                        VP.df$Ewe.V.dc <- VP.df$Ewe.V
                        VP.df$Qch.mAh[VP.df$Qdc.mAh != 0] <- NA
                        VP.df$Ewe.V.dc[is.na(VP.df$Qch.mAh)] <- NA

                        # create new data.frame
                        VPprofile <- data.frame(
                                "CycNr" = VP.df$cyc.nr, "time.s" = VP.df$time.s, "I.mA" = VP.df$I.mA, "Ewe.V" = VP.df$Ewe.V,
                                "Qch.mAh" = VP.df$Qdc.mAh, "Qch.mAh.g" = VP.df$Qdc.mAh/AMmass, "Ewe.V.ch" = VP.df$Ewe.V.ch,
                                "Qdc.mAh" = VP.df$Qch.mAh, "Qdc.mAh.g" = VP.df$Qch.mAh/AMmass, "Ewe.V.dc" = VP.df$Ewe.V.dc,
                                "Qloop" = VP.df$Qloop, "Qloop.mAh.g" = VP.df$Qloop/AMmass, "Ewe.V.rnd" = VP.df$Ewe.V.rnd,
                                "diffcap" = VP.df$diff.cap, "dQdV.mav3" = VP.df$dQdV.mav, "type" = VP.df$type)

                        VP.list[[k]] <- VPprofile
                        k = k+1
                        }

        } else if(cellType %in% c("halfcell-cathode", "fullcell")){

                # loop through selected cycles
                for(i in idx){

                        VP.ch <- raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(1,50, 2)) & I.mA > 0) %>%
                                #arrange(desc(Ewe.V)) %>%
                                mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                                       'diff.Q' = c(0, diff(Qdc.mAh)),
                                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                                       'diff.cap' = sqrt((diff.Q/diff.E)^2)*-1,
                                       'type' = 'ch') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA))
                        VP.ch <- tail(VP.ch, -1)

                        VP.dc <- raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(2,50,2)) & I.mA < 0) %>%
                                #arrange(Ewe.V) %>%
                                mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                                       'diff.Q' = c(0, diff(Qch.mAh)),
                                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                                       'diff.cap' = sqrt((diff.Q/diff.E)^2),#diff.Q/diff.E, #
                                       'type' = 'dc') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA))

                        VP.df <- rbind(VP.ch, VP.dc)

                        #remove infinite (+/- Inf) values for dQdV
                        VP.df$dQdV.mav[!is.finite(VP.df$dQdV.mav)] <- NA

                        VP.df <- rbind(VP.ch, VP.dc)

                        #remove inf values in diff.cap
                        #VP.df <- VP.df[!(VP.df$diff.Q == 0 | VP.df$diff.E == 0),] #is.infinite(dat$diff.cap) |

                        #Reset time of each sequence to 0
                        VP.df$time.s <- VP.df$time.s - min(VP.df$time.s)

                        # New column --> Qloop: create a column where capacity is counted up until the sequence changes and then counted back down.
                        VP.df$Qch.mAh[VP.df$Qch.mAh == 0] <- max(VP.df$Qch.mAh)
                        VP.df$Qloop <- VP.df$Qch.mAh - VP.df$Qdc.mAh

                        # modify Q.ch and Q.dc columns <- l[[for plotting, e.g. in Origin
                        VP.df$Ewe.V.ch <- VP.df$Ewe.V
                        VP.df$Qch.mAh[VP.df$Qdc.mAh != 0] <- NA
                        VP.df$Ewe.V.ch[is.na(VP.df$Qch.mAh)] <- NA

                        VP.df$Ewe.V.dc <- VP.df$Ewe.V
                        VP.df$Qdc.mAh[VP.df$Qch.mAh != 0] <- NA
                        VP.df$Ewe.V.dc[is.na(VP.df$Qdc.mAh)] <- NA

                        # create new data.frame
                        VPprofile <- data.frame(
                                "CycNr" = VP.df$cyc.nr+1, "time.s" = VP.df$time.s, "I.mA" = VP.df$I.mA, "Ewe.V" = VP.df$Ewe.V,
                                "Qch.mAh" = VP.df$Qch.mAh, "Qch.mAh.g" = VP.df$Qch.mAh/AMmass, "Ewe.V.ch" = VP.df$Ewe.V.ch,
                                "Qdc.mAh" = VP.df$Qdc.mAh, "Qdc.mAh.g" = VP.df$Qdc.mAh/AMmass, "Ewe.V.dc" = VP.df$Ewe.V.dc,
                                "Qloop" = VP.df$Qloop, "Qloop.mAh.g" = VP.df$Qloop/AMmass, "Ewe.V.rnd" = VP.df$Ewe.V.rnd,
                                 "diffcap" = VP.df$diff.cap, "dQdV.mav3" = VP.df$dQdV.mav, "type" = VP.df$type)

                VP.list[[k]] <- VPprofile
                k = k+1
                }

        }
        return(VP.list)
}
