#' @title Biologic_Analysis
#'
#' @description Analysis of Biologic potentiostat data
#'
#' @param raw - object of Biologic.CAPA
#' @param AMmass - object of Biologic.CAPA
#' @param cycles - selected cycles to extract for voltage profiles
#' @param cellType - object of Biologic.CAPA
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
                        cap <- data.frame("CycNr" = (seq2.df$cyc.nr)+1,
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
#' @details Biologic.VP extracts voltage profiles for selected cycles from cycling data
Biologic.VP <- function(raw, AMmass, cycles, cellType){

        #extract columns needed from result file of instrument, using piping operators
        #df.VP <- tmp %>%
        #         select('cycle.number', 'time.s', 'Ns', 'Ewe.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
        #        mutate(time.s = time.s - min(time.s))
        #colnames(df.VP) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

        #binding variables to function Biologic.CAP
        cyc.nr <- Ns <- I.mA <- Qch.mAh <- Qdc.mAh <- Ewe.V <- Ewe.V.rnd <- diff.Q <- diff.E <- diff.cap <- diff.pot <- NULL

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
                                       'diff.pot' = sqrt((diff.E/diff.Q)^2)*-1,
                                       'type' = 'ch') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA),
                                       "dVdQ.mav" = rollapply(diff.pot, 3, mean, fill=NA))


                        VP.dc <- raw %>%
                                filter(cyc.nr == i+1 & Ns %in% c(seq(0,50,2))) %>%
                                #arrange(Ewe.V) %>%
                                mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                                       'diff.Q' = c(0, diff(Qch.mAh)),
                                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                                       'diff.cap' = sqrt((diff.Q/diff.E)^2),
                                       'diff.pot' = sqrt((diff.E/diff.Q)^2),
                                       'type' = 'dc') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA),
                                       "dVdQ.mav" = rollapply(diff.pot, 3, mean, fill=NA))

                        VP.df <- rbind(VP.ch, VP.dc)

                        #remove infinite (+/- Inf) values for dQdV
                        VP.df$dQdV.mav[is.infinite(VP.df$dQdV.mav)] <- NA
                        VP.df$dVdQ.mav[is.infinite(VP.df$dVdQ.mav)] <- NA

                        #correct cycle number (is shifted by half a sequence in half cells)
                        VP.df$cyc.nr <- i

                        #remove inf values in diff.cap
                        #VP.df <- VP.df[!(VP.df$diff.Q == 0 | VP.df$diff.E == 0),] #is.infinite(dat$diff.cap) |

                        #Reset time of each sequence to 0
                        VP.df$time.s <- VP.df$time.s - min(VP.df$time.s)

                        # New column --> Qloop: create a column where capacity is counted up until the sequence changes and then counted back down.
                        VP.df$Qch.mAh[VP.df$Qch.mAh == 0] <- max(VP.df$Qch.mAh)
                        VP.df$Qloop <- VP.df$Qch.mAh - VP.df$Qdc.mAh

                        # sets Qcharge to NA during discharge (and vice versa); for easier plotting e.g. in Origin
                        VP.df$Ewe.V.ch <- VP.df$Ewe.V
                        VP.df$Qdc.mAh[VP.df$type == 'ch'] <- NA
                        VP.df$Ewe.V.ch[VP.df$type == 'dc'] <- NA

                        VP.df$Ewe.V.dc <- VP.df$Ewe.V
                        VP.df$Qch.mAh[VP.df$type == 'dc'] <- NA
                        VP.df$Ewe.V.dc[VP.df$type == 'ch'] <- NA

                        # create new data.frame
                        VPprofile <- data.frame(
                                        "CycNr" = VP.df$cyc.nr,
                                        "time.s" = VP.df$time.s,
                                        "I.mA" = VP.df$I.mA,
                                        "Ewe.V" = VP.df$Ewe.V,
                                        "Qch.mAh" = VP.df$Qch.mAh,
                                        "Qch.mAh.g" = VP.df$Qch.mAh/AMmass,
                                        "Ewe.V.ch" = VP.df$Ewe.V.ch,
                                        "Qdc.mAh" = VP.df$Qdc.mAh,
                                        "Qdc.mAh.g" = VP.df$Qdc.mAh/AMmass,
                                        "Ewe.V.dc" = VP.df$Ewe.V.dc,
                                        "Qloop" = VP.df$Qloop,
                                        "Qloop.mAh.g" = VP.df$Qloop/AMmass,
                                        "Ewe.V.rnd" = VP.df$Ewe.V.rnd,
                                        "diffcap" = VP.df$diff.cap,
                                        "dQdV.mav3" = VP.df$dQdV.mav,
                                        "dVdQ.mav3" = VP.df$dVdQ.mav,
                                        "type" = VP.df$type)

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
                                       'diff.Q' = c(0, diff(Qch.mAh)),
                                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                                       'diff.cap' = sqrt((diff.Q/diff.E)^2)*-1,
                                       'diff.pot' = sqrt((diff.E/diff.Q)^2)*-1,
                                       'type' = 'ch') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA),
                                       "dVdQ.mav" = rollapply(diff.pot, 3, mean, fill=NA))
                        VP.ch <- tail(VP.ch, -1)

                        VP.dc <- raw %>%
                                filter(cyc.nr == i & Ns %in% c(seq(2,50,2)) & I.mA < 0) %>%
                                #arrange(Ewe.V) %>%
                                mutate('Ewe.V.rnd'= round(Ewe.V, 4),
                                       'diff.Q' = c(0, diff(Qdc.mAh)),
                                       'diff.E' = c(0, diff(Ewe.V.rnd)),
                                       'diff.cap' = sqrt((diff.Q/diff.E)^2),
                                       'diff.pot' = sqrt((diff.E/diff.Q)^2),
                                       'type' = 'dc') %>%
                                mutate("dQdV.mav" = rollapply(diff.cap, 3, mean, fill=NA),
                                       "dVdQ.mav" = rollapply(diff.pot, 3, mean, fill=NA))

                        VP.df <- rbind(VP.ch, VP.dc)

                        #remove infinite (+/- Inf) values for dQdV
                        VP.df$dQdV.mav[is.infinite(VP.df$dQdV.mav)] <- NA
                        VP.df$dVdQ.mav[is.infinite(VP.df$dVdQ.mav)] <- NA

                        #remove inf values in diff.cap
                        #VP.df <- VP.df[!(VP.df$diff.Q == 0 | VP.df$diff.E == 0),] #is.infinite(dat$diff.cap) |

                        #Reset time of each sequence to 0
                        VP.df$time.s <- VP.df$time.s - min(VP.df$time.s)

                        # New column --> Qloop: create a column where capacity is counted up until the sequence changes and then counted back down.
                        VP.df$Qch.mAh[VP.df$Qch.mAh == 0] <- max(VP.df$Qch.mAh)
                        VP.df$Qloop <- VP.df$Qch.mAh - VP.df$Qdc.mAh

                        # modify Q.ch and Q.dc columns <- l[[for plotting, e.g. in Origin
                        VP.df$Ewe.V.ch <- VP.df$Ewe.V
                        VP.df$Qdc.mAh[VP.df$type == 'ch'] <- NA
                        VP.df$Ewe.V.ch[VP.df$type == 'dc'] <- NA

                        VP.df$Ewe.V.dc <- VP.df$Ewe.V
                        VP.df$Qch.mAh[VP.df$type == 'dc'] <- NA
                        VP.df$Ewe.V.dc[VP.df$type == 'ch'] <- NA

                        # create new data.frame
                        VPprofile <- data.frame(
                                "CycNr" = VP.df$cyc.nr+1,
                                "time.s" = VP.df$time.s,
                                "I.mA" = VP.df$I.mA,
                                "Ewe.V" = VP.df$Ewe.V,
                                "Qch.mAh" = VP.df$Qch.mAh,
                                "Qch.mAh.g" = VP.df$Qch.mAh/AMmass,
                                "Ewe.V.ch" = VP.df$Ewe.V.ch,
                                "Qdc.mAh" = VP.df$Qdc.mAh,
                                "Qdc.mAh.g" = VP.df$Qdc.mAh/AMmass,
                                "Ewe.V.dc" = VP.df$Ewe.V.dc,
                                "Qloop" = VP.df$Qloop,
                                "Qloop.mAh.g" = VP.df$Qloop/AMmass,
                                "Ewe.V.rnd" = VP.df$Ewe.V.rnd,
                                "diffcap" = VP.df$diff.cap,
                                "dQdV.mav3" = VP.df$dQdV.mav,
                                "dVdQ.mav3" = VP.df$dVdQ.mav,
                                "type" = VP.df$type)

                VP.list[[k]] <- VPprofile
                k = k+1
                }

        }
        return(VP.list)
}
