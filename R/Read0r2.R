#' @title Read0r2.R
#'
#' @description Imports experimental data from data folders, depending on data format/instrument.
#' Experimental data is converted into a uniform format for further processing
#'
#' @param dir directory to raw data folder
#' @param filename sample name
#' @param f.path full file path with ending
#'
#' @return returns data.frame with raw data
#'
#' @include Process0r.R Report0r.R
#' @importFrom utils read.table
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @import readxl RODBC
#'
#' @examples
#' \dontrun{
#' raw <- BCSraw()
#' }

#' @export
#' @rdname Read0r
#' @details read .txt files from raw data directory (for Biologic BCS)
BCSraw <- function(dir, filename){

  #binding variables locally to function BCSraw()
  time.s <- NULL

  # read .txt file
  # little workaround required:
  # some computers export values with "," decimal, others with "." decimal sign
  tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
  #save column names for after the operation
  tmp.colnames <- colnames(tmp)
  #convert all column classes into class "numeric"
  tmp.l <- lapply(1:ncol(tmp), function(x) tmp[,x] <- as.numeric(tmp[,x]))
  #put data.frame back together and reinsert header names
  tmp <- as.data.frame(do.call(cbind, tmp.l))
  colnames(tmp) = tmp.colnames

  raw <- tmp %>%
    select('cycle.number', 'time.s', 'Ns', 'Ecell.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
    mutate(time.s = time.s - min(time.s))

  colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

  raw <- raw

  return(raw)

}

#' @export
#' @rdname Read0r
#' @details read .txt files from raw data directory (for Biologic VMP)
VMPraw <- function(dir, filename){

  #binding variables locally to function VMPraw()
  time.s <- NULL

  # read .txt file
  # little workaround required:
  # some computers export values with "," decimal, others with "." decimal sign
  tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
  #save column names for after the operation
  tmp.colnames <- colnames(tmp)
  #convert all column classes into class "numeric"
  tmp.l <- lapply(1:ncol(tmp), function(x) tmp[,x] <- as.numeric(tmp[,x]))
  #put data.frame back together and reinsert header names
  tmp <- as.data.frame(do.call(cbind, tmp.l))
  colnames(tmp) = tmp.colnames

  raw <- tmp %>%
    select('cycle.number', 'time.s', 'Ns', 'Ewe.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
    mutate(time.s = time.s - min(time.s))

  colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

  return(raw)

  }

#' @export
#' @rdname Read0r
#' @details read .xlsx files from raw data directory
ARBINrawXLSX <- function(dir, f.path){

    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package \"readxl\" must be installed to use this function.",
        call. = FALSE
      )
    }

    #f.dir <- paste0(dir, "/", filename, ".xlsx")

    # @param filename must end in .xls or xlsx
    l <- lapply(grep("Channel*", excel_sheets(f.path), value=TRUE),
                read_excel, path = f.path)
    l <- do.call(rbind,l)

    raw <- l %>%
      select('Cycle_Index', 'Test_Time(s)', 'Step_Index', 'Voltage(V)', 'Current(A)', 'Charge_Capacity(Ah)', 'Discharge_Capacity(Ah)')
    #mutate(Test_Time(s) = Test_Time(s) - min(Test_Time(s)))
    colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qch.Ah', 'Qdc.Ah')

    return(raw)
  }

#' @export
#' @rdname Read0r
#' @details read .accdb (MS Access) files from raw data directory
ARBINrawACCDB <- function(filename){

    if (!requireNamespace("RODBC", quietly = TRUE)) {
      stop(
        "Package \"RODBC\" must be installed to use this function.",
        call. = FALSE
      )
    }

    raw <- data.frame()
    print('importing accdb')
    #system.file() required???
    RODBC <- tryCatch(
                          expr = {
                                  con <- RODBC::odbcConnectAccess2007(filename)
                                  #sqlTables(con, tableType="TABLE")$TABLE_NAME
                                  raw <- RODBC::sqlFetch(con, "Channel_Normal_Table")
                                  RODBC::odbcCloseAll()
                          },
                          error = function(e) e,
                          warning = function(w) w
                  )

    if(inherits(RODBC, "simpeError")){

        print(paste0("Error: could not access odbcConnectAccess2007. Use .xlsx file instead."))
        raw <- NULL

    }else if(inherits(RODBC, "simpeWarning")){

        print(paste0("Error: could not access odbcConnectAccess2007. Use .xlsx file instead."))
        raw <- NULL

    }else{
        print('selecting columns')
        print(raw)
        raw <- raw%>%
          select('Cycle_Index', 'Test_Time', 'Step_Index', 'Voltage', 'Current', 'Charge_Capacity', 'Discharge_Capacity')

        colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qch.Ah', 'Qdc.Ah')
    }

    return(raw)
}
