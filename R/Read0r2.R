BCSraw <- function(dir, meta){

  tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
  raw <- tmp %>%
    select('cycle.number', 'time.s', 'Ns', 'Ecell.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
    mutate(time.s = time.s - min(time.s))
  colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

  return(raw)

}

VMPraw <- function(dir, meta){

  tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
  raw <- tmp %>%
    select('cycle.number', 'time.s', 'Ns', 'Ewe.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
    mutate(time.s = time.s - min(time.s))
  colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

  return(raw)

  }

ARBINraw <- function(dir, meta){

  file <- list.files(pattern = filename)

  if(endsWith(file, ".xlsx")){

    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package \"readxl\" must be installed to use this function.",
        call. = FALSE
      )
    }

    fname <- paste0(dir, "/", file)

    # @param filename must end in .xls or xlsx
    l <- lapply(grep("Channel*", excel_sheets(fname), value=TRUE),
                read_excel, path = fname)
    l <- do.call(rbind,l)

    raw <- l %>%
      select('Cycle_Index', 'Test_Time(s)', 'Step_Index', 'Voltage(V)', 'Current(A)', 'Charge_Capacity(Ah)', 'Discharge_Capacity(Ah)')
    #mutate(Test_Time(s) = Test_Time(s) - min(Test_Time(s)))
    colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qdc.Ah', 'Qch.Ah')

  }else if(endsWith(file, ".accdb")){

    if (!requireNamespace("RODBC", quietly = TRUE)) {
      stop(
        "Package \"RODBC\" must be installed to use this function.",
        call. = FALSE
      )
    }

    fname <- paste0(dir, "/", file)

    con <- odbcConnectAccess2007(fname)
    #sqlTables(con, tableType="TABLE")$TABLE_NAME
    raw <- sqlFetch(con, "Channel_Normal_Table")
    odbcCloseAll()

    raw <- raw %>%
      select('Cycle_Index', 'Test_Time', 'Step_Index', 'Voltage', 'Current', 'Charge_Capacity', 'Discharge_Capacity')

    colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qch.Ah', 'Qdc.Ah')
  }
}
