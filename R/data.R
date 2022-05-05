#' Example data for galvanostatic cell cycling.
#'
#' A typical dataset for a galvanostatic cycling experiments of batteries.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{cycle.number}{cycle number}
#'   \item{Ns}{sequence counter}
#'   \item{time.s}{experiment time}
#'   \item{X.I..mA}{applied current}
#'   \item{Ecell.V}{cell potential}
#'   \item{Q.discharge.mA.h}{negative charge passed}
#'   \item{Q.charge.mA.h}{positive charge passed}
#'   \item{dq.mAh}{charge increments}
#'   \item{X.Q.Q0..mA.h}{Difference between Q and Q0}
#'   \item{Capacity.mA.h}{capacity}
#'   \item{Energy.charge.W.h}{energy on charge sequences}
#'   \item{Energy.discharge.W.h}{energy on discharge sequences}
#'   \item{X}{empty column}
#' }
#' @source galvanostatic cycling experiment of a graphite-potassium half cell measured on a Biologic BCS instrument
"exampleData"

#' Example meta data file of a battery cell.
#'
#' Specifics of meta data file.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{Identifier}{identifier of experiment}
#'   \item{sample name}{name of the file *without* file extension}
#'   \item{instrument}{type of potentiostat used, with currently three options:
#'   *"Biologic BCS", "Biologic VMP"* (= also for MPG and VSP instruments), *"Arbin"*}
#'   \item{cell config}{cell setup with four options:
#'   *"halfcell-cathode", "fullcell", "halfcell-anode", "LiS"*;
#'   **Note that spelling need to be exact.**}
#'   \item{AM loading}{absolut active material content of working electrode}
#' }
#' @source galvanostatic cycling experiment of a graphite-potassium half cell measured on a Biologic BCS instrument
"exampleMeta"
