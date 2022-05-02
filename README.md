
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bat2dat

<!-- badges: start -->
<!-- badges: end -->

Different Potentiostats produce different experimental output files.
Some of them are easy readable, as they are saves as .txt or .xlsx
files, while others need to be exported into a readable format prior to
further processing. This tools aims to convert various potentiostat
outputs into a uniform structure while extracting key parameters and
datasets for further graphical processing (e.g. in Origin). For this
tool to work metadata has to be provided.

## Installation

bat2dat is based on R programming language. To install R and the
functions of bat2dat on your computer follow these three steps:  
1) Install the **latest R version**. You’ll find the latest version on
the following website

> <https://www.r-project.org/>  
> <https://ftp.fau.de/cran/>

2)  install required Rpackages used within bat2dat. To do so copy the
    following lines into your R console. If the code is not run
    automatically press *Enter*

``` r
#utils: basic functions
install.packages('utils')   

#magrittr: piping operators
install.packages('magrittr')

#dplyr: modify data through filter, select and mutate
install.packages('dplyr')

#tidyr: modify data.frames
install.packages('tidyverse')

#zoo: for moving average on data
install.packages('zoo')

#readxl: open excel files
install.packages('readxl')

#ggplot2: plot data
install.packages('ggplot2')

#gridExtra: combine plots generated with ggplot2
install.packages('gridExtra')

#viridis: colour spectrum
install.packages('viridis')

#rmarkdown: generates html or pdf files / reports
install.packages('rmarkdown')

#optparse: interface for Linux subsystem
install.packages("optparse")

#RODBC: open microsoft access files
install.packages('RODBC')

#knitr: knitr package to generate reports
install.packages('knitr')

#kableExtra: additional package to knitr & Rmarkdown
install.packages('kableExtra')
```

A more user-friedly environment is provided by RStudio, which can be
downloaded as free desktop version from here: \>
<https://www.rstudio.com/products/rstudio/download/#download>  

Note: in RStudio R is executed by marking the code snippet you’d like to
run and press **ctrl + enter** (*Windows*) or **cmd + enter**
(*MacOS*).  

3)  You can install the development version of bat2dat via GitHub like
    so:  

``` r
devtools::install_github(fabje223/bat2dat)
```

To update to the latest version of *bat2dat* repeat step 3) once in a
while.

## Requirements

The main idea of bat2dat is to provide an easy way to convert data from
battery cyclers into a standardized format for further processing.  
bat2dat requires a curated data folder containing:  
- experimental data (.txt, .xlsx, … etc.) –\> the format depends on the
battery cycler used  
- a metadata file *‘meta.csv’* meta.csv requires the following
structure:  

| Identifier | sample.name |  instrument  |  cell.config   | AM.loading |
|:----------:|:-----------:|:------------:|:--------------:|:----------:|
|  Sample 1  | experiment1 | Biologic BCS | halfcell-anode |    1.00    |

### **Notes -1-: bat2dat specific *meta.csv* syntax** 

-   in case meta.csv is created manually:

> *Identifier*, *sample.name*, *instrument* and *cell.config* are text
> or character items and need to be put in quotation marks, e.g. “sample
> 1”  
> items in meta.csv are separated by a tab separator  

-   *instrument*:

> *Biologic BCS* refers to BCS instrument by Biologic (.txt format,
> exported from ECLab)  
> *Biologic VMP* refers to VMP/VSP instruments by Biologic (.txt format,
> exported from ECLab)  
> *Arbin* refers to Arbin battery cyclers (.res format or exported via
> Excel Plug-In as .xlsx)  

bat2dat has a modular approach and more cyclers can be added if
needed.  

-   *cell.config* has to be defined for some of the scripts in order to
    split up charge and discharge sequences correctly:

> *halfcell-anode* (cycling starts with a negative current)  
> *halfcell-cathode* & *fullcell* (the two are equivalent; cycling
> starts with a positive current)  
> *LiS* is an except, as it is cell-specific: sulfur batteries, though a
> cathode, are techniqually in a charged state and thus start with a
> negative current.  

-   *AM.loading*: the active material mass should be given in units of
    \[mg\]

### **Notes -2-: bat2dat specific *meta.csv* syntax** 

*meta.csv* can be created from digital lab notebooks.  
A POLiS and Kadi4Mat compatible notebook is provided in the examples
folder.  
The meta.csv file can be generated using the *CellLog-xlsx-to-csv.R*
script.  
Other notebook imports can be created upon request. **Note: this
function is currently under construction.**  

## Supported datasets

### Biologic Data

Biologic instruments save acquired data in a *.mpr* file (*EC-Lab raw
data binary file*), which is complex to read-in directly. Therefore, the
workaround herein is to **use export function of EC-Lab** (*Experiment
\> Export as Text…*).

<img src="man/figures/README-BiologicTXTExport.png" width="100%" />

The exported data table should look like this (may vary depending on the
exported variables):

``` r
data(exampleData)
knitr::kable(head(exampleData, 5), 
             align = 'c',
             caption = "Table: Structure of EC-Lab .txt files")
```

| cycle.number | Ns  | time.s | X.I..mA | Ecell.V  | Q.discharge.mA.h | Q.charge.mA.h | dq.mA.h | X.Q.Qo..mA.h | Capacity.mA.h | Energy.charge.W.h | Energy.discharge.W.h |  X  |
|:------------:|:---:|:------:|:-------:|:--------:|:----------------:|:-------------:|:-------:|:------------:|:-------------:|:-----------------:|:--------------------:|:---:|
|      0       |  0  |   0    |    0    | 2.153887 |        0         |       0       |    0    |      0       |       0       |         0         |          0           | NA  |
|      0       |  0  |  300   |    0    | 2.156328 |        0         |       0       |    0    |      0       |       0       |         0         |          0           | NA  |
|      0       |  0  |  600   |    0    | 2.158218 |        0         |       0       |    0    |      0       |       0       |         0         |          0           | NA  |
|      0       |  0  |  900   |    0    | 2.159084 |        0         |       0       |    0    |      0       |       0       |         0         |          0           | NA  |
|      0       |  0  |  1200  |    0    | 2.160541 |        0         |       0       |    0    |      0       |       0       |         0         |          0           | NA  |

Table: Structure of EC-Lab .txt files

The strange looking column names are a result of the import into R and
are renamed during the analysis. For the script to work, ***the
following parameters need to be exported***:

> cycle.number, time.s, Ns, Ecell.V, X.I..mA, Q.discharge.mA.h,
> Q.charge.mA.h

**Note**: Biologic BCS name the potential *Ecell.V*, whereas VMP/VSP/MPG
potentiostats call the same parameter *Ewe.V*. This is an important
distinction and the reason why *Biologic BCS* is a separate “instrument
parameter” in the *meta.csv* file.

### Arbin Data

Arbin data comes in 2 formats: it is saved by the instrument as a *.res*
file and can be converted via the Excel plug-in to a *.xlsx* result file
with several tabs for metadata. raw cycling data, (processed) Stats-Tab
for end-of-cycling-sequence information (e.g. cycle number
vs. capacity). Both types can be imported into R. Import of *.res* files
require the RODBC package (included in the package recommendations
above) and changing the file extension from .res to *.accdb* (Access
database file).

## How to use

**Experimental data and the metadata file need to be located in the same
folder on the local hard drive.**  
The analysis script is started by executing:

    #if library is not loaded already
    library(bat2dat)

    # generates a data report if htmlReport == TRUE
    # exports a .txt file if exportCap == TRUE
    report0r(   htmlReport = TRUE, 
                exportCap = TRUE)

During execution you will be asked for the directory of your
experimental data. Select the meta.csv file and execution will continue.
**It is important that meta.csv and raw data files are located in the
same folder**. Additional files do not interfere.

> the script offers two options:  
> - generate a html-report? yes(=TRUE)/no(=FALSE)  
> - export analysed data as .txt file? yes(=TRUE)/no(=FALSE)  
>
> the script won’t run the analysis if both values are set to FALSE.  
> once the script started, you will be asked to specify the location of
> your data.  

If you’d like to work with your data in R afterwards, call report0r()
like this:  

    samples <- report0r( htmlReport = TRUE, 
                         exportCap = TRUE)

samples will be a list of all samples contained in the metadata file
(*meta.csv*) and contains up to three different analysed objects:

> *capacity*: end of cycling squence (e.g. charge & discharge capacity
> or Coulombic efficiency vs. cycle number) –\> format: R data.frame  
> *VoltageProfiles*: section of raw data for selected cycles –\> format:
> list of data.frames  
> *CCCV*: if galvanostatic cycling was performed in a constant current
> constant potential technqiue (CC-CV), a CCCV step analysis can be
> performed, splitting the contributions of the CC and CV step –\>
> format: R data.frame  

Exported data will be saved as a subfolder *Rprocessed* of the raw data
folder (*path/to/data/Rprocessed*) on your local hard drive.

## Examples

This is a basic example which shows you how to solve a common problem:
blub

``` r
#library(bat2dat)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-BiologicTXTExport.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
