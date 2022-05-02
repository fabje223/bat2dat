usethis::use_tidy_description()
usethis::use_build_ignore(c("myfolder", "file"))

#load files in data folder
usethis::use_data('filename')
#call objects from data folder with
data('filename')

devtools::load_all()

