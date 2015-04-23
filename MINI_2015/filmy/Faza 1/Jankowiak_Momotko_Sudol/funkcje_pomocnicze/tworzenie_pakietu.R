library(devtools)
library(roxygen2)
library(testthat)
library(formatR)

#scieżka ustawiona na folder Filmy

#setwd()
session_info()

setwd("FilmyJMS")

create("FilmyJMS")

build()

install()

document()

tidy_dir("R")

#sprawdzamy
library(FilmyJMS)
