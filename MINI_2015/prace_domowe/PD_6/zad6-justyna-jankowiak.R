
library(devtools)
library(roxygen2)
library(testthat)
library(formatR)

session_info()

setwd("RandBigData/MINI_2015/materialy/pakiety/subtitler")

build()

install()

document()

test()

tidy_dir("R")

