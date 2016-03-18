setwd("./dane/exhibits-logs-2011")

library(stringi)

path <- getwd()

x <- list.files(path, recursive = TRUE, pattern = "cnk.*\\.log")
ile <- rep(0, length(x))

for(j in 1:length(x))
{
  f <- file(file.path(path, x[j]))
  
  open(f, "r")
  
  y <- readLines(f)
  if(length(y > 0))
  {
    id <- stri_extract_first_regex(y, "(?<=Added visitor )[0-9]+")
  }
  
  ile[j] <- sum(!is.na(unique(id)))
  
  close(f)
}


nazwa <- stri_split_regex(x, "/")
names(ile) <- sapply(nazwa, function(x){x[3]})

lapply(split(ile, names(ile)), sum)

# $cnk02a
# [1] 65362
# 
# $cnk02b
# [1] 59221
# 
# $cnk03
# [1] 38213
# 
# $cnk04
# [1] 0
# 
# $cnk05
# [1] 55672
# 
# $cnk06
# [1] 36979
# 
# $cnk07
# [1] 55745
# 
# $cnk09
# [1] 61252
# 
# $cnk10
# [1] 62260
# 
# $cnk100
# [1] 23228
# 
# $cnk11
# [1] 56912
# 
# $cnk12
# [1] 66579
# 
# $cnk13
# [1] 34959
# 
# $cnk15
# [1] 0
# 
# $cnk16
# [1] 82738
# 
# $cnk17
# [1] 59095
# 
# $cnk18
# [1] 67624
# 
# $cnk19a
# [1] 131096
# 
# $cnk19b
# [1] 81591
# 
# $cnk20
# [1] 54129
# 
# $cnk21
# [1] 48831
# 
# $cnk22
# [1] 57763
# 
# $cnk23
# [1] 40876
# 
# $cnk24
# [1] 58925
# 
# $cnk25
# [1] 35753
# 
# $cnk26
# [1] 29769
# 
# $cnk27
# [1] 0
# 
# $cnk28
# [1] 0
# 
# $cnk29a
# [1] 20191
# 
# $cnk30
# [1] 0
# 
# $cnk31a
# [1] 0
# 
# $cnk31b
# [1] 0
# 
# $cnk31c
# [1] 0
# 
# $cnk31d
# [1] 0
# 
# $cnk32
# [1] 31697
# 
# $cnk34
# [1] 0
# 
# $cnk36
# [1] 24
# 
# $cnk37
# [1] 37203
# 
# $cnk38
# [1] 26596
# 
# $cnk39
# [1] 44387
# 
# $cnk40
# [1] 45257
# 
# $cnk41
# [1] 0
# 
# $cnk42a
# [1] 45403
# 
# $cnk42b
# [1] 0
# 
# $cnk43
# [1] 27438
# 
# $cnk44
# [1] 18969
# 
# $cnk45
# [1] 28523
# 
# $cnk46a
# [1] 29468
# 
# $cnk46b
# [1] 35337
# 
# $cnk47
# [1] 34175
# 
# $cnk48a
# [1] 36943
# 
# $cnk48b
# [1] 0
# 
# $cnk48c
# [1] 0
# 
# $cnk48d
# [1] 0
# 
# $cnk48e
# [1] 0
# 
# $cnk49
# [1] 39191
# 
# $cnk52
# [1] 0
# 
# $cnk53
# [1] 0
# 
# $cnk54
# [1] 48759
# 
# $cnk55
# [1] 30112
# 
# $cnk56
# [1] 39007
# 
# $cnk57
# [1] 26147
# 
# $cnk58a
# [1] 0
# 
# $cnk58b
# [1] 24232
# 
# $cnk59
# [1] 21951
# 
# $cnk60
# [1] 32639
# 
# $cnk61
# [1] 44644
# 
# $cnk62
# [1] 43740
# 
# $cnk63a
# [1] 4533
# 
# $cnk65
# [1] 0
# 
# $cnk66
# [1] 55144
# 
# $cnk67
# [1] 47807
# 
# $cnk68
# [1] 0
# 
# $cnk69
# [1] 47991
# 
# $cnk70a
# [1] 0
# 
# $cnk70b
# [1] 0
# 
# $cnk70c
# [1] 0
# 
# $cnk71
# [1] 29581
# 
# $cnk72
# [1] 41136
# 
# $cnk73
# [1] 29217
# 
# $cnk74a
# [1] 0
# 
# $cnk75
# [1] 35076
# 
# $cnk76
# [1] 0
# 
# $cnk78a
# [1] 55746
# 
# $cnk78b
# [1] 0
# 
# $cnk79
# [1] 43202