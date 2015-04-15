
df1 <- read.table("F:\\doc\\R i Big Data\\Rproject1\\wybory\\2015-03-25.csv", 
                  header = TRUE, sep=";", stringsAsFactors = FALSE)
df2 <- read.table("F:\\doc\\R i Big Data\\Rproject1\\wybory\\dolaczyc\\2015-03-25.csv", 
                  header = TRUE, sep=";", dec=",", stringsAsFactors = FALSE)

df1 <- rbind(df1,df2)

write.table(df1,"C:\\Users\\grabarze\\Desktop\\2015-03-25.csv", sep=";",dec=",", 
            append=TRUE, col.names = TRUE, row.names = FALSE, quote = FALSE)

#############################
   aa <- iconv(readLines("E:\\doc\\R i Big Data\\Rproject1\\wybory\\tweety_kandydatow_2015-03-18.csv"), 
               from="windows-1250", to="UTF-8")
#############################

df11 <- readLines("F:\\doc\\R i Big Data\\Rproject1\\wybory\\2015-03-21.csv", encoding = "UTF-8")
writeLines(df11,"C:\\Users\\grabarze\\Desktop\\2015-03-21.csv")


