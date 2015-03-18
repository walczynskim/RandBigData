d <- read.csv("C:/Users/Marcin/Desktop/Analizy/Grape/FB.csv", header=TRUE, sep=";")
names(d)
t1 <- d[, "text"]

head(t1)

t2 <- unique(t1)

t2[1:100]


t11 <- as.character(t1)
rle(t11)$values[which(rle(t11)$lengths > 1)]


which(t2 == "brak tekstu")

t2[11570:11582]
write.csv(t2, "C:/Users/Marcin/Desktop/Analizy/Grape/tekst.csv")
write.table(t2, "C:/Users/Marcin/Desktop/Analizy/Grape/tekst.txt", sep="\t", quote=FALSE)

t2
require(stringi)
?stri_flatten
g <- stri_flatten(t2, collapse= "\n ")

write.table(g,"C:/Users/Marcin/Desktop/Analizy/Grape/jeden.txt", sep="\t", quote=FALSE )
?write.table
