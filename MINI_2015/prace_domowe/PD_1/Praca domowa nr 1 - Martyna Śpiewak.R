### 50 FIFTY SHADES OF GREY VS. THE THEORY OF EVERYTHING

library(stringi)
library(rvest)
library(XML)

basic<-function(html){
   stopifnot(is.character(html), length(html)==1)
   
   # obsada
   
   table <- readHTMLTable(html, stringsAsFactors = FALSE)
   cast<-as.data.frame(table[2])[,-c(1,3)]
   names(cast)<-c("Aktor/Aktorka", "Postaæ")
   
   html2 <- html(html)
   tmp<-html_text(html_nodes(html2, ".star-box-rating-label , 
                  .star-box-details, .star-box-giga-star, 
                  #titleDetails :nth-child(11), :nth-child(11) .inline, 
                  #titleDetails :nth-child(10), #titleCast :nth-child(2) div"))
   
   # ocena
   score<-as.numeric(tmp[1])
   
   # bud¿et
   
   budget<-stri_trim_both(unlist(stri_extract_all_regex(tmp[6], 
                                                        "(?<=Budget: ).+")))

   # recenzje
   tmp1<-stri_replace_all_regex(tmp[3], "\\n", "")
   
   reviews<-as.numeric(unlist(stri_extract_all_regex(tmp1, 
                                                     "(?<=Reviews: )[0-9]+")))
   critics<-as.numeric(unlist(stri_extract_all_regex(tmp1, "[0-9]+ (?=critic)")))
   
   return(list(ocena=score, budzet=budget, recenzje=reviews, 
               krytyka=critics, obsada=cast))
}

### PODSTAWOWE INFROMACJE:
# 1. 50 FIFTY SHADES OF GREY

grey <- "http://www.imdb.com/title/tt2322441/?ref_=nv_sr_1"
basic(grey)

# 2. THE THEORY OF EVERYTHING

ttoe<-"http://www.imdb.com/title/tt2980516/?ref_=nv_sr_1"
basic(ttoe)


### STATYSTYKI

piefun2<-function(html){
   stopifnot(is.character(html), length(html)==1)
   
   tmp1<-readHTMLTable(html)
   
   pie1<-as.data.frame(tmp1[[1]])
   proc<-unlist(stri_extract_all_regex(pie1[,2], "[0-9]+.[0-9]+%"))
   pie(as.numeric(as.vector(pie1[,1])), col=rainbow(10), 
       labels = stri_paste(c(seq(10, 1, by=-1)), proc, sep = "; "))
}

### WYKRESY PIE: "Jak procentowo rozkladaja sie oceny?"

html1<-"http://www.imdb.com/title/tt2322441/ratings?ref_=tt_ov_rt"
html2<-"http://www.imdb.com/title/tt2980516/ratings?ref_=tt_ov_rt"

# 1. 50 shades of grey
piefun2(html1)
# 2. THE THEORY OF EVERYTHING
piefun2(html2)

barplotfun2<-function(html){
   stopifnot(is.character(html), length(html)==1)
   
   tmp2<-readHTMLTable(html)
   
   bplot2<-as.data.frame(tmp2[2])
   
   par(mfrow=c(2,1))
   
   w <- bplot2[c(5,8,11,14),c(1,3)]
   w[,2] <- as.numeric(unlist(stri_extract_all_regex(w[,2],"[0-9.]+")))
   w[,1]<-c("<18", "18-29", "30-44", "45+")
   colnames(w) <- c("Wiek","Srednia Ocen")
   w
   
   barplot(height = w[,2], names.arg = w[,1], 
           main="Srednia ocen oddanych przez kobiety")

   
   m <- bplot2[c(4,7,10,13),c(1,3)]
   m[,2] <- as.numeric(unlist(stri_extract_all_regex(m[,2],"[0-9.]+")))
   m[,1]<-c("<18", "18-29", "30-44", "45+")
   colnames(m) <- c("Wiek","Srednia Ocen")
   
   barplot(height = m[,2], names.arg = m[,1], 
           main="Srednia ocen oddanych przez mezczyzn")
   
   par(mfrow=c(1,1))
   
}

# 1. 50 shades of grey
barplotfun2(html1)

# 2. THE THEORY OF EVERYTHING
barplotfun2(html2)

