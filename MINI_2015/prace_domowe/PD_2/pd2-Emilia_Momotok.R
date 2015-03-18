library(stringi)
library(tm)
library(wordcloud)
library(streamR)
require(RColorBrewer)
library(colorRamps)
getwd()

##setwd("D:\\Moje dokumenty\\Emilka\\SMAD\\RAndBigData\\pd2")

#tweety odnosnie tennisa
parsedTwees <- parseTweets("TENNIS.json", simplify = FALSE, verbose = TRUE)

tweety<-parsedTwees[,1]

#zostawiamy tylko slowa, spacje, liczby i znaki interpunkcyjne

modified<-stri_extract_all_regex(tweety,"((\\w)|(\\s)|[:punct:])+")
modified<-unlist(lapply(modified,stri_flatten," "))

#teraz pozbywamy sie linkow
modified<-stri_replace_all_regex(modified,"http((\\w)|([:punct:]))+","")

#teraz pozbywamy sie adresow e-mail oraz nickow
modified<-stri_replace_all_regex(modified,"@(\\w)+(?=((\\s)|([:punct:])))","")
modified<-stri_replace_all_regex(modified,"((\\w)|([:punct:]))+(@|[0-9])((\\w)|([:punct:]))+","")

#wydobywamy slwa
slowa<-unlist(stri_extract_all_words(modified))
slowa<-stri_trans_tolower(slowa)
slowa<-slowa[!(slowa%in%stopwords("English"))]
irrelevant<-c('u','i',"ain't","i'm","a","im","you","he" ,"about", "above", "above", "across", "after",
              "afterwards", "again", "against", "all", "almost", "alone",
              "along", "already", "also","although","always","am","among",
              "amongst", "amoungst", "amount",  "an", "and", "another", "any",
              "anyhow","anyone","anything","anyway", "anywhere", "are", "around",
              "as",  "at", "back","be","became", "because","become","becomes",
              "becoming", "been", "before", "beforehand", "behind", "being",
              "below", "beside", "besides", "between", "beyond", "bill", "both",
              "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con",
              "could", "couldnt", "cry", "de", "describe", "detail", "do",
              "done", "down", "due", "during", "each", "eg", "eight", "either",
              "eleven","else", "elsewhere", "empty", "enough", "etc", "even",
              "ever", "every", "everyone", "everything", "everywhere", "except", 
              "few", "fifteen", "fify", "fill", "find", "fire", "first", "five",
              "for", "former", "formerly", "forty", "found", "four", "from",
              "front", "full", "further", "get", "give", "go", "had", "has", 
              "hasnt", "have", "he", "hence", "her", "here", "hereafter",
              "hereby", "herein", "hereupon", "hers", "herself", "him", 
              "himself", "his", "how", "however", "hundred", "ie", "if", "in",
              "inc", "indeed", "interest", "into", "is", "it", "its", "itself",
              "keep", "last", "latter", "latterly", "least", "less", "ltd",
              "made", "many", "may", "me", "meanwhile", "might", "mill", "mine",
              "more", "moreover", "most", "mostly", "move", "much", "must", "my",
              "myself", "name", "namely", "neither", "never", "nevertheless", 
              "next", "nine", "no", "nobody", "none", "noone", "nor", "not", 
              "nothing", "now", "nowhere", "of", "off", "often", "on", "once", 
              "one", "only", "onto", "or", "other", "others", "otherwise", "our",
              "ours", "ourselves", "out", "over", "own","part", "per", "perhaps",
              "please", "put", "rather", "re", "same", "see", "seem", "seemed",
              "seeming", "seems", "serious", "several", "she", "should", "show",
              "side", "since", "sincere", "six", "sixty", "so", "some", 
              "somehow", "someone", "something", "sometime", "sometimes",
              "somewhere", "still", "such", "system", "take", "ten", "than",
              "that", "the", "their", "them", "themselves", "then", "thence",
              "there", "thereafter", "thereby", "therefore", "therein",
              "thereupon", "these", "they", "thickv", "thin", "third", "this",
              "those", "though", "three", "through", "throughout", "thru",
              "thus", "to", "together", "too", "top", "toward", "towards",
              "twelve", "twenty", "two", "un", "under", "until", "up", "upon",
              "us", "very", "via", "was", "we", "well", "were", "what",
              "whatever", "when", "whence", "whenever", "where", "whereafter",
              "whereas", "whereby", "wherein", "whereupon", "wherever",
              "whether", "which", "while", "whither", "who", "whoever", "whole",
              "whom", "whose", "why", "will", "with", "within", "without",
              "would", "yet", "you", "your", "yours", "yourself", "yourselves",
              "the","amp")

slowa<-slowa[unlist(lapply(slowa,nchar))>2]

slowa<-slowa[!(slowa%in%irrelevant)]
ile<-sort(table(slowa),decreasing = TRUE)

ramka<-data.frame(slowo=names(ile),czestosc=ile,row.names=NULL)
kolory <- brewer.pal(8,"Dark2")
wordcloud(ramka[,1],ramka[,2], scale=c(4,0.1),min.freq=3,
          max.words=100, random.order=FALSE, rot.per=.20, colors=kolory)
plot.new()
plot.window(c(-1,1),c(-1,1))
x<-barplot(ile[1:10],col=c("lightcoral"),width=0.5,ylim=c(0,300),main="Czêstoœæ wystêpowania s³ów",cex.names=0.85)
text(x, ile[1:10], labels = format(ile[1:10], 2),
     pos = 3, cex = .75)
