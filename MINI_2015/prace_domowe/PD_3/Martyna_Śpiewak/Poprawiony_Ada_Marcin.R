require(stringi)
require(XML)
require(rvest)

  portale <- data.frame(adres = c("http://www.tvn24.pl", "http://www.tvn24.pl/wybory-prezydenckie-2015,117,m",
                                'http://www.wp.pl', 'http://www.wp.pl', "http://www.newsweek.pl",
                                "http://www.onet.pl/", 'http://www.wiadomosci.onet.pl/wybory-prezydenckie/xcnpc',
                                "http://www.gazeta.pl"),
                      nodes1 = c('a', 'a', 'a', 'a', 'a', '.title , .boxContent a , .firstItem',
                                 '.datePublished+ a .itemTitle', 'a'),
                      nodes2 = c('p , .black', 'p , .black', '.artCont', '.artCont', 'p', 
                                 'p , .black', 'p , .black', '#artykul , #gazeta_article_lead'),
                      stringsAsFactors=FALSE)

  slownik <- readLines(stri_paste(getwd(), 'slownik.txt', sep="/"), encoding = "UTF-8")
  
  source(paste(getwd(), "portale_informacyjne.R", sep="/"))

  for(i in 1:nrow(portale)){
     portale_informacyne(portale$adres[i], portale$nodes1[i], portale$nodes2[i])
  }
