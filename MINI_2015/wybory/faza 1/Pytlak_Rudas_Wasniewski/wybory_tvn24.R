###------------tvn24.pl------------

#http://www.tvn24.pl/wybory-prezydenckie-2015,117,m
library(stringi)
library(rvest)


#news524345 > div.textRight > h1 > a   html_link<-html(link)
html_nodes(html("http://www.tvn24.pl/wybory-prezydenckie-2015,117,m"),
                     "div.textRight > h1 > a")%>%html_attr("href")


tvn24_artykul_info<-function(link,id){
   link<-html(link)
   #zrodlo
   zrodlo<-"tvn24.pl"
   #tytul
   tytul<-html_nodes(link,"article > h1 > span") %>% 
                                                html_text(encoding="UTF-8")
   if(length(tytul)==0)
   {
      tytul<-html_nodes(link,"div.mainContainer > h1") %>% 
         html_text(encoding="UTF-8")
   }
   tytul<-stri_trim(tytul)
   # tagi
   tagi<-html_nodes(link,"div.relatedTopic > ul > li > a")%>%html_text(encoding="UTF-8")
   if(length(tagi)>0){
   tagi<-stri_trim(tagi) %>% stri_paste(collapse = ",")
   }else tagi<-""
   #data
   data<-html_nodes(link,"div.articleDateContainer.borderGreyBottom > time") %>%
                                       html_attr("datetime")
   #tresc
   tresc1<-html_nodes(link," article > h2.size18.mt10.mb15") %>% html_text(encoding="UTF-8")%>%stri_trim()
   tresc2<-html_nodes(link,"div > div.articleDetailHolder > article > p") %>% html_text()
   tresc2<-tresc2[-length(tresc2)]%>%stri_trim()%>%stri_replace_all_regex("(if(\\n|.)+\\}|\\n|\\r|\\t)"," ")
   tresc<-stri_paste(tresc1,tresc2,collapse = " ")
   
   #liczba komentarzy
   liczba_kom<-html_nodes(link,"#forum > div.headerBgGrey.mb15 > h1 > span")
   liczba_kom<-html_text(liczba_kom)%>%stri_extract_all_regex("[0-9]+")%>%unlist()
   liczba_kom
   data.frame("id"=id,"zrodlo"=zrodlo,"data"=data,"tytul"=tytul,"tresc"=tresc,
              "tagi"=tagi,"liczba komentarzy"=liczba_kom)

}



tvn24_artykul_info("http://www.tvn24.pl/wiadomosci-z-kraju,3/bronislaw-komorowski-odpowiada-na-muzeum-zgody,524168.html",3)


l<-"http://www.tvn24.pl/wybory-prezydenckie-2015,117,m"
p<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\riduzedane"
cz<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\czas\\czastvn24.txt"
tvn24_info<-function(link,path1,pathczas){
   #czas ostatniego pobierania danych
   czass<-readLines(pathczas)
   czas1<-strptime(czass,"%Y-%m-%d %H:%M")
   czas1
   #linki do artykulow:
   html_link<-html(link)
   artykuly_linki1<-html_nodes(html_link,"div.textRight > h1 > a")%>%html_attr("href")
   artykuly_linki2<-html_nodes(html_link,"div.textLeft > h1 > a")%>%html_attr("href")
   artykuly_linki3<-html_nodes(html_link,"article.singleArtMain > div > h1 > a")%>%html_attr("href")
   artykuly_linki<-stri_paste("http://www.tvn24.pl",
                              c(artykuly_linki1,artykuly_linki2,artykuly_linki3))
   artykuly_linki<-unique(artykuly_linki)
   n<-length(artykuly_linki)

      df<-data.frame()
      for(i in 1:n){
         df1<-tvn24_artykul_info(artykuly_linki[i],i)
         if(unclass(czas1-strptime(df1$data,"%Y-%m-%d %H:%M"))<0)
         {
            df<-rbind(df,df1)
         }
      }
      odroznik<-Sys.time()#ta czesc nazwy pliku odroznia go od pozostalych
      odroznik1<-strftime(odroznik,"%Y-%m-%d %H:%M:%S %Z")
      writeLines(odroznik1,pathczas)
      path<-stri_paste(path1,"\\b",coll="")
      odroznik<-stri_replace_all_regex(odroznik,"\\.","")
      path<-stri_paste(path,odroznik,coll="")
      path<-stri_paste(path,".txt",coll="")
      if(nrow(df)!=0)
      {
         write.table(df,path)  
      } 

   
}

tvn24_info(l,p,cz)


