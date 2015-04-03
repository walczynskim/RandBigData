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


library(stringi)
library(rvest)

#przykladowy link do artykulu:
# link1<-"http://wiadomosci.wp.pl/kat,140394,title,Kampania-Magdaleny-Ogorek-z-mniejszym-budzetem-Na-to-pytanie-nie-odpowie,wid,17345828,wiadomosc.html"
# link<-html(link1)

wp_artykul_info<-function(link,id){
   link<-html(link)
   #data
   data<-html_text(html_nodes(link,"time")[[1]],encoding="UTF-8")
   datas<-unlist(stri_extract_all_regex(data,"[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}"))
   datas<-stri_replace_all_regex(datas,"\\.","-")
   times<-unlist(stri_extract_all_regex(data,"[0-9]{2}:[0-9]{2}"))
   data<-stri_paste(datas,times,sep=" ")
   
   #zrodlo
   zrodlo<-"wiadomosci.wp.pl"
   #tytul
   tytul<-html_text(html_nodes(link,".fullWidth .h1")[[1]],encoding="UTF-8")
   # tagi
   tagi<-html_text(html_nodes(link,".fullWidth .ST-BX-Tagi-dol a"),encoding="UTF-8")
   tagi<-stri_paste(tagi,collapse = "-")
   
   
   #tresc
   tresc1<-html_nodes(link,".lead")[[1]]%>%html_text(encoding="UTF-8")
   tresc<-html_nodes(link,"section.artCnt:nth-child(1) > main:nth-child(3) > div:nth-child(4)")   
   tresc<-stri_replace_all_regex(html_text(tresc,encoding="UTF-8"),"(if(\\n|.)+\\}|\\n|\\r|\\t)","")
   tresc<-stri_paste(tresc1,tresc)
   
   #liczba komentarzy
   liczba_kom<-html_text(html_nodes(link,"#stgOpinie .opStron+ .opNNav .opAllCom"))
   if (length(liczba_kom)==0){
      liczba_kom<-html_text(html_nodes(link,"#opOpinie+ .opNNav .opAllCom"))
   }
   liczba_kom<-unlist(stri_extract_all_regex(liczba_kom,"[0-9]+"))
   
   data.frame("id"=id,"zrodlo"=zrodlo,"data"=data,"tytul"=tytul,"tresc"=tresc,
              "tagi"=tagi,"liczba komentarzy"=liczba_kom)
   
}

wp_artykul_info("http://wiadomosci.wp.pl/kat,140394,title,Leszek-Miller-wtedy-wraz-z-kierownictwem-partii-podam-sie-do-dymisji,wid,17342615,wiadomosc.html?ticaid=11482e",1)




l<-"http://wiadomosci.wp.pl/kat,140394,title,Wybory-prezydenckie-w-2015-r,raport.html"
p<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\riduzedane"
cz<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\\\czas\\czaswp.txt"
wp_info<-function(link,path1,pathczas){
   czass<-readLines(pathczas)
   czas1<-strptime(czass,"%Y-%m-%d %H:%M")
   czas1
   html_link<-html(link)
   artykuly<-html_nodes(html_link,".kontener h2 a")
   #linki do artykulow:
   atrykuly_linki<-stri_paste("http://wiadomosci.wp.pl",html_attr(artykuly,"href"))
   atrykuly_linki<-unique(atrykuly_linki)
   n<-length(atrykuly_linki)
   df<-data.frame()
   daty<-html_nodes(html_link,".stampZrodloData")%>%html_text()
   godziny<-html_nodes(html_link,".stampZrodloCzas .stampZrodloGodzina")%>%html_text()
   godziny<-stri_extract_all_regex(godziny,"[0-9]{2}:[0-9]{2}")%>%unlist()
   czasy_art<-stri_paste(daty,godziny,sep=" " )
   for(i in 1:n){
      if(unclass(czas1-strptime(czasy_art[i],"%Y-%m-%d %H:%M"))<=0)
      {
         df<-rbind(df,wp_artykul_info(atrykuly_linki[i],i))
      }
   }
   odroznik<-Sys.time()#ta czesc nazwy pliku odroznia go od pozostalych
   odroznik1<-strftime(odroznik,"%Y-%m-%d %H:%M:%S %Z")
   writeLines(odroznik1,pathczas)
   path<-stri_paste(path1,"\\a",coll="")
   odroznik<-stri_replace_all_regex(odroznik,"\\.","")
   path<-stri_paste(path,odroznik,coll="")
   path<-stri_paste(path,".txt",coll="")
   if(nrow(df)!=0)
   {
      write.table(df,path)  
   }
   
}

wp_info(l,p,cz)

## ------------- ONET-------------------------
##"http://wiadomosci.onet.pl/wybory-prezydenckie-2015"
library(stringi)
library(rvest)


#przykladowy artykol
link<-"http://wiadomosci.onet.pl/kraj/andrzej-duda-w-polsce-powinny-powstac-polsko-amerykanskie-bazy-wojskowe/vw98hk"

onet_artykul_info<-function(link,id){
   link<-html(link)
   #zrodlo
   zrodlo<-"wiadomosci.onet.pl"
   #tytul
   tytul<-html_nodes(link,"#mainTitle > h1")[[1]] %>% html_text(encoding="UTF-8")
   tytul<-stri_trim(tytul)
   # tagi
   tagi<-html_nodes(link,"#relatedTopics > span")[-1]%>%
      html_text(encoding="UTF-8")
   tagi<-stri_trim(tagi) %>% stri_paste(collapse = ",")%>% 
      stri_replace_all_regex(",\\.+","")
   
   
   #data
   data<-html_nodes(link,"#articleHeading > meta") %>% html_attr("content")
   data<-stri_replace_all_regex(data,"\\+[0-9]{4}","")
   
   #tresc
   tresc1<-html_nodes(link,"#lead > p") %>% html_text(encoding="UTF-8")
   tresc<-html_nodes(link,"#detail > p") %>% html_text()
   if(length(tresc)==0)
   {
      tresci<-html_nodes(link,"#detail > div.interview > p") %>% html_text()
      tresc<-c()
      for (i in 1:length(tresci))
      {
         tresc<-paste(tresc,tresci[i])
      }
      
      tresc<-paste(tresc1, tresc)
   }
   else
   {
      tresc<-stri_paste(tresc1,tresc,collapse = " ")
   }
   
   
   #liczba komentarzy
   liczba_kom<-html_nodes(link,"#socialTop > div > div.box3_forum > div.forumCommentButton > div.dymek > div.dymek4")
   liczba_kom<-html_text(liczba_kom)
   data.frame("id"=id,"zrodlo"=zrodlo,"data"=data,"tytul"=tytul,"tresc"=tresc,
              "tagi"=tagi,"liczba komentarzy"=liczba_kom)
   
   
}
onet_artykul_info(link,1)

l<-"http://wiadomosci.onet.pl/wybory-prezydenckie-2015"
p<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\riduzedane"
cz<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\czas\\czasonet.txt"
onet_info<-function(link,path1,pathczas){
   #czas ostatniego pobierania danych
   czass<-readLines(pathczas)
   czas1<-strptime(czass,"%Y-%m-%d %H:%M")
   czas1
   html_link<-html(link)
   artykuly<-html_nodes(html_link,"#staticStreamContent > div > a")
   #linki do artykulow:
   atrykuly_linki<-html_attr(artykuly,"href")
   atrykuly_linki<-unique(atrykuly_linki)
   n<-length(atrykuly_linki)
   df<-data.frame()
   for(i in 1:n){
      df1<-onet_artykul_info(atrykuly_linki[i],i)
      if(unclass(czas1-strptime(df1$data,"%Y-%m-%d %H:%M"))<0)
      {
         df<-rbind(df,df1)
      }
   }
   odroznik<-Sys.time()#ta czesc nazwy pliku odroznia go od pozostalych
   odroznik1<-strftime(odroznik,"%Y-%m-%d %H:%M:%S %Z")
   writeLines(odroznik1,pathczas)
   
   path<-stri_paste(path1,"\\c",coll="")
   odroznik<-stri_replace_all_regex(odroznik,"\\.","")
   path<-stri_paste(path,odroznik,coll="")
   path<-stri_paste(path,".txt",coll="")
   if(nrow(df)!=0)
   {
      write.table(df,path)  
   }
}



onet_info(l,p,cz)

