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

