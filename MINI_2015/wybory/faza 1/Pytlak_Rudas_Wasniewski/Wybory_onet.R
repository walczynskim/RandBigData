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
