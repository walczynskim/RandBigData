library(rvest)
library(stringi)
gazetapl_artykul_info<-function(link,id)
{
   adresartykulu<-html(link)
   d<-html_nodes(adresartykulu,"#gazeta_article_date")
   data<-html_text(d)
   datas<-unlist(stri_extract_all_regex(data,"[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}"))
   datas<-stri_replace_all_regex(datas,"\\.","-")
   times<-unlist(stri_extract_all_regex(data,"[0-9]{2}:[0-9]{2}"))
   czas<-paste(datas,times)
   tyt<-html_nodes(adresartykulu,"h1")
   tytul<-html_text(tyt,encoding="UTF-8")
   zrodlo<-"gazeta.pl"
   tre<-html_nodes(adresartykulu,"#artykul , #gazeta_article_lead")
   tre<-html_text(tre,encoding="UTF-8")
   tre<-paste(tre[1],tre[2])
   tresc<-tre
   tags<-html_nodes(adresartykulu,"#gazeta_article_tags ul")
   tag<-html_text(tags,encoding="UTF-8")
   
   
   
   licz<-html_nodes(adresartykulu,".head a")
   licz<-html_text(licz)
   if(length(licz)==0)
   {
      licz<-licz<-html_nodes(adresartykulu,".head")
      licz<-html_text(licz)
   }
   liczbakom<-as.numeric(stri_extract_all_regex(licz,"[0-9]{1,10}"))
   data.frame("id"=id,"zrodlo"=zrodlo,"data"=czas,"tytul"=tytul,"tresc"=tresc,
             "tagi"=tag,"liczba komentarzy"=liczbakom)
}
gazetapl_artykul_info("http://wiadomosci.gazeta.pl/wiadomosci/1,114871,17575348,Nowy_sondaz_CBOS_u__Dwucyfrowy_spadek_notowan_Komorowskiego.html",1)
l<-"http://wiadomosci.gazeta.pl/wiadomosci/0,114916.html?tag=wybory+prezydenckie+2015"
p<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\riduzedane"
cz<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\czas\\czas.txt"
gazetapl_info<-function(link,path1,pathczas)
{
   czass<-readLines(pathczas)
   czas1<-strptime(czass,"%Y-%m-%d %H:%M:%S")
   adress<-html(link)
   adres1<-html_nodes(adress,"h2 a")
   adres1<-html_attr(adres1,"href")
   wiaddnia<-html_nodes(adress,".base")
   wiaddnia<-html_text(wiaddnia,encoding="UTF-8")
   ktorewiaddnia<-na.omit(unlist(stri_extract_all_regex(wiaddnia,"Wiadomoœci dnia")))
   niesa<-attr(ktorewiaddnia,"na.action")# i ktore nie sa
   bierz<-setdiff(1:length(adres1),niesa)# bierzemy tylko te ktore sa
   adres1<-adres1[bierz]
   dl<-length(adres1)
   df<-data.frame()
   for(i in 1:dl)
   {
      if(unclass(czas1-strptime(gazetapl_artykul_info(adres1[i],i)$data,"%d-%m-%Y %H:%M"))<=0)
      {
         df<-rbind(df,gazetapl_artykul_info(adres1[i],i))
      }
   }
   odroznik<-Sys.time()#ta czesc nazwy pliku odroznia go od pozostalych
   odroznik1<-strftime(odroznik,"%Y-%m-%d %H:%M:%S %Z")
   writeLines(odroznik1,pathczas)
   path<-stri_paste(path1,"\\d",coll="")
   odroznik<-stri_replace_all_regex(odroznik,"\\.","")
   path<-stri_paste(path,odroznik,coll="")
   path<-stri_paste(path,".txt",coll="")
   if(nrow(df)!=0)
   {
      write.table(df,path)  
   }
   
   
}
gazetapl_info(l,p,cz)





