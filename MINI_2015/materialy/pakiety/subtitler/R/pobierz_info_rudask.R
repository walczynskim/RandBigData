#' Funkcja zapisuje plik z napisami dla danego filmu i znajduje informacje o nim.
#'
#' Funkcja \code{pobierz_info_rudas_k(id,lan)} pobiera napisy ze strony http://www.opensubtitles.org/ a takze znajduje informacje o filmie z Wikipedii i z IMDb
#'
#'
#' @param id, id filmu (zgodnie z opensubtitles.org)
#' @param lan jezyk, w ktorym maja byc napisy (domyslnie angielski)
#'
#' @examples
#' pobierz_info_rudas_k(1) #Obcy 3
#' pobierz_info_rudas_k(123) #Schaolin Soccer
#' pobierz_info_rudas_k(2553) #Tora! Tora! Tora!
#' pobierz_info_rudas_k(31497,"pol") #Jak rozpetalem druga wojne swiatowa
#'
#' @import rvest
#' @import stringi
#'
#' @author Krzysztof Rudas

pobierz_info_rudas_k<-function(id,lan="eng")
{
  stopifnot(is.character(lan),length(lan)==1)
  stopifnot(is.numeric(id),length(id)==1)
  p<-pobierzIMDB(id)
  p1<-pobierzWiki(p[[1]],p[[4]])
  napisy(id,lan)
  c(p,p1)
}

czysc<-function(napis){
   napis<-stri_replace_all_regex(napis,"\n","")
   napis<-stri_replace_all_regex(napis,"\t","")
   napis<-stri_replace_all_regex(napis,"\b","")
   napis<-stri_replace_all_regex(napis,"\r","")
   napis<-stri_replace_all_regex(napis,"\a","")
   return(napis)
}

pobierzIMDB<-function(id)
{
   adres_pom<-paste0("http://www.opensubtitles.org/en/search/sublanguageid-eng/idmovie-",id)
   html0<-NULL
   try(html0<-html(adres_pom),silent=TRUE)
   if(length(html0)!=0)
   {
      nodes0<-html_nodes(html0,"td:nth-child(8) a")
      adres<-html_attr(nodes0,"href")[1]
      adres<-stri_replace_all_regex(adres,"/redirect/","")
      
      html1<-NULL
      try(html1<-html(adres),silent=TRUE)
      if(length(html1)!=0)
      {
         #Tytuly polskie
         nodes1<-html_nodes(html1,".header .itemprop")
         tytul_pol<-html_text(nodes1)
         if(length(tytul_pol)==0)
         {
            tytul_pol<-""
         }
         #Ewentualny tytul angielski
         nodes2<-html_nodes(html1,".title-extra")
         tytul_ang<-html_text(nodes2)
         if(length(tytul_ang)==0)
         {
            tytul_ang<-""
         }
         else
         {
            tytul_ang<-czysc(tytul_ang)
            tytul_ang<-unlist(stri_extract_all_regex(tytul_ang,"\".*\""))
            tytul_ang<-stri_replace_all_regex(tytul_ang,"\"","")
         }
         #Gatunek
         nodes3<-html_nodes(html1,".infobar .itemprop")
         gatunek<-html_text(nodes3)
         if(length(gatunek)==0)
         {
            gatunek<-""
         }
         #Rok powstania
         nodes4<-html_nodes(html1,".header .nobr")
         rok<-html_text(nodes4)
         if(length(rok)==0)
         {
            rok<--1
         }
         else
         {
            rok<-unlist(stri_extract_all_regex(rok,"[0-9]{4}"))
            rok<-rok[1]
            rok<-as.numeric(rok)
         }
         #Czas trwania
         nodes5<-html_nodes(html1,"#overview-top time")
         czas<-html_text(nodes5)
         if(length(czas)==0)
         {
            nodes5a<-html_nodes(html1,"#titleDetails time")
            czas<-html_text(nodes5a)
         }
         if(length(czas)==0)
         {
            czas<--1
         }
         else
         {
            czas<-unlist(stri_extract_all_regex(czas,"[0-9]{1,4}"))
            czas<-as.numeric(czas)
         }
         #Kraje
         nodes6<-html_nodes(html1,"#titleDetails.article>div.txt-block")
         kraj<-html_text(nodes6)
         if(length(kraj)==0)
         {
            kraj<-""
         }
         else
         {
            kraj<-kraj[stri_detect_regex(kraj,"Country:")]
            kraj<-czysc(kraj)
            kraj<-stri_replace_all_regex(kraj,"Country:","")
            kraj<-unlist(strsplit(kraj,"\\|"))
            kraj<-unlist(stri_extract_all_regex(kraj,"[^ ].*[^ ]"))
         }
         #streszczenie fabuly
         nodes7<-html_nodes(html1,"#titleStoryLine p")
         streszczenie<-html_text(nodes7)
         if(length(streszczenie)==0)
         {
            streszczenie<-""
         }
         else
         {
            streszczenie<-stri_replace_all_regex(streszczenie,"\n","")
            streszczenie<-stri_replace_all_regex(streszczenie,"Written by.*","")
            streszczenie<-czysc(streszczenie)
         }
         #Oceny
         nodes8<-html_nodes(html1,"strong span")
         
         #Ocena
         ocena<-as.numeric(html_text(nodes8))
         if(length(ocena)==0)
         {
            ocena<--1
         }
         #Rezyserzy
         html2<-html(paste0(adres,"fullcredits"))
         nodes9<-html_nodes(html2,".simpleCreditsTable:nth-child(2) a")
         rezyser<-html_text(nodes9)
         rezyser<-czysc(rezyser)
         
         rez<-data.frame()
         
         if(length(rezyser)!=0)
         {
            for(i in 1:length(rezyser))
            {
               p<-unlist(stri_extract_all_words(rezyser[i]))
               p<-data.frame(Imie=p[1],Nazwisko=p[length(p)])
               rez<-rbind(rez,p)
            }
         }
         else
         {
            rez<-rbind(rez,data.frame(Imie="",Nazwisko=""))
         }
         #Aktorzy
         nodes10<-html_nodes(html1,"#titleCast .itemprop")
         aktorzy<-html_text(nodes10)
         if(length(aktorzy)!=0)
         {
            aktorzy<-aktorzy[1:(length(aktorzy)/2)*2]
         }
         akt<-data.frame()
         if(length(aktorzy)!=0)
         {
            for(i in 1:length(aktorzy))
            {
               p<-unlist(stri_extract_all_words(aktorzy[i]))
               p<-data.frame(Imie=p[1],Nazwisko=p[length(p)])
               akt<-rbind(akt,p)
            }
         }
         else
         {
            akt<-rbind(akt,data.frame(Imie="",Nazwisko=""))
         }
         l<-list(tytul_pol,tytul_ang,gatunek,rok,czas,kraj,streszczenie,ocena,rez,akt)
         names(l)<-c("tytul_polski","tytul_angielski","gatunek","rok_produkcji",
                     "czas_trwania","kraj","streszczenie","ocena","rezyserowie","aktorzy")
         l
      }
      else
      {
         list()
      } 
   }
   else
   {
      return("Zle id")
   }
}




pobierzWiki<-function(tytul,rok)
{
   #Zwykle stri_extract_all_words nie dziala np. dla Tora! Tora! Tora!
   slowa<-unlist(stri_extract_all_regex(stri_paste(tytul," "),"[^ ]*[ ]"))
   slowa<-stri_replace_all_regex(slowa," ","")
   tytul <- paste(slowa, collapse="_")
   adres<- paste("http://pl.wikipedia.org/wiki/", tytul,"_(film_",rok,")",sep="")
   html1<-NULL
   try(html1 <- html(adres),silent=TRUE)
   if(length(html1)!=0)
   {
      info <- html_nodes(html1, "td")
   }
   else
   {
      info<-NULL
   }
   if(length(info)==0)
   {
      #przypadek szczegolny np. Tora!Tora!Tora!
      adres<- paste("http://pl.wikipedia.org/wiki/", tytul,"_(film)",sep="")
      html1<-NULL
      try(html1 <- html(adres),silent=TRUE)
      if(length(html1)!=0)
      {
         info<- html_nodes(html1, "td")
      }
      else
      {
         info<-NULL
      }
   }
   if(length(info)==0)
   {
      adres<- paste0("http://pl.wikipedia.org/wiki/", tytul)
      html1<-NULL
      try(html1 <- html(adres),silent=TRUE)
      if(length(html1)!=0)
      {
         info <- html_nodes(html1, "td")
      }
      else
      {
         info<-NULL
      }
   }
   if(length(info)!=0)
   {
      info<-html_text(info)
      nodes1<-html_nodes(html1,".infobox span:nth-child(1)")
      tytul<-html_text(nodes1)[1]
      p<-which(stri_detect_regex(info,"Gatunek"))
      if(length(p)==1)
      {
         gatunek<-info[p+1]
         gatunek<-unlist(strsplit(gatunek, split = ","))
      }
      else
      {
         gatunek<-""
      }
      p<-which(stri_detect_regex(info,"Rok produkcji"))
      if(length(p)==1)
      {
         rok_pr<-info[p+1]
         rok_pr<-as.numeric(rok_pr)

      }
      else
      {
         rok_pr<-(-1)
      }
      p<-which(stri_detect_regex(info,"Data premiery"))
      if(length(p)==1)
      {
         premiera<-info[p+1]
         premiera<-unlist(stri_extract_all_regex(premiera,"([0-9]{1,2}[^[0-9]]*)?[0-9]{4}"))
      }
      else
      {
         premiera<-""
      }
      p<-which(stri_detect_regex(info,"Czas trwania"))
      if(length(p)==1)
      {
         czas<-info[p+1]
         czas<-unlist(stri_extract_all_regex(czas,"[0-9]{1,4}"))
         czas<-as.numeric(czas)

      }
      else
      {
         czas<-(-1)
      }
      l<-list(tytul,gatunek,rok_pr,premiera,czas)
      names(l)=c("tytul","gatunek","rok_produkcji","premiera","czas_trwania")
      l
   }
   else
   {
      list()
   }
}

napisy<-function(id,lan="eng")
{
   adres <- paste("http://www.opensubtitles.org/en/search/sublanguageid-",
                  lan, "/idmovie-", id, sep="")
   html1 <- html(adres)
   nodes1 <- html_nodes(html1, "#search_results td:nth-child(5) a")
   if(length(nodes1)!=0)
   {
      href1 <- html_attr(nodes1, "href")[1]
      link_pobr <- paste("http://www.opensubtitles.org", href1, sep = "")
      nodes2<-html_nodes(html1,".bnone")
      name<-html_text(nodes2)[1]
      name<-czysc(name)
      name<-stri_replace_all_regex(name,"[ ]\\(.*\\)","")
      name<-stri_replace_all_regex(name," ","_")
      folder<-"subtitles"
      dir <- file.path(getwd(), folder)
      if(!file.exists(dir))
      {
         dir.create(file.path(getwd(), folder))
      }
      dojscie <- paste(folder, "/", name, ".zip", sep = "")
      download.file(link_pobr, dojscie, mode = "wb")
   }
   else
   {
      return("Nie ma napisow dla zadanego id lub jezyka")
   }
      
}




