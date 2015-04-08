
library("stringi")

#Funkcja generująca wzór w dniu dzisiejszym
generuj_dzisiejszy_wzor<-function(okres){
   if(okres=="dzien"){
      wzor<-as.character(Sys.Date())
   }
   else if(okres=="tydzien"){
      wzor<-Sys.Date()
      wzor<-as.POSIXlt(wzor)
      wzor<-ceiling((wzor$yday+4)/7)
      wzor<-paste0("2015-",wzor)
   }
   else if(okres=="miesiac"){
      wzor<-Sys.Date()
      wzor<-strftime(wzor, "%Y-%m")
   }
   return(wzor)
}

# typ<-c("dzien","tydzien","miesiac")
# generuj_dzisiejszy_wzor(typ[1])
# generuj_dzisiejszy_wzor(typ[2])
# generuj_dzisiejszy_wzor(typ[3])

#Funkcja generująca wektor dat (wzorów) pomocnych przy zapisie plików
generuj_date<-function(okres,od,do=generuj_dzisiejszy_wzor(okres)){
   if(okres=="dzien"){
      od<-as.Date(od)
      do<-as.Date(do)
      wektor<-as.character(seq(od,do,1))
   }
   else if(okres=="tydzien"){
      przedrostek<-unlist(stri_extract_all_regex(od,"^[^-]+"))
      przedrostek<-paste0(przedrostek,"-")
      od<-unlist(stri_extract_all_regex(od,"[^-]+$"))
      do<-unlist(stri_extract_all_regex(do,"[^-]+$"))
      wektor<-od:do
      wektor<-paste0(przedrostek,wektor)
   }
   else if(okres=="miesiac"){
      przedrostek<-unlist(stri_extract_all_regex(od,"^[^-]+"))
      przedrostek<-paste0(przedrostek,"-")
      od<-unlist(stri_extract_all_regex(od,"[^-]+$"))
      do<-unlist(stri_extract_all_regex(do,"[^-]+$"))
      wektor<-as.character(od:do)
      wektor<-sapply(wektor,function(element){
            if(stri_length(element)==1){
               element<-paste0("0",element)
            }
            else {
               element<-element
            }
         },USE.NAMES=FALSE)
      wektor<-paste0(przedrostek,wektor)
   }
   return(wektor)
}

# # przykłady
# 
# typ<-c("dzien","tydzien","miesiac")
# 
# ok<-typ[1]
# do<-Sys.Date()-2
# od<-do-5
# do<-as.character(do)
# od<-as.character(od)
# 
# generuj_date(ok,od) #domyślnie do dziś
# generuj_date(ok,od,do)
# 
# ok<-typ[2]
# od<-"2015-7"
# do<-"2015-13"
# generuj_date(ok,od) #domyślnie do dziś
# generuj_date(ok,od,do)
# 
# ok<-typ[3]
# od<-"2015-03"
# do<-"2015-12"
# generuj_date(ok,od) #domyślnie do dziś
# generuj_date(ok,od,do)
