Sklej<-function(path1)
{
   dane<-data.frame()
   sciezkiplikow<-stri_paste(path1,list.files(path1))
   for(i in 1:length(sciezkiplikow))
   {
     d<-read.table(sciezkiplikow[i])
     dane<-rbind(dane,d)
   }
   dane$id<-1:nrow(dane)
   odroznik<-Sys.time()#ta czesc nazwy pliku odroznia go od pozostalych
   odroznik1<-strftime(odroznik,"%Y-%m-%d %H:%M:%S")
   odroznik1<-stri_replace_all_regex(odroznik1," ","_")
   odroznik1<-stri_replace_all_regex(odroznik1,":","_")
   path<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\efkonc\\a"
   path<-stri_paste(path,odroznik1)
   path<-stri_paste(path,".txt")
   write.table(dane,path)
}
p<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\riduzedane\\"
Sklej(p)



