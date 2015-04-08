library(stringi)
sciezka<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\efkonc"
sciezka1<-"C:\\Users\\Krzysztof\\Documents\\Pytlak_Rudas_Wasniewski\\przyklady\\"
#czestotliwosci

czestotliwosci<-c(czestotliwosc(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),po.zrodle=TRUE),
czestotliwosc(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),"07-04-2015",po.zrodle=TRUE),
czestotliwosc(sciezka,NULL,"tytul",datapierwszgopobrania(sciezka,NULL,FALSE),aktualnydzien(),po.zrodle=FALSE),
czestotliwosc(sciezka,"tvn24.pl","tresc",datapierwszgopobrania(sciezka,"tvn24.pl",TRUE),aktualnydzien(),po.zrodle=TRUE),
czestotliwosc(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),po.zrodle=TRUE),
czestotliwosc(sciezka,"wiadomosci.onet.pl","tagi",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),po.zrodle=TRUE),
czestotliwosc(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),funkcja=median,po.zrodle=TRUE))
path1<-stri_paste(sciezka1,"czestotliwosci.txt")
write.table(czestotliwosci,path1)

#iloscidni
iloscidni<-c(list(iloscDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)),
list(iloscDni(sciezka,NULL,"tytul",datapierwszgopobrania(sciezka,NULL,FALSE),aktualnydzien())),
list(iloscDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),maks=FALSE,5,po.zrodle=TRUE)),
list(iloscDni(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)),
list(iloscDni(sciezka,"wiadomosci.onet.pl","tagi",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)),
list(iloscDni(sciezka,"tvn24.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),po.zrodle=TRUE)))
path2<-stri_paste(sciezka1,"iloscidni.txt")
write.table(iloscidni,path2)

#przezileDni
przezIleDni<-c(list(przezileDni(sciezka,"gazeta.pl","tresc",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),TRUE)),
list(przezileDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),TRUE)),
list(przezileDni(sciezka,NULL,"tytul",datapierwszgopobrania(sciezka,NULL,FALSE),aktualnydzien(),FALSE)),
list(przezileDni(sciezka,"tvn24.pl","tytul",datapierwszgopobrania(sciezka,"tvn24.pl",TRUE),aktualnydzien(),TRUE)))
path3<-stri_paste(sciezka1,"przezileDni.txt")
write.table(przezIleDni,path3)

#ilezliczenprzezDni
ileZliczenprzezDni<-c(list(ilezliczenprzezDni(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",TRUE),aktualnydzien(),TRUE)),
list(ilezliczenprzezDni(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),TRUE)))
path4<-stri_paste(sciezka1,"ilezliczenprzezDni.txt")
write.table(ileZliczenprzezDni,path4)

#weekendy
week<-c(analizaweek(sciezka,"gazeta.pl","tytul",datapierwszgopobrania(sciezka,"gazeta.pl",FALSE),aktualnydzien(),TRUE),
analizaweek(sciezka,"tvn24.pl","tytul",datapierwszgopobrania(sciezka,"tvn24.pl",TRUE),aktualnydzien(),TRUE),
analizaweek(sciezka,"wiadomosci.onet.pl","tagi",datapierwszgopobrania(sciezka,"wiadomosci.onet.pl",TRUE),aktualnydzien(),TRUE),
analizaweek(sciezka,"wiadomosci.wp.pl","tresc",datapierwszgopobrania(sciezka,"wiadomosci.wp.pl",TRUE),aktualnydzien(),TRUE))
path5<-stri_paste(sciezka1,"weekendy.txt")
write.table(week,path5)