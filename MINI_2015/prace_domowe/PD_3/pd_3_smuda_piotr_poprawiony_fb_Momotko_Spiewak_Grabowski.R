###################################
###        facebook.com         ###
###################################
### aby rozpocząć: CTRL+SHIFT+S ###
###################################

require("Rfacebook")
require("stringi")
load("D:/Dokumenty/studia/8 semestr/R i Big Data/poprawa_pd/fb_oauth")

###################################

   sciezka<-"D:/Dokumenty/studia/8 semestr/R i Big Data/poprawa_pd/Facebook/"
   dir.create(sciezka,showWarnings=FALSE)

   #id stron wybranych kadydatów na prezydenta, dla stron na których ukazał się post po 2015/01/01
   id.kandydant.all<-list(
      bronislaw_komorowski=c("113077252063553","903760883001833","268467546620008","108537862522546",
                             "553818191388543","1535058823440445","1398961593742823","449248448558093",
                             "1438836586408237","413090908855188","1609194429302220","811168678956871",
                             "1562912467297933","370428239811313","1559222271024577","900130833371598",
                             "913128815385222","1554360618180271","428627390633869","1412589775718922",
                             "450938998408112","803427056406784"),
      andrzej_duda=c("160906857259563","1522296784688424"),
      magdalena_ogorek=c("427827194035100","1532372707037125","598366133596584","1340895182715750",
                         "348097728708974","1530592410526790","399655096851013","575970469204903",
                         "332431223631163","814165825294001","1016081188408742",
                         "1771867373038732"),
      pawel_kukiz=c("328170794051845","838077856255619","749808495126104",
                    "1559380707648545","536949806446858"),
      adam_jarubas=c("140229586049381","583962355036720","1544799479104013",
                     "901557673197599","1413267715636806","821440701256215","780491481998803",
                     "415050505286595","1445193509055416","1590345221203820","1564789917117636"),
      janusz_korwin_mikke=c("277681892059","826989754027897","403245929843154","1527592940842688",
                            "692610310813179","795841360427908","282757195247660","1481806018753061",
                            "1540108336236735","625623610869279","240193466178018","619949791452127",
                            "572486229447767","1438870139682604","1413639702253093","345979218853303"),
      janusz_palikot=c("564069080349050","1579227922299058","320028194786",
                       "605295646244674","202306806581062","707491569370914"),
      anna_grodzka=c("259175827459379","1375011909489052","600799303354380","514521015354263" ),
      marian_kowalski=c("1596608107219296","1522364481362800", "769615856461529"))

   #liczba kandydatów
   ll<-length(id.kandydant.all)

   ########################
   ### posts & comments ###
   ########################

   #funkcja, która 'zbiera' informacje na temat postów i komentarzy publikowanych na stronach o kandydatach

   facebook<-function(id.kandydant.all){
      kandydant<-data.frame()
      id.kandydant<-id.kandydant.all[[1]]
      kandydant.name<-names(id.kandydant.all)

      #zbieramy informacje z poprzedniego tygodnia, ale zapisujemy jedynie nowe posty i komentarze do tych postow
      for(x in id.kandydant){
         kandydant<-rbind(kandydant, tryCatch(getPage(page=x, token=fb_oauth,
                  since = stri_replace_all_regex(as.character(Sys.Date()-7), "-", "/"),
                  until = stri_replace_all_regex(as.character(Sys.Date()-1), "-", "/"), feed=FALSE),
                  error = function(e) return(invisible(NULL))))
      }

      #obróbka tekstu
      kandydant<-kandydant[!is.na(kandydant$message),]
      kandydant$message<-stri_replace_all_regex(kandydant$message, ';',"")
      kandydant$message<-stri_trim_both(stri_replace_all_regex(kandydant$message,"(\\n)|(\\t)|(\\r)|(\")"," "))

      #posty i komentarze z dnia poprzedniego:
      kandydant_today<-kandydant[unlist(stri_extract_all_regex(kandydant$created_time,
                                                               "[0-9\\-]{10}"))==(Sys.Date()-1),]

      #zapisywanie do pliku: kandydant.name_facebook
      n<-nrow(kandydant_today) #liczba postów dla kandydata (wierszy)
      if(n!=0){
         fname<-paste0(sciezka,kandydant.name,"_facebook.csv")
         if(file.exists(fname)){
            f<-file(fname, open="a")
         }
         else{
            f<-file(fname, open="a")
            #header gdy tworzymy plik:
            writeLines(stri_paste('\"from_id\"','\"from_name\"','\"message\"','\"created_time\"',
                                  '\"type\"','\"link\"','\"id\"','\"likes_count\"',
                                  '\"comments_count\"','\"shares_count\"', sep=";"), f)
         }

         for(i in 1:n){
            #dopisujemy do pliku kolejny wiersz
            wiersz<-paste0(paste0('"',kandydant_today[i,],collapse='";'),'"')
            writeLines(wiersz,f)
         }
         close(f)
      }

      ################
      ### comments ###
      ################

      kandydant_comments<-data.frame()
      for(x in kandydant$id){
         GetPost<-getPost(x, token=fb_oauth, likes = FALSE)

         #obróbka tekstu
         tmp_post<-as.data.frame(GetPost[1])
         tmp_post$post.message<-stri_replace_all_regex(tmp_post$post.message, ';', "")
         tmp_post$post.message<-stri_trim_both(stri_replace_all_regex(tmp_post$post.message,
                                                                              "(\\n)|(\\t)|(\\r)|(\")"," "))

         tmp_comments<-as.data.frame(GetPost[2])
         tmp_comments$comments.message<-stri_replace_all_regex(tmp_comments$comments.message, ';', "")
         tmp_comments$comments.message<-stri_trim_both(stri_replace_all_regex(tmp_comments$comments.message,
                                                                              "(\\n)|(\\t)|(\\r)|(\")"," "))
         #komentarze z dnia poprzedniego
         tmp_comments<-tmp_comments[unlist(stri_extract_all_regex(tmp_comments$comments.created_time,
                                                                  "[0-9\\-]{10}"))==(Sys.Date()-1),]
         m<-merge(tmp_post, tmp_comments)
         kandydant_comments<-rbind(kandydant_comments, m)
      }
      n<-nrow(kandydant_comments) #liczba komentarzy dla kandydata (wierszy)
      if(n!=0){
         gname<-paste0(sciezka,kandydant.name,"_facebook_comments.csv")
         if(file.exists(gname)){
            g<-file(gname, open="a")
         }
         else{
            g<-file(gname, open="a")
            #header gdy tworzymy plik:
            writeLines(stri_paste('\"post.from_id\";\"post.from_name\";\"post.message\";\"post.created_time\"',
                                  '\"post.type\";\"post.link\";\"post.id\";\"post.likes_count\"',
                                  '\"post.comments_count\";\"post.shares_count\";\"comments.from_id\"',
                                  '\"comments.from_name\";\"comments.message\";\"comments.created_time\"',
                                  '\"post.likes.count\";\"comments.id\"',sep=";"), g)
         }

         for(i in 1:n){
            wiersz<-paste0(paste0('"',kandydant_comments[i,],collapse='";'),'"')
            writeLines(wiersz,g)
         }
         close(g)
         }
   }

   #wywołanie funkcji facebook dla każdego kandydata:
   for(i in 1:ll){
      facebook(id.kandydant.all[i])
   }

   #############
   ### LIKES ###
   #############

   #funkcja która zapisuje liczbę like-ów dla stron o kandydatach na prezydenta

   facebook.likes<-function(id.kandydant.all){
      likes<-data.frame()
      id.kandydant<-id.kandydant.all[[1]]
      kandydant.name<-names(id.kandydant.all)

      likes<-getUsers(users = id.kandydant, token=fb_oauth)
      likes<-likes[, c(1, 2, 9, 10)]

      #dodajemy kolumnę z datą
      likes<-cbind(likes, date=Sys.Date())

      #zapisywanie do pliku: kandydant.name_likes
      n<-nrow(likes) #liczba stron dla kandydata (wierszy)
      if(n!=0){
         sciezka<-paste0(sciezka,"Likes/")
         dir.create(sciezka,showWarnings=FALSE)
         fname<-paste0(sciezka, kandydant.name, "_likes.csv")
         if(file.exists(fname)){
            f<-file(fname, open="a")
         }
         else{
            f<-file(fname, open="a")
            #header gdy tworzymy plik:
            writeLines('\"id\";\"name\";\"likes\";\"picture\";\"date\"', f)
         }

         for(i in 1:n){
            #dopisujemy do pliku kolejny wiersz
            data<-as.character(likes[i,5])
            wiersz<-paste0(paste0('"',likes[i,-5],collapse='";'),'";"',data,'"')
            writeLines(wiersz,f)
         }
         close(f)
      }

   }

   #wywołanie funkcji facebook.likes dla każdego kandydata:
   for(i in 1:ll){
      facebook.likes(id.kandydant.all[i])
   }

###################################
