####################################################
### facebook.com
####################################################
### aby rozpoczac: CTRL+SHIFT+S
###################################################

require("Rfacebook")
require("stringi")
load("fb_oauth")

####################################################

# id stron na temat wybranych kadydatow na prezydenta
# dla stron na ktorych ukazal sie post po 2015/01/01
id.kandydant.all<-list(
      bronislaw_komorowski=c("113077252063553","903760883001833","268467546620008","108537862522546",
                             "553818191388543", "1535058823440445","1398961593742823", "449248448558093",  
                             "1438836586408237", "413090908855188",  "1609194429302220", "811168678956871",
                             "1562912467297933", "370428239811313",  "1559222271024577", "900130833371598",
                             "913128815385222", "1554360618180271", "428627390633869","1412589775718922",
                             "450938998408112",  "803427056406784"),
      andrzej_duda=c("160906857259563","1522296784688424"),
      magdalena_ogorek=c("427827194035100","1532372707037125","598366133596584","1340895182715750", 
                         "348097728708974", "1530592410526790", "399655096851013",  "575970469204903",  
                         "332431223631163",  "814165825294001",  "1016081188408742",
                         "1771867373038732"),
      pawel_kukiz=c("328170794051845",  "838077856255619", "749808495126104",  
                    "1559380707648545", "536949806446858"),
      adam_jarubas=c("140229586049381",  "583962355036720",  "1544799479104013", 
                     "901557673197599",  "1413267715636806","821440701256215",  "780491481998803",  
                     "415050505286595",  "1445193509055416", "1590345221203820", "1564789917117636"),
      janusz_korwin_mikke=c("277681892059",     "826989754027897",  "403245929843154",  "1527592940842688", 
                            "692610310813179",  "795841360427908",   "282757195247660",  "1481806018753061", 
                            "1540108336236735", "625623610869279",  "240193466178018",  "619949791452127", 
                            "572486229447767",  "1438870139682604", "1413639702253093",  "345979218853303"),
      janusz_palikot=c("564069080349050",  "1579227922299058",  "320028194786", 
                       "605295646244674",  "202306806581062",  "707491569370914"),
      anna_grodzka=c("259175827459379", "1375011909489052", "600799303354380",  "514521015354263" ),
      marian_kowalski=c("1596608107219296", "1522364481362800",  "769615856461529"))

#liczba kandydatow
ll<-length(id.kandydant.all)

#############################################
### posts & comments
#############################################

# funkcja, ktora 'zbiera' informacje na temat postow i komentarzy 
# publikowanych na stronach na temat kandydatow

facebook<-function(id.kandydant.all){
      kandydant<-data.frame()
      id.kandydant<-id.kandydant.all[[1]]
      kandydant.name<-names(id.kandydant.all)
      
      # zbieramy iformacje na temat artykulow z poprzedniego tygodnia, 
      # ale zapisujemy jedynie nowe posty i nowe komentarze do tych postow
      for(x in id.kandydant){
            kandydant<-rbind(kandydant, tryCatch(getPage(page=x, token=fb_oauth, 
                                                         since = stri_replace_all_regex(as.character(Sys.Date()-7), "-", "/"),
                                                         until = stri_replace_all_regex(as.character(Sys.Date()-1), "-", "/"), feed=FALSE),
                                                 error = function(e) return(invisible(NULL))))
      }
      
      kandydant<-kandydant[!is.na(kandydant$message),]
      kandydant$message<-stri_replace_all_regex(kandydant$message, ';',"")
      kandydant$message<-stri_trim_both(stri_replace_all_regex(kandydant$message,"(\\n)|(\\t)|(\\r)|(\")"," "))
      # posty i komentarze z dnia poprzedniego:
      kandydant_today<-kandydant[unlist(stri_extract_all_regex(kandydant$created_time, 
                                                               "[0-9\\-]{10}"))==(Sys.Date()-1),]
      
      # zapisywanie do pliku: kandydant.name_facebook
      n<-nrow(kandydant_today) #number of rows
      if(n!=0){
            fname<-paste0("Wybory\\Facebook\\", kandydant.name, "_facebook", ".csv")
            if(file.exists(fname)) {f<-file(fname, open="a")}
            if (!file.exists(fname)){
                  f<-file(fname, open="a")
                  #header:
                  writeLines(stri_paste('\"from_id\"','\"from_name\"','\"message\"','\"created_time\"',
                                        '\"type\"','\"link\"','\"id\"','\"likes_count\"',
                                        '\"comments_count\"','\"shares_count\"', sep=";"), f)
            }
            
            for(i in 1:n){
                  #dopisuje do pliku kolejny wiersz      
                  writeLines(stri_paste(paste0('"', kandydant_today[i,1],'"'), 
                                        paste0('"', kandydant_today[i,2],'"'),
                                        paste0('"', kandydant_today[i,3],'"'),
                                        paste0('"', kandydant_today[i,4],'"'),
                                        paste0('"', kandydant_today[i,5],'"'),
                                        paste0('"', kandydant_today[i,6],'"'),
                                        paste0('"', kandydant_today[i,7],'"'),
                                        paste0('"', kandydant_today[i,8],'"'),
                                        paste0('"', kandydant_today[i,9],'"'),
                                        paste0('"', kandydant_today[i,10],'"'),sep=";"),f)
            }
            close(f)
      }
      
      #########################################################################
      ### comments
      #########################################################################
      
      kandydant_comments<-data.frame()
      for(x in kandydant$id){
            GetPost<-getPost(x, token=fb_oauth, likes = FALSE)
            
            tmp_post<-as.data.frame(GetPost[1])
            tmp_post$post.message<-stri_replace_all_regex(tmp_post$post.message, ';', "")
            tmp_post$post.message<-stri_trim_both(stri_replace_all_regex(tmp_post$post.message,
                                                                         "(\\n)|(\\t)|(\\r)|(\")"," "))
            
            tmp_comments<-as.data.frame(GetPost[2])
            tmp_comments$comments.message<-stri_replace_all_regex(tmp_comments$comments.message, ';', "")
            tmp_comments$comments.message<-stri_trim_both(stri_replace_all_regex(tmp_comments$comments.message,
                                                                                 "(\\n)|(\\t)|(\\r)|(\")"," "))
            # komentarze z dnia poprzedniego
            tmp_comments<-tmp_comments[unlist(stri_extract_all_regex(tmp_comments$comments.created_time, 
                                                                     "[0-9\\-]{10}"))==(Sys.Date()-1),]
            m<-merge(tmp_post, tmp_comments)
            kandydant_comments<-rbind(kandydant_comments, m)
      }
      n<-nrow(kandydant_comments) #number of rows
      if(n!=0){
            gname<-paste0("Wybory\\Facebook\\",kandydant.name, "_facebook_comments", ".csv")
            if(file.exists(gname)) {g<-file(gname, open="a")}
            if (!file.exists(gname)){
                  g<-file(gname, open="a")
                  #header:
                  writeLines(stri_paste('\"post.from_id\"','\"post.from_name\"','\"post.message\"','\"post.created_time\"',
                                        '\"post.type\"','\"post.link\"','\"post.id\"','\"post.likes_count\"',
                                        '\"post.comments_count\"','\"post.shares_count\"', 
                                        '\"comments.from_id\"','\"comments.from_name\"','\"comments.message\"',
                                        '\"comments.created_time\"',
                                        '\"post.likes.count\"','\"comments.id\"',sep=";"), g)
            }
            
            for(i in 1:n){         
                  writeLines(stri_paste(paste0('"', kandydant_comments[i,1],'"'), 
                                        paste0('"', kandydant_comments[i,2],'"'),
                                        paste0('"', kandydant_comments[i,3],'"'),
                                        paste0('"', kandydant_comments[i,4],'"'),
                                        paste0('"', kandydant_comments[i,5],'"'),
                                        paste0('"', kandydant_comments[i,6],'"'),
                                        paste0('"', kandydant_comments[i,7],'"'),
                                        paste0('"', kandydant_comments[i,8],'"'),
                                        paste0('"', kandydant_comments[i,9],'"'),
                                        paste0('"', kandydant_comments[i,10],'"'),
                                        paste0('"', kandydant_comments[i,11],'"'),
                                        paste0('"', kandydant_comments[i,12],'"'),
                                        paste0('"', kandydant_comments[i,13],'"'),
                                        paste0('"', kandydant_comments[i,14],'"'),
                                        paste0('"', kandydant_comments[i,15],'"'),
                                        paste0('"', kandydant_comments[i,16],'"'),sep=";"),g)
            }
            close(g)
      }
}

# funkcja facebook dla kazdego kandydata:
for(i in 1:ll) facebook(id.kandydant.all[i])

############################################################
### LIKES
############################################################

# funkcja ktora zapisuje liczbe likeow dla stron na temat
# kandydatow na prezydenta

facebook.likes<-function(id.kandydant.all){
      likes<-data.frame()
      id.kandydant<-id.kandydant.all[[1]]
      kandydant.name<-names(id.kandydant.all)
      
      likes<-getUsers(users = id.kandydant, token=fb_oauth)
      likes<-likes[, c(1, 2, 9, 10)]
      #likes<-do.call(rbind.data.frame, likes)
      #likes<-data.frame(Reduce(rbind, likes))[, c(1, 2, 9, 10)]
      #names(likes)<-c("id", "name", "likes", "picture")
      #dodajemy kolumne z data
      likes<-cbind(likes, date=Sys.Date())
      
      # zapisywanie do pliku: kandydant.name_likes
      n<-nrow(likes) #number of rows
      if(n!=0){
            fname<-paste0("Wybory\\Facebook\\Likes\\", kandydant.name, "_likes", ".csv")
            if(file.exists(fname)) {f<-file(fname, open="a")}
            if (!file.exists(fname)){
                  f<-file(fname, open="a")
                  #header:
                  writeLines(stri_paste('\"id\"','\"name\"','\"likes\"','\"picture\"',
                                        '\"date\"', sep=";"), f)
            }
            
            for(i in 1:n){
                  #dopisuje do pliku kolejny wiersz      
                  writeLines(stri_paste(paste0('"', likes[i,1],'"'), 
                                        paste0('"', likes[i,2],'"'),
                                        paste0('"', likes[i,3],'"'),
                                        paste0('"', likes[i,4],'"'),
                                        paste0('"', likes[i,5],'"'),sep=";"),f)
            }
            close(f)
      }
      
}

# funkcja facebook.likes dla kazdego kandydata:
for(i in 1:ll) facebook.likes(id.kandydant.all[i])

################################################################
