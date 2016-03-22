library(stringi)

sciezka<-getwd()

# lista folderóœ będących miesiącami
foldery <- list.files(file.path(sciezka,"projekt1/dane"))

# tworzymy katolg, do którego będą zapisywane nasze pliki
dir.create(file.path(scizka,"projekt1/dane_csv"))

for (f in folder){
   # tworzymy folder danego miesiaca
   dir.create(file.path(sciezka,"projekt1/dane_csv",f))
   
   # lista eksponatow w danym folderze (miesiącu)
   urz<-list.files(file.path(sciezka,"projekt1/dane",f))
   
   for (u in urz){
      # tworzenie folderu danego eksponatu w danym miesiacu
      dir.create(file.path(sciezka,"projekt1/dane_csv",f,u))
      
      # sciezka pliku `.log`
      plik<-file.path(sciezka,"projekt1/dane",f,u,paste0(u,".log"))
      
      # wczytanie pliku z danymi
      tmp <- stri_read_lines(plik)
      
      # rozdzielenie kazdego wiersza z danych na przyszle kolumny w ramce danych
      lista <- stri_match_all_regex(tmp,"^([A-z]{3}) ([ 0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}) hostname=(cnk[0-9]{2}[a-m]?)(.*(?<=\\s)([0-9]+).*(?<=\\s)([0-9]+)|(.*))")
      
      # przeksztalcenie listy do ramki danych
      df <- data.frame(matrix(unlist(lista), nrow=length(lista), byrow=T))
      
      # dodanie kolumny z numerem wiersza w pliku `.log`
      df<-cbind(df,nr=1:nrow(df))
      
      # utworzenie ramki danych z samymi logowaniami użytkowników
      new_df<-df[stri_length(new_df$X7)>5,]
      
      # zapisanie obu tabel
      write.csv(new_df,file.path(sciezka,"projekt1/dane_csv",f,u,paste0(u,".csv")))
      write.csv(df,file.path(sciezka,"projekt1/dane_csv",f,u,paste0(u,"_cale",".csv")))
      
      # wyświetlenie folderu oraz eksponatu, aby wiedzieć na jakim eteapie jestem
      print(f)
      print(u)
   }
}
