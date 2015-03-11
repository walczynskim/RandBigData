# Kodowanie artykułów na portalu pudelek.pl
Marcin Kosinski  
Marzec 11, 2015  




# Wczytanie pakietu i pobranie listy artykulów


```r
library(rvest)
library(magrittr)
library(stringr)

pudelek <- html("http://www.pudelek.pl/")
artykuly <- html_nodes(pudelek,"div.entry__header h3 a")

 encodings <- lapply( html_attrs(artykuly), function(element){
     
     
     landing_page_artykulu <- html(element[1])
     charset <- html_nodes( landing_page_artykulu ,"meta[http-equiv=Content-Type]") %>% 
         html_attr("content") 
          
     if ( length( charset > 0 )){
          encoding <- str_split( string = charset, pattern = "=" )[[1]][2]
          encoding
     }else{ #charset nie jest spojny na wszystkich stronach
          encoding <- html_nodes( landing_page_artykulu , "meta[charset]" ) %>% 
              html_attrs( )   
          encoding[[1]]
     }
     
     
     
 })


encodings <- tolower(unlist(encodings))
```

# Jakie sa kodowania dla artykulów ze strony glównej pudelek.pl


```r
table(encodings)
```

```
encodings
utf-8 
   40 
```


# Dla ilu artykulów kodowanie bylo wprowadzone inaczej niz poprzez `meta http-equiv`



```r
table(names(encodings))
```

```

        charset 
     33       7 
```

```r
# puste to standardowy sposób
# o nazwie "charset" to archaizm
```



