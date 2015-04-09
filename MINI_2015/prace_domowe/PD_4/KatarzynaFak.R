# Napisać funkcję przyjmująca za argumenty markę, model, rodzaj paliwa,
# moc silnika, rok produkcji, przebieg
# a jako wynik zwracającą minimum, maksimum i kwartyle dla cen aut
# pasujących do argumentów.
# Jeżeli argument jest wskazany to należy zawęzić się tylko do aut z tym
# argumentem (np Model='Toyota'), jeżeli argument nie jest podany to nie
# należy filtrować po tym kryterium.
# 
# Wewnątrz funkcji należy wykorzystać na ile to możliwe funkcje
# dplyr'owe'.

require(PogromcyDanych)
dane <- tbl_df(auta2012)

# Swiadoma, ze taka funkcja moze nie przejsc proby Sprawdzajacego...
funkcja <- function( marka = dane%>%select(Marka)%>%as.matrix%>%unique,
                     model = dane%>%select(Model)%>%as.matrix%>%unique,
                     paliwo = dane%>%select(Rodzaj.paliwa)%>%as.matrix%>%unique,
                     moc = dane%>%select(kW)%>%as.matrix%>%unique,
                     rok = dane%>%select(Rok.produkcji)%>%as.matrix%>%unique, 
                     przebieg  = dane%>%select(Przebieg.w.km)%>%as.matrix%>%unique ){
      
      dane %>% filter(Marka%in%marka,
                      Model%in%model,
                      Rodzaj.paliwa%in%paliwo,
                      kW%in%moc,
                      Rok.produkcji%in%rok,
                      Przebieg.w.km%in%przebieg) %>%
            arrange(Cena.w.PLN) %>%
            summarise( minimum = first(Cena.w.PLN),
                       maksimum = last(Cena.w.PLN),
                       Q1 = as.numeric(quantile(Cena.w.PLN, 0.25)), # tak, daloby sie z funkcja nth i czterema ifami :)
                       mediana = nth(Cena.w.PLN, n = if( n()%%2==0 )
                             mean(n()/2,n()/2+1) else
                                   (n()+1)/2), 
                       Q3 = as.numeric(quantile(Cena.w.PLN, 0.75)),  # jw.
                       mediana_spr = median(Cena.w.PLN)
                       )
}
funkcja(marka="Kia")


# .. przesylam rowniez i te: :)
funkcja2 <- function( WybraneKolumny, Wartosci ){
      stopifnot(WybraneKolumny%in%names(dane))
      df <- dane
      for(i in seq_along(WybraneKolumny)){
          filter_(df, paste(WybraneKolumny[i], "%in%", Wartosci[i]) ) -> df
      }
      df %>%
            arrange(Cena.w.PLN) %>%
            summarise( minimum = first(Cena.w.PLN),
                       maksimum = last(Cena.w.PLN),
                       Q1 = as.numeric(quantile(Cena.w.PLN, 0.25)), # tak, daloby sie z funkcja nth i czterema ifami :)
                       mediana = nth(Cena.w.PLN, n = if( n()%%2==0 )
                             mean(n()/2,n()/2+1) else
                                   (n()+1)/2), 
                       Q3 = as.numeric(quantile(Cena.w.PLN, 0.75)),  # jw.
                       mediana_spr = median(Cena.w.PLN)
            )
}

## CO MOŻNA WPISAĆ DO PIERWSZEGO PARAMETRU funkcja2():
# "Cena"                         "Waluta"                       "Cena.w.PLN"                  
# "Brutto.netto"                 "KM"                           "kW"                          
# "Marka"                        "Model"                        "Wersja"                      
# "Liczba.drzwi"                 "Pojemnosc.skokowa"            "Przebieg.w.km"               
# "Rodzaj.paliwa"                "Rok.produkcji"                "Kolor"                       
# "Kraj.aktualnej.rejestracji"   "Kraj.pochodzenia"             "Pojazd.uszkodzony"           
# "Skrzynia.biegow"              "Status.pojazdu.sprowadzonego" "Wyposazenie.dodatkowe" 

funkcja2("Przebieg.w.km","100000")
funkcja(przebieg=100000)

funkcja2("Model","'Soul'")
funkcja(model="Soul")

funkcja2(WybraneKolumny=c("Marka","Model"), Wartosci=c("'Kia'","'Soul'"))
funkcja(model="Soul", marka="Kia")

funkcja(przebieg=100, marka="Volvo", paliwo="olej napedowy (diesel)")
funkcja2(c("Przebieg.w.km","Marka","Rodzaj.paliwa"), c("100","'Volvo'","'olej napedowy (diesel)'"))
#