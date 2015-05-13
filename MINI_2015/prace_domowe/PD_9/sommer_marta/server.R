library(shiny)
library(PogromcyDanych)
library(dplyr)

auta <- auta2012

wybierz_samochody <- function(
  rodzaj_paliwa,
  min_moc_silnika,
  max_moc_silnika,
  min_rok_produkcji,
  max_rok_produkcji,
  przebieg_ponizej,
  czy_klimatyzacja,
  czy_centralny_zamek,
  czy_autoalarm,
  sortuj_po_cenie_rosnaco,
  kraj_pochodzenia){
  
  if(sortuj_po_cenie_rosnaco==TRUE){
    auta %>%
      mutate(Autoalarm = grepl(pattern = "autoalarm", Wyposazenie.dodatkowe),
             Centralny.zamek = grepl(pattern = "centralny zamek", Wyposazenie.dodatkowe),
             Klimatyzacja = grepl(pattern = "klimatyzacja", Wyposazenie.dodatkowe)) %>%
      filter(
        Rodzaj.paliwa %in% rodzaj_paliwa, 
        kW >= min_moc_silnika, 
        kW <= max_moc_silnika,
        Rok.produkcji >= min_rok_produkcji,
        Rok.produkcji <= max_rok_produkcji,
        Przebieg.w.km <= przebieg_ponizej,
        Klimatyzacja == czy_klimatyzacja,
        Centralny.zamek == czy_centralny_zamek,
        Autoalarm == czy_autoalarm,
        Kraj.pochodzenia %in% kraj_pochodzenia) %>%
      arrange(Cena.w.PLN)
  } else{
    auta %>%
      mutate(Autoalarm = grepl(pattern = "autoalarm", Wyposazenie.dodatkowe),
             Centralny.zamek = grepl(pattern = "centralny zamek", Wyposazenie.dodatkowe),
             Klimatyzacja = grepl(pattern = "klimatyzacja", Wyposazenie.dodatkowe)) %>%
      filter(
        Rodzaj.paliwa %in% rodzaj_paliwa, 
        kW >= min_moc_silnika, 
        kW <= max_moc_silnika,
        Rok.produkcji >= min_rok_produkcji,
        Rok.produkcji <= max_rok_produkcji,
        Przebieg.w.km <= przebieg_ponizej,
        Klimatyzacja == czy_klimatyzacja,
        Centralny.zamek == czy_centralny_zamek,
        Autoalarm == czy_autoalarm,
        Kraj.pochodzenia %in% kraj_pochodzenia) %>%
      arrange(-Cena.w.PLN)
  }
  
  
}

shinyServer(function(input, output) {
  
  output$tabela <- renderTable({
    
    wybierz_samochody(
      rodzaj_paliwa = input$rodzaj_paliwa,
      min_moc_silnika = input$moc_silnika[1],
      max_moc_silnika = input$moc_silnika[2],
      min_rok_produkcji = input$min_rok_produkcji,
      max_rok_produkcji = input$max_rok_produkcji,
      przebieg_ponizej = input$maks_przebieg,
      czy_klimatyzacja = ifelse("k" %in% input$opcje_dodatkowe, TRUE, FALSE),
      czy_centralny_zamek = ifelse("c" %in% input$opcje_dodatkowe, TRUE, FALSE),
      czy_autoalarm = ifelse("a" %in% input$opcje_dodatkowe, TRUE, FALSE),
      sortuj_po_cenie_rosnaco = input$czy_rosnaco,
      kraj_pochodzenia = input$kraj)
    
  })
 
    output$podsumowanie <- renderPrint({
      cat("Witaj ", input$text, "! Pomozemy Ci wybrac najlepszy samochod na dzien", 
          as.character(input$date), "!")
    })
  
})
