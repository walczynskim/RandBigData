# Projekt 'Centrum Nauki Kopernik' -  R And Big Data 2015/2016
### Zofia Rogala, Ewa Baranowska, Bartosz Topolski

### Faza 1 projektu:
Przetworzyliśmy dostarczone nam pliki .log do formy ramek danych, oddzielnych dla każdego urządzenia i miesiąca, z informacją o:

* miesiącu użytkowania
* dniu użytkowania
* id użytkownika
* id sesji
* informacją czy użytkownik użył urządzenia czy tylko włożył do niego kartę (czy pojawił się ekran     powitalny)
* czasem rozpoczęcia interakcji z urządzeniem
* czasem zakończenia interakcji z urządzeniem.

Przetworzone pliki tekstowe znajdują się w folderze *Raporty*, a skrypt R-owy *scrap_multithread.R* użyty do przetworzenia w folderze *Kody*. Następnie przetworzyliśmy otrzymane raporty do plików .csv po jednym dla każdego urządzenia, zawierających informacje o:

* dacie interakcji
* id użytkownika
* czasem rozpoczęcia interakcji z urządzeniem
* czasem zakończenia interakcji z urządzeniem
* różnicy w czasach użytkowania liczonej w sekundach.

Przetworzone raporty znajdują się w folderze *Przetworzone*, a skrypt R-owy to *przetwarzanie_raportow.R*

Następnie po wstępnych analizach, zrobiliśmy 4 wykresy: 

* 2 boxploty (rozkład czasu użytkowania urządzenia cnk46a w podziale na dni tygodnia i miesiące)
* heatmapę ( procent ilości użytkowników urządzenia cnk46a w rozbiciu na dni tygodnia i godziny)
* wykres szeregów czasowych dla średniego czasu odwiedzin dla 3 wybranych stacji w ciągu roku
* wykres słupkowy dla całkowitego czasu użytkowania 3 urządzeń w ciągu dnia w rozbiciu na dni tygodnia.

Plakat znajduje się w folderze *Plakat*, natomiast kody do tworzenia wykresów w folderze *Kody*.




