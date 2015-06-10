

W folderze `shiny` jest aplikacja shiny.
W pliku `pomniejszenie_macierzy.R` są kody, które pomniejszają początkowe macierze, które miały wielkość 200-300 MB,
do macierzy, które mają po 1.5-3 MB. Dla każdego filmu pozostawiamy odległości tylko dla 20 najbliższych filmów, a następnie
dla takiej rzadzkiej macierzy przechodzimy na jej postać rzadką. Takie rozwiązanie świetnie nadaje się upublicznienia aplikacji w internecie, ze względu na lekkość danych.

Liczba filmów w aplikacji: 8400.

Proponowane kryterium oceny projektu: liczba filmów dzielona przez sumę megabajtów danych potrzebnych do uruchomienia aplikacji.