Zakres:
-------

# Motto

50% narzędzi o których myślałem w grudniu układając sylabs będzie przestarzała gdy w czerwcu będziemy kończyć zajęcia. 
Nie dlatego, że narzędzia są źle wybrane, ale dlatego że tak szybko się rozwijają.

Dlatego celem tych zajęć nie jest poznanie żadnego konkrentego języka, narzędzia czy techniki, ale:

1. poznanie problemów, które pojawiają się przy pracy z dużymi danymi,
2. poznanie podejść do rozwiązywania tych problemów,
3. wykształcenie umiejętności ciągłego wyszukiwania rozwiązań dla pojawiających się problemów,
4. krytycznego patrzenia na własne rozwiązania,
5. uczenia się z rozwiązań innych.



# Punkt wyjścia

Pomysł na zajęcia bazuje silnie na doświadczeniach z Data School
http://www.dataschool.io/teaching-data-science/#usingvideosasteachingtools

Być może nie wszystkie pomysły tam przedstawione są dobre, to właśnie sprawdzimy.



# Sylabus

Dostęp i przetwarzanie dużych zbiorów danych
* Web scraping [2 spotkania ]
* Praca z pakietami tidyr + dplyr [2 spotkania] 
* Praca z SQL i bazami danych [1 spotkanie]

Web based data products
* Tworzenie pakietów [1 spotkanie]
* OpenCPU i serwer REST [2 spotkania]
* Shiny [2 spotkania]

Obliczenia równoległe / masywne
* Równoległe uruchamianie zadań na domowym komputerze [1 spotkanie]
* profilowanie i debugowanie kodu [1 spotkanie]
* Praca z klastrami obliczeniowymi HPC [2 spotkania]
* Praca z hadoop/spark [1 spotkanie]


Materia&#322;y:
---------------

* Web scraping https://rawgithub.com/pbiecek/RandBigData/master/MINI_2015/materialy/webscrap/scrap.html
* Awesome R https://github.com/qinwf/awesome-R


Plan spotka&#324; MINI PW:
-------------------------

* 26 II - nie było zajęć, BigDataTech
* 5 III
* 12 III
* 19 III - prezentacja pierwszej części projektu 'Wybory'
* 26 III
* 9 IV  - prezentacja drugiej części projektu 'Wybory'
* 16 IV 
* 23 IV - prezentacja pierwszej części projektu 'Filmy'
* 30 IV
* 7 V 
* 14 V - prezentacja trzeciej części projektu 'Wybory'
* 21 V - prezentacja drugiej części projektu 'Filmy'
* 28 V
* 11 VI - prezentacja trzeciej części projektu 'Filmy'


Projekty:
---------

Projekty mogą być realizowane w grupach od 2 do 4 osób. Te same osoby nie mogą razem uczestniczyć w obu projektach.
Projekty będą przedstawiane na zajęciach (terminy wypisane powyżej). Należy je zgłosić przez GitHuba przed prezentacją. Projekty, które nie będą zaprezentowane nie zostaną ocenione.

1. Projekt 'Wybory'

Pierwsza tura wyborów prezydenckich odbędzie się 10 maja. Większe partie wystawiły swoich kandydatów, powoli rozkręcają się kampanie. W ramach projektu 'wybory' każda grupa będzie z wybranego medium / mediów zbierać dane o ,,widoczności'' kandydatów na prezydenta oraz przeanalizują kontekst w jakim pojawiają się nazwiska kandydatów. Realizacja projektu podzielona jest na 3 części:

A. Celem jest zbudowanie rozwiązania, które w automatyczny sposób zbiera informacje o tym gdzie w Internecie i w jakim kontekście pojawiają się nazwiska kandydatów. Ocenie podlegać będzie ilość zebranych danych, automatyzacja procesu zbierania danych oraz różnorodność obserwowanych źródeł danych (obserwować można portale informacyjne czy twittera). [max 10 punktow]

B. Celem jest opracowanie procesu przetwarzania danych uzyskiwanych przez narzędzie opracowane w fazie A. Wynikiem tej fazy jest zbiór wskaźników opisujących ,,widoczność'' kandytdatów. Takim wskaźnikiem może być liczba dni, w których dane nazwisko znajdowało się na głównej stronie portalu internetowego X, wielkość kroju pisma jakim dane nazwisko było napisane, wydźwięk kontekstu w jakim to nazwisko się znalazło. Ocenie podlegać będzie liczba i różnorodność wzkaźników oraz automatyzacja procesu ich oceny. Ocenie podlegać będzie również ilość nowych uzyskanych danych. [max 20 punktow]

C. Celem jest przedstawienie jak wskaźniki opracowane w punkcie B zmieniają się w czasie. Prezentacja powinna automatycznie się aktualizować i być możliwie autonomiczna. Ocenie podlegać będzie autonomiczność, iość zebranych danych, ilość obserowanych wskaźników oraz pomysłowość całego rozwiazania. [max 30 punktów]


2. Projekt 'Filmy'

W internecie w wielu miejscach można znaleźć informacje o filmach, między innymi w bazie danych IMDB czy w Wikipedii.
W ramach projektu 'Filmy' budować będziemy mapę filmów. Na bazie informacji z różnych źródeł opracowana zostanie macierz podobieństwa pomiędzy filmami, która następnie zostanie przedstawiona w postaci mapy. Realizacja projektu podzielona jest na 3 części:

A. Celem jest zebranie możliwie kompleksowych informacji o możliwie dużym zbiorze filmów. Ocenie podlegać będzie ilość zebranych danych, różnorodność zebranych charakterystyk oraz ilość przeanalizowanych filmów. [max 20 punktów]

B. Celem jest zbudowanie macierzy odległośći pomiędzy filmami opartą o zebrane dane. Następnie na bazie macierzy odległości należy zbudować mapę filmów lub seriali. [max 20 punktów]

C. Celem jest zbudowanie usługi internetowej opartej o zebrane dane o filmach. Aplikacja może być dostępna jako pakiet w R, jako serwis REST lub jako aplikacja shiny. [max 20 punktów]



Ocena:
------
Ocena ko&#324;cowa zale&#380;e&#263; b&#281;dzie od sumy ocen cz&#261;stkowych z dwóch projektów (2*(10+20+30) = 120 pkt).

Prace domowe:
-------------
Cz&#281;&#347;&#263; wyk&#322;adów ko&#324;czy&#263; si&#281; b&#281;dzie zadaniem domowym. Samodzielne wykonanie zadania domowego i przes&#322;anie przez GitHuba to zysk 4 punktów.

