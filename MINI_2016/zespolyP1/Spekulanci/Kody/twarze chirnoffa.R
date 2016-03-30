
setwd("C:/Users/Rafa³/Desktop/Projekt R")



library(dplyr)
library(RSQLite)

sterownik <- dbDriver("SQLite")
polaczenie <- dbConnect(sterownik, "dane.db")
dbListTables(polaczenie)

logi=dbReadTable(polaczenie, "logiCNK")
czas=dbReadTable(polaczenie, "timeCNK")






mediany<-czas%>%select(hostname, time)%>%group_by(hostname)%>%summarise(mediana=median(time))%>%arrange(mediana)
View(mediany)

grupa1=mediany$hostname[mediany$mediana<40 & mediany$mediana>10]
grupa2=mediany$hostname[mediany$mediana<60 & mediany$mediana>40]
grupa3=mediany$hostname[mediany$mediana<90 & mediany$mediana>60]
grupa4=mediany$hostname[mediany$mediana<109 & mediany$mediana>90]
grupa5=mediany$hostname[mediany$mediana<122 & mediany$mediana>109]
grupa6=mediany$hostname[mediany$mediana<149 & mediany$mediana>122]
grupa7=mediany$hostname[mediany$mediana<191 & mediany$mediana>149]
grupa8=mediany$hostname[mediany$mediana<500 & mediany$mediana>200]



czasy1=czas[czas$hostname==grupa1,c(3,4,5)]
czasy1$hostname="grupa1"
czasy1$day="blue"
czasy2=czas[czas$hostname==grupa2,c(3,4,5)]
czasy2$hostname="grupa2"
czasy1$day="orange"
czasy3=czas[czas$hostname==grupa3,c(3,4,5)]
czasy3$hostname="grupa3"
czasy3$day="green"
czasy4=czas[czas$hostname==grupa4,c(3,4,5)]
czasy4$hostname="grupa4"
czasy4$day="yellow"
czasy5=czas[czas$hostname==grupa5,c(3,4,5)]
czasy5$hostname="grupa5"
czasy5$day="red"
czasy6=czas[czas$hostname==grupa6,c(3,4,5)]
czasy6$hostname="grupa6"
czasy6$day="purple"
czasy7=czas[czas$hostname==grupa7,c(3,4,5)]
czasy7$hostname="grupa7"
czasy7$day="pink"
czasy8=czas[czas$hostname==grupa8,c(3,4,5)]
czasy8$hostname="grupa8"
czasy8$day="grey"

noweczasy=rbind(czasy1,czasy2,czasy3,czasy4,czasy5,czasy6,czasy7,czasy8)
head(noweczasy)



install.packages('ggvis')
install.packages('reshape2')
library('ggvis')
library('reshape2')
library(ggplot2)

#############################################


head(noweczasy)
names(noweczasy)=c("colors","hostname","time")

#Wykres gêstoœci

ggplot(noweczasy, aes(log(time), fill = hostname)) +geom_density(alpha = 0.7)+labs(title="Wykres gêstoœci logarytmu czasu po³¹czenia ze stanowiskiem w poszczególnych grupach",y="Czêstoœæ", x = "Log(czas po³¹czenia)")

#Wykres skrzypcowy z naniesionymi statystykami - œredni¹ i odchyleniem

data_summary <- function(x) {
	m <- mean(x)
	ymin <- m-sd(x)
	ymax <- m+sd(x)
	return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(noweczasy, aes(x=hostname, y=log(time),fill=hostname,colours=hostname)) + geom_violin()+ stat_summary(fun.data=data_summary,geom="pointrange", color="black")+labs(title="Wykres gêstoœci logarytmu czasu po³¹czenia ze stanowiskiem w poszczególnych grupach",x="Grupa", y = "Log(czas po³¹czenia)")

#Twarze Chernoffa

library(aplpack)
library(e1071)  
library(reldist)

head(noweczasy)
zmienne<-noweczasy%>%select(hostname, time)%>%group_by(hostname)%>%summarise(srednia=mean(time),mediana=median(time),odchylenie=sd(time),cv=sd(time)/mean(time),gini=gini(time),kurtoza=kurtosis(time),skosnosc=skewness(time),ile=n(),min=min(time),max=max(time),q1=quantile(time,1/8),q2=quantile(time,2/8),q3=quantile(time,3/8),q5=quantile(time,5/8),q6=quantile(time,6/8),q7=quantile(time,7/8))
View(zmienne)
faces(zmienne[,-1])

