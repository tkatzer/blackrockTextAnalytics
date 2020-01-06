url<-fromJSON("https://www.forbes.com/ajax/list/data?year=2019&uri=global2000&type=organization", dataframe = "columns")


library(jsonlite)
#Chrome, Untersuchen, Tab Network, /list url, --> nach Request URL schauen --> JSON in dataframe umwandeln, zur Sicherheit mal speichern
datacom = jsonlite::fromJSON(txt="https://www.forbes.com/ajax/list/data?year=2019&uri=global2000&type=organization")
save(datacom, file="data.Rda")
### Dataframe nach interessanten Spalten untersuchen
head(data)
### interessante Spalten in einem seperaten Dataframe ablegen
rawCompanyData<-datacom
save(rawCompanyData, file = "rawCompanyData.csv")

rank<-datacom[,2]
name<-datacom[,4]
industry<-datacom[,6]
country<-datacom[,7]
revenue<-datacom[,8]
marketvalue<-datacom[,9]
profits<-datacom[,12]
assets<-datacom[,13]
tidydata<-data.frame(rank,name,industry,country,revenue,marketvalue,profits,assets)
write.csv2(tidydata, file = "tidydata.csv")
