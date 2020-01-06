datacom<-read.csv2(file = "tidydata.csv")
pn<-c()
##pn stellt die Summe aller Vorkomnisse einer der 2000 größten Firmen in Artikeln dar
## manche Firmennamen kommen als Regexpattern sehr häufig vor weil sie Bestandteile von Wörtern sein können ("att", "informa").
## kommen die Firmennamen auffällig häufig vor, werden diese Firmen nocheinmal mit Leerzeichen überprüft um sicherzustellen, dass sie als Firmenname alleine stehen

for (i in 1:length(datacom$name)){
  pn[i]<-sum(grepl(datacom$name[i], lexiscorpus$documents$texts, ignore.case = TRUE))
  if(pn[i]>50){
    pn[i]<-sum(grepl(paste("",datacom$name[i],""), lexiscorpus$documents$texts, ignore.case = TRUE))
  }
}
write.csv2(pn, file = "pn.csv")

options(stringsAsFactors = F)
setwd("/Users/timkatzer/Desktop/ArtikelBlackrock")

pn<-read.csv2(file = "pn.csv")
pn<-pn[,2]
datacom<-read.csv2(file = "tidydata.csv")
datacom$name[pn>05]
comp<-pn[pn>09]
mean(comp)
barplot(comp, names.arg = datacom$name[pn>09], las = 3)


##ind ist ein Dataframe der die Unternehmensnamen mit den Indizes enthält in denen Firmen vorkommen
ind<-data.frame(datacom$name,list(c(1:2000)))
for (i in 1:length(datacom$name)){ 
  if(pn[i]>0){
    if(sum(grepl(datacom$name[i], lexiscorpus$documents$texts, ignore.case = TRUE))>50){
      ind$X1.2000[i]<-list(which(grepl(paste("",datacom$name[2],""), lexiscorpus$documents$texts, ignore.case = TRUE)))
    } else{
      ind$X1.2000[i]<-list(which(grepl(datacom$name[i], lexiscorpus$documents$texts, ignore.case = TRUE)))
    }
  } else {
    ind$X1.2000[i]<-NA
  }}
ind2<-ind
ind$X1.2000[2]

#qualitative AnaLyse 4 Firmen zur näheren Untersuchung: 3m, apple, eon, rbc
## rbc Kooperation mit Blackrock
## Unlist um die Listen der Indizes zu entpacken
## Indizevectoren benutzen um die zur Firma passenden Artikel in eigene DF zu laden
com3m<-unlist(ind$X1.2000[which(grepl("3m",ind$datacom.name))])
comapple<-unlist(ind$X1.2000[which(grepl("apple",ind$datacom.name))])
comeon<-unlist(ind$X1.2000[which(grepl("eon",ind$datacom.name))])
comrbc<-unlist(ind$X1.2000[which(grepl("rbc",ind$datacom.name))])
article3m<-data[c(com3m),]
articleapple<-data[c(comapple),]
articleeon<-data[c(comeon),]
articlerbc<-data[c(comrbc),]


## 4 Firmen aussuchen. ind$X1.2000 als vector abspeichern und dann aus DF nur die mit den Indizes filtern.
## diese UnternehmensDF speichern!. Danach group_by Firma und Sentimentanalyse machen 
## jetzt Nennungen nach Zeit herausfinden! anschließend auffällige Zeitpunkte filtern und analysieren
## im Anschluss mit Aktienkurs vergleichen

####3m Zeitreihe
a1 <- article3m %>% separate('date', into=c("year","month","day"), sep = "-")

counts_per_year <- a1 %>%
  group_by(year)

testi<-attributes(counts_per_year)
graph<-data.frame()
for ( i in 1:length(testi$groups$.rows)){
  graph[i,1]<-testi$groups$year[i]
graph[i,2]<-length(testi$groups$.rows[[i]])
}
write.csv2(graph, file = "graph3m.csv")
graph<-read.csv2(file = "graph3m.csv")

graph<-graph[,c(2,3)]
plot(graph,main="Häuigkeit 3m", ylab = "Anzahl", xlab = "Jahr", type="l")
####apple Zeitreihe!
a2 <- articleapple %>% separate('date', into=c("year","month","day"), sep = "-")

counts_per_year2 <- a2 %>%
  group_by(year)

testi2<-attributes(counts_per_year2)
graph2<-data.frame()
for ( i in 1:length(testi2$groups$.rows)){
  graph2[i,1]<-testi2$groups$year[i]
  graph2[i,2]<-length(testi2$groups$.rows[[i]])
}

write.csv2(graph2, file = "graphapple.csv")
graph2<-read.csv2(file = "graphapple.csv")

graph2<-graph2[,c(2,3)]

plot(graph2,main="Häuigkeit Apple", ylab = "Anzahl", xlab = "Jahr", type="l")

#####eon Zeitreihe
a3 <- articleeon %>% separate('date', into=c("year","month","day"), sep = "-")

counts_per_year3 <- a3 %>%
  group_by(year)

testi3<-attributes(counts_per_year3)
graph3<-data.frame()
for ( i in 1:length(testi3$groups$.rows)){
  graph3[i,1]<-testi3$groups$year[i]
  graph3[i,2]<-length(testi3$groups$.rows[[i]])
}

write.csv2(graph3, file = "grapheon.csv")
graph3<-read.csv2(file = "grapheon.csv")

graph3<-graph3[,c(2,3)]



plot(graph3,main="Häuigkeit eon", ylab = "Anzahl", xlab = "Jahr", type="l")

#####rbc
a4 <- articlerbc %>% separate('date', into=c("year","month","day"), sep = "-")

counts_per_year4 <- a4 %>%
  group_by(year)

testi4<-attributes(counts_per_year4)
graph4<-data.frame()
for ( i in 1:length(testi4$groups$.rows)){
  graph4[i,1]<-testi4$groups$year[i]
  graph4[i,2]<-length(testi4$groups$.rows[[i]])
}

write.csv2(graph4, file = "graphrbc.csv")
graph4<-read.csv2(file = "graphrbc.csv")

graph4<-graph4[,c(2,3)]


plot(graph4,main="Häuigkeit rbc", ylab = "Anzahl", xlab = "Jahr", type="l")

par(mfrow=c(2,2))
plot(graph,main="Häuigkeit 3m", ylab = "Anzahl", xlab = "Jahr", type="l")
plot(graph2,main="Häuigkeit Apple", ylab = "Anzahl", xlab = "Jahr", type="l")
plot(graph3,main="Häuigkeit eon", ylab = "Anzahl", xlab = "Jahr", type="l")
plot(graph4,main="Häuigkeit rbc", ylab = "Anzahl", xlab = "Jahr", type="l")




library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
install.packages("textdata")
library(textdata)
afinn_terms <- get_sentiments(lexicon = "afinn")

positive_terms_all <- afinn_terms$word[afinn_terms$value > 0]
negative_terms_all <- afinn_terms$word[afinn_terms$value < 0]

## das ist jetzt eine Sentimentanalyse mit allen Texten 
positive_terms_in_text <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- rowSums(DTM[, positive_terms_in_text])

negative_terms_in_text <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- rowSums(DTM[, negative_terms_in_text])


counts_all_terms <- rowSums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)

datedate <- data %>% separate('date', into=c("year","month","day"), sep = "-")

sentiments_per_year <- aggregate(relative_sentiment_frequencies, by = list(year = datedate$year), mean)

head(sentiments_per_year)

require(reshape2)
df <- melt(sentiments_per_year, id.vars = "year")
require(ggplot2)
ggplot(data = df, aes(x = year, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()






###Sentimentanalyse nach Firma 3m

counts_per_yearcorpus<-corpus(counts_per_year$article[counts_per_year$year=="2019"] , docnames = counts_per_year$month[counts_per_year$year=="2019"])
docvars(counts_per_yearcorpus, "month")<-counts_per_year$month[counts_per_year$year=="2019"]

corpus_tokens2 <- counts_per_yearcorpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed")%>%
  tokens_remove(pattern = stopwords, padding = T)

collocations <- textstat_collocations(corpus_tokens2, min_count = 20)
sum(collocations$count > 10)
head(collocations, 10)
# compound collocations
corpus_tokens2 <- tokens_compound(corpus_tokens2, collocations)

# Create DTM (also remove padding empty term)
DTM <- corpus_tokens2 %>% 
  tokens_remove("") %>%
  dfm() 


positive_terms_in_text <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- rowSums(DTM[, positive_terms_in_text])

negative_terms_in_text <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- rowSums(DTM[, negative_terms_in_text])


counts_all_terms <- rowSums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)

datedate <- data %>% separate('date', into=c("year","month","day"), sep = "-")
counts_per_year <- counts_per_year %>%
  group_by(month)

testi<-attributes(counts_per_year)
graph<-data.frame()
for ( i in 1:length(testi$groups$.rows)){
  graph[i,1]<-testi$groups$year[i]
  graph[i,2]<-length(testi$groups$.rows[[i]])
}

sentiments_per_month <- aggregate(relative_sentiment_frequencies, by = list(month = counts_per_yearcorpus$documents$month), mean)

head(sentiments_per_year)

require(reshape2)
df <- melt(sentiments_per_month, id.vars = "month")
write.csv2(df, file = "sentiment3m.csv")
df<-read_csv2(file = "sentiment3m.csv")
require(ggplot2)
ggplot(data = df, aes(x = month, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()

title <- counts_per_year$title[counts_per_year$year=="2019"]
title[counts_per_year$month=="11"]

###Sentimentanalyse nach Firma apple
counts_per_yearcorpus2<-corpus(counts_per_year2$article[counts_per_year2$year=="2015"] , docnames = counts_per_year2$month[counts_per_year2$year=="2015"])
docvars(counts_per_yearcorpus2, "month")<-counts_per_year2$month[counts_per_year$year=="2015"]

corpus_tokens2 <- counts_per_yearcorpus2 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed")%>%
  tokens_remove(pattern = stopwords, padding = T)

collocations <- textstat_collocations(corpus_tokens2, min_count = 20)
sum(collocations$count > 10)
head(collocations, 10)
# compound collocations
corpus_tokens2 <- tokens_compound(corpus_tokens2, collocations)

# Create DTM (also remove padding empty term)
DTM <- corpus_tokens2 %>% 
  tokens_remove("") %>%
  dfm() 


positive_terms_in_text <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- rowSums(DTM[, positive_terms_in_text])

negative_terms_in_text <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- rowSums(DTM[, negative_terms_in_text])


counts_all_terms <- rowSums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)


sentiments_per_month <- aggregate(relative_sentiment_frequencies, by = list(month = counts_per_yearcorpus2$documents$month), mean)

head(sentiments_per_month)

require(reshape2)
df <- melt(sentiments_per_month, id.vars = "month")
write.csv2(df, file = "sentimentapple.csv")
df<-read_csv2(file = "sentimentapple.csv")
require(ggplot2)
ggplot(data = df, aes(x = month, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()



###Sentimentanalyse nach Firma eon
counts_per_year3<-na.omit(counts_per_year3)
counts_per_yearcorpus3<-corpus(counts_per_year3$article[counts_per_year3$year=="2016"] , docnames = counts_per_year3$month[counts_per_year3$year=="2016"])
docvars(counts_per_yearcorpus3, "month")<-counts_per_year3$month[counts_per_year3$year=="2016"]

corpus_tokens2 <- counts_per_yearcorpus3 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed")%>%
  tokens_remove(pattern = stopwords, padding = T)

collocations <- textstat_collocations(corpus_tokens2, min_count = 20)
sum(collocations$count > 10)
head(collocations, 10)
# compound collocations
corpus_tokens2 <- tokens_compound(corpus_tokens2, collocations)

# Create DTM (also remove padding empty term)
DTM <- corpus_tokens2 %>% 
  tokens_remove("") %>%
  dfm() 


positive_terms_in_text <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- rowSums(DTM[, positive_terms_in_text])

negative_terms_in_text <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- rowSums(DTM[, negative_terms_in_text])


counts_all_terms <- rowSums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)


sentiments_per_month <- aggregate(relative_sentiment_frequencies, by = list(month = counts_per_yearcorpus3$documents$month), mean)

head(sentiments_per_month)

require(reshape2)
df <- melt(sentiments_per_month, id.vars = "month")
write.csv2(df, file = "sentimenteon.csv")
df<-read_csv2(file = "sentimenteon.csv")
require(ggplot2)
ggplot(data = df, aes(x = month, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()

###Sentimentanalyse nach Firma rbc

counts_per_yearcorpus4<-corpus(counts_per_year4$article[counts_per_year4$year=="2015"] , docnames = counts_per_year4$month[counts_per_year4$year=="2015"])
docvars(counts_per_yearcorpus4, "month")<-counts_per_year4$month[counts_per_year4$year=="2015"]

corpus_tokens2 <- counts_per_yearcorpus4 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed")%>%
  tokens_remove(pattern = stopwords, padding = T)

collocations <- textstat_collocations(corpus_tokens2, min_count = 20)
sum(collocations$count > 10)
head(collocations, 10)
# compound collocations
corpus_tokens2 <- tokens_compound(corpus_tokens2, collocations)

# Create DTM (also remove padding empty term)
DTM <- corpus_tokens2 %>% 
  tokens_remove("") %>%
  dfm() 


positive_terms_in_text <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- rowSums(DTM[, positive_terms_in_text])

negative_terms_in_text <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- rowSums(DTM[, negative_terms_in_text])


counts_all_terms <- rowSums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)


sentiments_per_month <- aggregate(relative_sentiment_frequencies, by = list(month = counts_per_yearcorpus4$documents$month), mean)

head(sentiments_per_month)

require(reshape2)
df <- melt(sentiments_per_month, id.vars = "month")
write.csv2(df, file = "sentimentrbc.csv")
df<-read_csv2(file = "sentimentrbc.csv")
require(ggplot2)
ggplot(data = df, aes(x = month, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) + coord_flip()