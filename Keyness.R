options(stringsAsFactors = FALSE)
library(dplyr)
library(quanteda)
getwd()
setwd("/Users/timkatzer/Desktop/tm4ss.github.io-master")
x <- read.csv2("data/sotu.csv")

Ignaural <- corpus(x) %>% 
  tokens(remove_symbols= TRUE,remove_punct = TRUE) %>% 
  tokens_remove(stopwords("en")) 

y<-kwic(Ignaural,"ryan")
y

%>%
  dfm() %>% 
  dfm_trim(min_termfreq = 10)

Ignaural_Trump <- textstat_keyness(Ignaural, target = which(docvars(Ignaural, 'president') == "Donald J. Trump")[3])

textplot_keyness(Ignaural_Trump)

