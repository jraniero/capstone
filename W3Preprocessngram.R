library(data.table)
library(tm)
#freq_raw<-readRDS("ngrams.rds")
freq_clean<-data.table(freq_raw)
setkey(freq_clean,words,word)
#freq_clean<-freq_raw[freq_raw$frequency>1,] #Remove single occurrences

#freq_agg<-aggregate(frequency~.,data=freq_clean,FUN=sum)

#freq_clean[, totalFreq := sum(frequency), by = list(word,words)]
saveRDS(freq_clean,"ngrams_agg.rds")

myText<-Corpus(VectorSource("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"))


myText <- tm_map(myText, removeNumbers)
myText <- tm_map(myText, removePunctuation)
myText <- tm_map(myText , stripWhitespace)
myText <- tm_map(myText, content_transformer(tolower))

#myText<-tm_map(myText,content_transformer(removeURL))
#myText<-tm_map(myText,content_transformer(removeHashTags))
#myText<-tm_map(myText,content_transformer(removeTwitterHandles))

myText <- tm_map(myText, removeWords, stopwords("english")) 