knitr::opts_chunk$set(echo = TRUE)
library(stringr)
library(tm)
datapath<-file.path(".","final","en_US")

datapath<-file.path(".","final","en_US","sampled")

removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

#file="en_US.blogs.txt"
myText<-Corpus(DirSource(datapath,pattern="*"))


myText <- tm_map(myText, removeNumbers)
myText <- tm_map(myText, removePunctuation)
myText <- tm_map(myText , stripWhitespace)
myText <- tm_map(myText, content_transformer(tolower))

myText<-tm_map(myText,content_transformer(removeURL))
myText<-tm_map(myText,content_transformer(removeHashTags))
myText<-tm_map(myText,content_transformer(removeTwitterHandles))

myText <- tm_map(myText, removeWords, stopwords("english")) 
freq<-data.frame(word=character(0),frequency=character(0),words=numeric(0))
for(ngrams in c(1:5)){
  print(paste("Doing ",ngrams," ngrams"))
for(i in c(1:3)){
  print(paste("Doing file ",i))
  myTextDtm<-DocumentTermMatrix(myText[i],
                                control=list(tokenizer=function(x){
                                  unlist(lapply(ngrams(words(x), ngrams), paste, collapse = " "), use.names = FALSE)
                                }))
  freq<-rbind(freq,data.frame(word=myTextDtm$dimnames$Terms,frequency=myTextDtm$v,words=ngrams))
}
}

saveRDS(freq,"ngrams.rds")

freq_agg<-aggregate(frequency~.,data=freq,FUN=sum)

saveRDS(freq_agg,"ngrams_agg.rds")