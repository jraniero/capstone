library(tm)
library(stringi)
enb <- stri_read_lines('./final/en_US/en_US.blogs.txt',locale = "en")
lowerenb <- stri_trans_tolower(enb)
quiz21 <- lowerenb[grepl('case',lowerenb)]
enb.vec <- VectorSource(quiz21)
enb <- Corpus(enb.vec)

enb <- tm_map(enb, removeNumbers)
enb <- tm_map(enb, removePunctuation)
enb <- tm_map(enb , stripWhitespace)
enb <- tm_map(enb, content_transformer(tolower))
#enb <- tm_map(enb, removeWords, stopwords("english")) 

enblog.tdm <- TermDocumentMatrix(enb)
BigramTokenizer <-function(x){
  unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)
}
bdm <- TermDocumentMatrix(enb, control = list(tokenize = BigramTokenizer))
case <- bdm$dimnames$Terms[grepl('case',bdm$dimnames$Terms)]
m = as.matrix(bdm[case,])
v = sort(rowSums(m), decreasing = TRUE)