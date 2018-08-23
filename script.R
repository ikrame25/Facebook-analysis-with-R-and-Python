
library(Rfacebook)
library(httpuv)
library(methods)
library(reticulate)
library(rjson)
library(csv)
library(RCurl)
library(httr)
library(xlsx)
library(DT)
library(foreach)
library(plotrix)
library(rsconnect)
library(PythonInR)
library(NLP)
library(circlize)
library(RColorBrewer)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(tm)

#library(cleanNLP)
#library(textreuse)
#library(text2vec)
#library(coreNLP)
#library(tm)
#library(tidytext)
#library(koRpus)
#library(corpustools)
#library(kerasR)
#library(RTextTools)
#library(stringi)
#library(tokenizers)
#library(readxl)
#library(antiword)
#library(plyr)
#library(rPython)

Functionwordcloud<-function(p){
  #p<-ScrappingPosts(nompage, tokenpage)
  #textvectors
  textVector<-c(p$message, encoding = "UTF-8",language="fr")
  #View(textVector) it works just fine
  Texte<-SpecialCharacterCleaner(textVector)
  textcorpus <- Corpus(VectorSource(Texte))
  textcorpus <- tm_map(textcorpus,content_transformer(function(x) iconv(x,"UTF-8", "ASCII//TRANSLIT")))
  textcorpus <- tm_map(textcorpus, content_transformer(tolower))
  textcorpus <- tm_map(textcorpus, removePunctuation)
  textcorpus <- tm_map(textcorpus,removeWords, c("tous","â€™","ÙŠÙˆÙ","ÙŠØ","ÙˆØ","ÙƒØ","grÃ","Ù\u0081ÙŠ","ÙƒÙŠÙ\u0081Ø","plus"))
  textcorpus <- tm_map(textcorpus, function(x)removeWords(x,stopwords(kind = "fr")))
  textcorpus <- tm_map(textcorpus, removeNumbers)
  textcorpus <- tm_map(textcorpus, function(x)stemDocument(x,language="french"))
  
  dtm <- TermDocumentMatrix(textcorpus)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  set.seed(1234)
  pal <- rev(brewer.pal(10,"RdYlBu"))
  wordcloudResult<-wordcloud::wordcloud(words = d$word, freq = d$freq, scale=c(4,0.5), max.words = 200, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  
  return(wordcloudResult)
  
}
SpecialCharacterCleaner <- function(text_vector){
  text_vector <- gsub("[ä]", "ae", text_vector)
  text_vector <- gsub("[ö]", "oe", text_vector)
  text_vector <- gsub("[ü]", "ue", text_vector)
  text_vector <- gsub("[è]", "e", text_vector)
  text_vector <- gsub("[é]", "e", text_vector)
  text_vector <- gsub("[à]", "a", text_vector)
  text_vector <- gsub("[ê]", "e", text_vector)
  text_vector <- gsub("[ç]", "c", text_vector)
  text_vector <- gsub("[ô]", "o", text_vector)
  text_vector <- gsub("[î]", "i", text_vector)
  text_vector <- gsub("[ù]", "u", text_vector)
  text_vector <- gsub("[â€™]", "", text_vector)
  return(text_vector)
  
}

ChangetoCorpusFunction<-function(text_vector){
  
  Texte<-SpecialCharacterCleaner(Functionwordcloud(text_vector))
  text_corpus <- Corpus(VectorSource(Texte))
  text_corpus <- tm_map(text_corpus,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, removePunctuation)
  text_corpus <- tm_map(text_corpus,removeWords, c("tous","â€™","ÙŠÙˆÙ","ÙŠØ","ÙˆØ","ÙƒØ","grÃ","Ù\u0081ÙŠ","ÙƒÙŠÙ\u0081Ø","plus"))
  text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords(kind = "fr")))
  text_corpus <- tm_map(text_corpus, removeNumbers)
  text_corpus <- tm_map(text_corpus, function(x)stemDocument(x,language="french"))
  
  return(inspect(Texte))
}
ScrappingPosts<-function(nompage,tokenpage){
  
  page_FB<-Rfacebook::getPage(nompage,tokenpage,n=10000)
  #plottrying<- plot (page$likes_count, main = "ok", xlab = "numéro de la donnée", ylab = "likes_count", col = "blue")
  return(page_FB)
}
ScrappingComms<-function(nomdepage,tokendepage){
  page<-getPage(nomdepage,tokendepage,n=1000)
  page_FB<-foreach(i=1:nrow(page)) %do% 
  {
    post <- getPost(page$id[i],tokendepage, n.comments=1000, likes=FALSE)
  }
  lescommentaires<-data.frame()
  foreach(i=1:length(page_FB)) %do%
  { 
    commentaire<-data.frame(page_FB[[i]]$comments)
    lescommentaires<-rbind(lescommentaires,commentaire)
  }
  return(lescommentaires)
}
ScrappingReactions<-function(nomdepage,tokendepage){
  page<-getPage(nomdepage,tokendepage,n=1000)
  reactions=data.frame()
  foreach(i=page$id) %do% {
    reaction<-data.frame(getReactions(i,tokendepage))
    reactions<-rbind(reactions,reaction)
  }
  return(reactions)
}
ScrappingCommentReplies<-function(ScrappingComms,tokendepage){
  replies<-foreach(i=ScrappingComms$id) %do%
  {com<-getCommentReplies(i,tokendepage,n=1000)}
  return(replies)
}
source_python("PythonScript.py")
centre_dinteret<-function(donnees){
  interets<-as.data.frame(Frequencedesmots(donnees))
  interets<-t(interets)
  #interets<-as.data.frame(interets)
  interets<-data.frame(Mots=row.names(interets),frequences=interets[row.names(interets),])
  #interets<-data.frame(frequences=interets[row.names(interets),])
  returnValue(interets)}
analyse_sentimentale<-function(centredinteret){
  Analyse<-subset(centredinteret,row.names(centredinteret)%in%c('positif','negatif'))
  returnValue(Analyse)
}
mots_frequentes<-function(centredinteret){
  
  motsfrequentes<-subset(centredinteret,!(row.names(centredinteret) %in% c('positif','negatif')))
  motsfrequentes<-motsfrequentes[order(motsfrequentes$frequences,decreasing = TRUE),]
  returnValue(motsfrequentes)}
wordcloud_Comms<-function(mots_frequentes){
  set.seed(1234)
  wordcloud::wordcloud(words = mots_frequentes$Mots, freq = mots_frequentes$frequences, min.freq = 0.4,
                       max.words = Inf, random.order=FALSE, rot.per=0.35, 
                       colors=brewer.pal(8, "Dark2"))}