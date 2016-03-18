library(rvest)
library(tm)
library(wordvector)
library(devtools)
library(magrittr)
library(tm)
library(ggplot2)
library(wordcloud)

#load in corpus from DHfasion. note the mispelling
setwd('~\\DHfasion')
filename = "aesth"
setwd("C:\\Users\\maxwell\\Documents\\DHfasion\\aesth1")
# Read the PDF file
temp <- VCorpus(DirSource("C:\\Users\\maxwell\\Documents\\DHfasion\\aesth1"))
temp2 <- VCorpus(DirSource("C:\\Users\\maxwell\\Documents\\DHfasion\\aesth21"))
count<-0
for (hj in list.dirs('~\\DHfasion\\')){
  while(count<70){
    ext<-paste('\\aesth',count,sep="")
    pat=paste(hj,ext,sep="")
    if (file.exists(file.path(pat))){
      temp2 <- VCorpus(DirSource(file.path(hj)))
      temp<-c(temp,temp2)#combine
      print(pat)
    }
    
    count<-count+1
  }
  count<-0
}

#do preprocessing on the corpus
corp<-temp
temp1 <- tm_map(corp, stripWhitespace)
temp2 <- tm_map(temp1 , content_transformer(tolower))
temp3 <- tm_map(temp2 , removeWords , stopwords("SMART"))
temp5 <- tm_map(temp3 , content_transformer(removePunctuation))
temp6 <- tm_map(temp5 , content_transformer(removeNumbers))

tdm <- TermDocumentMatrix(temp6) 
tfre=as.matrix(tdm)
#tfreq<-findFreqTerms(tfre, 20)
s = sort(rowSums(tfre), decreasing=TRUE)
df = data.frame(word = names(s) , freq  = s )
wordcloud(words = df$word, freq = df$freq, min.freq = 20, max.words = 100, random.order = FALSE)

barplot(df[1:9,]$freq,names.arg = df[1:9,]$word)


tmat=weightTfIdf(tdm,normalize=TRUE)
tfidffre=as.matrix(tmat)
s1 = sort(rowSums(tfidffre), decreasing=TRUE)
df1 = data.frame(word = names(s1) , freq  = s )
wordcloud(words = df1$word, freq = df1$freq, min.freq = 20, max.words = 100, random.order = FALSE)
barplot(df1[1:6,]$freq,names.arg = df1[1:6,]$word)


tmat2=weightSMART(tdm)
tmat2=as.matrix(tmat2)
s2 = sort(rowSums(tmat2), decreasing=TRUE)
df2 = data.frame(word = names(s2) , freq  = s )
wordcloud(words = df2$word, freq = df2$freq, min.freq = 20, max.words = 100, random.order = FALSE)
barplot(df2[1:9,]$freq,names.arg = df2[1:9,]$word)


tmat=weightTfIdf(tdm,normalize=TRUE)
tdm2<-removeSparseTerms(tmat,sparse=0.60)#0.80=189terms,07=readability
r<-as.matrix(tdm2)
dm<-dist(scale(r))
fit<-hclust(dm,method="ward.D")
plot(fit,cex=0.90,hang=-1) 
rect.hclust(fit,k=3)

f<-findAssocs(tmat,"fashion",corlimit=0.73)
barplot(fashion$fashion)

aesthetic<-findAssocs(tmat,"aesthetic",corlimit=0.84)
barplot(aesthetic$aesthetic)

reform<-findAssocs(tmat,"reform",corlimit=0.87)
barplot(reform$reform)

tdm.de<-removeSparseTerms(tdm,sparse=0.90)#0.80=189terms,07=readability
assocs<-findAssocs(tdm.de,c("reform","aesthetic","fashion","ruskin","ready"),corlimit=0.60assocs)
assoc
a<-f$fashion
names(a)#a is a double so a gives values
tm(a)
?tm_reduce
assocs

#create netwrok from adjacency matrix
tdm2<-removeSparseTerms(tdm,sparse=0.60)#0.80=189terms,07=readability
small<-as.matrix(tdm2)
small[small>=1]<-1#make binary
small.m2<-small%*%t(small)
tf<-graph.adjacency(small.m2,weighted=TRUE,mode='undirected')
tf2.g<-simplify(tf)

tf<-graph.adjacency(a,weighted=TRUE,mode='undirected')
c(a$american,a$day)
plot(tf2.g,layout=layout.auto,vertex.size=20,edge.width=1)

library(igraph)



library(topicmodels)
dtm<-DocumentTermMatrix(temp6)

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k<-5
ldaOut <-LDA(dtm.new,k, method='VEM')

rowTotals <- apply(dtm , 1, sum)
dtm.new   <- dtm[rowTotals> 0, ]  

  ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics <- as.matrix(topics(ldaOut))
#empty.rows <- dtm[rowTotals == 0, ]$dimnames[1][[1]] get corpus and document names
#corpus <- corpus[-as.numeric(empty.rows)]to be the same

findAssocs(dtm, 'reform', .50)

















words <- rownames(findAssocs(dtm, 'reform', .005))[1:20]
find1 <- colnames(dtm) %in% words
corr <- cor(as.matrix(a$reform))
#plot heatmap of correlations
library(corrplot)
library(extrafont)
par(family="AppleMyungjo") # ????????? ????????? ??????
corrplot(corr, type = "upper")


a<-findAssocs(dtm, 'reform', .005)[1:20]
corr <- cor(as.matrix(a$reform))
corrplot(corr, type = "upper")


build_graph(dtm.new)
install.packages("igraph")
install.packages("network") 
install.packages("sna")
install.packages("ndtv")


graph.data.frame()
