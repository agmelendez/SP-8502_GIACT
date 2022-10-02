library(readxl)
ForoDiscusión <- read_excel("ForoDiscusión.xlsx")
View(ForoDiscusión)

attach(ForoDiscusión)
names(ForoDiscusión)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stringr")

text <- readLines(file.choose(), encoding="UTF-8")
text = iconv(text, to="ASCII//TRANSLIT")

a<- ForoDiscusión$Respuesta
docs<-readLines("respuesta.csv")
docs<-ForoDiscusión$Respuesta
docs <- text

# Load the data as a corpus
docs <- Corpus(VectorSource(docs))

#Inspect the content of the document
inspect(docs)

#Replacing "/", "@" and "|" with space:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, ":")

#Cleaning the text
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

#Remove punctuation
docs <- tm_map(docs, toSpace, "[[:punct:]]")

#Remove numbers
docs <- tm_map(docs, toSpace, "[[:digit:]]")

# add two extra stop words: "available" and "via"
myStopwords = readLines("stopwords.txt")

# remove stopwords from corpus
docs <- tm_map(docs, removeWords, myStopwords)

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("flight","you","air","lion","airline","reviewed")) 

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Remove URL
removeURL <- function(x) gsub("http[[:alnum:]]*", " ", x)
docs <- tm_map(docs, removeURL)

#Replace words
docs <- tm_map(docs, gsub, pattern="Howver", replacement="However")

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


dataframe<-data.frame(text=unlist(sapply(docs, `[`)), stringsAsFactors=F)

write.csv(dataframe, "lista.csv")
save.image()

names(ForoDiscusión)

boxplot(edad)
mean(edad)
sd(edad)
t.test(edad)
t.test(edad ,mu=60.5)
t.test(edad,mu=55.5, alternative="less")
ks.test(edad,"pnorm",mean(edad),sd(edad))
summary(edad)
sd(edad)
ks.test(edad,"pnorm",60.31,10.64586)

#1.2 Prueba Kolmogorov Smirnov

ks.test(edad,"pnorm",mean(edad),sd(edad))
summary(edad)
sd(edad)
ks.test(edad,"pnorm",60.31,10.64586)

#1.3 Prueba Shapiro-Wilks

shapiro.test(edad)
ks.test(edad,"pexp",1/mean(edad))
hist(edad)

#1.4 Prueba de Jarque Bera

#install.packages("tseries")
library(tseries)
jarque.bera.test(edad)

#1.6 Test de Wilcoxon

median(edad)
wilcox.test(edad, mu=59)

#tahap 2

setwd("/cloud/project/Sentiment-Analysis-with-R-master/")
kalimat2<-read.csv("lion2.csv",header=TRUE)

#skoring
positif <- scan("positive-words.txt",what="character",comment.char=";")
negatif <- scan("negative-words.txt",what="character",comment.char=";")
kata.positif = c(positif, "is near to")
kata.negatif = c(negatif, "cant")
score.sentiment = function(kalimat2, kata.positif, kata.negatif, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(kalimat2, function(kalimat, kata.positif, kata.negatif) {
    kalimat = gsub('[[:punct:]]', '', kalimat)
    kalimat = gsub('[[:cntrl:]]', '', kalimat)
    kalimat = gsub('\\d+', '', kalimat)
    kalimat = tolower(kalimat)
    
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=kalimat2)
  return(scores.df)
}
library(plyr)
hasil = score.sentiment(kalimat2$text, kata.positif, kata.negatif)
View(hasil)

#CONVERT SCORE TO SENTIMENT
hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif",ifelse(hasil$score==0,"Netral","Positif"))
hasil$klasifikasi
View(hasil)

#EXCHANGE ROW SEQUENCE
data <- hasil[c(3,1,2)]
View(data)
write.csv(data, file = "lion3.csv")



#tahap 3
#sebelum lanjut ke tahap ini pisahkan data lion3.cvs menjadi ulasan Negatif,Netral, Positif kedalam file berbeda 



#UlasanNegatif

docs<-readLines("lionNeg.csv")

# Load the data as a corpus
docs <- Corpus(VectorSource(docs))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("airlines"))

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Replace words
docs <- tm_map(docs, gsub, pattern="delayed", replacement="delay")


#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)

#asosiasi kata
asoskataNeg<-as.list(findAssocs(dtm, terms =c("delay","time","hours","cheap","chek",
                                              "bad","worst","flights","service"),
                                corlimit = c(0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10)))

asoskataNeg

#barplot
k<-barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,cex.axis=1.2,cex.names=1.2,
           main ="Most frequent words",
           ylab = "Word frequencies",col =topo.colors(20))

termFrequency <- rowSums(as.matrix(dtm))
termFrequency <- subset(termFrequency, termFrequency>=5)

text(k,sort(termFrequency, decreasing = T)-
       1,labels=sort(termFrequency, decreasing = T),pch = 6, cex =
       1)


#ulasannetral

docs<-readLines("lionNet.csv")

# Load the data as a corpus
docs <- Corpus(VectorSource(docs))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("airlines"))

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)

#asosiasi kata
asoskataNet<-as.list(findAssocs(dtm, terms =c("time","service","late","check","budget",
                                              "cheap","flights","fly","june"),
                                corlimit = c(0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15)))

asoskataNet

#barplot
k<-barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,cex.axis=1.2,cex.names=1.2,
           main ="Most frequent words",
           ylab = "Word frequencies",col =topo.colors(20))

termFrequency <- rowSums(as.matrix(dtm))
termFrequency <- subset(termFrequency, termFrequency>=5)

text(k,sort(termFrequency, decreasing = T)-
       1,labels=sort(termFrequency, decreasing = T),pch = 6, cex =
       1)



#ulasanPositif

docs<-readLines("lionPos.csv")

# Load the data as a corpus
docs <- Corpus(VectorSource(docs))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("airlines"))

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 15)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)

#asosiasi kata
asoskataPos<-as.list(findAssocs(dtm, terms =c("time","good","service","price","low",
                                              "check","friendly","cost","budget"),
                                corlimit = c(0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15)))

asoskataPos

#barplot
k<-barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,cex.axis=1.2,cex.names=1.2,
           main ="Most frequent words",
           ylab = "Word frequencies",col =topo.colors(20))

termFrequency <- rowSums(as.matrix(dtm))
termFrequency <- subset(termFrequency, termFrequency>=5)

text(k,sort(termFrequency, decreasing = T)-
       1,labels=sort(termFrequency, decreasing = T),pch = 6, cex =
       1)