#Fuente: https://rstudio-pubs-static.s3.amazonaws.com/460563_44403a3b087a415797de75ba3b21d17b.html

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- readLines(file.choose(), encoding="UTF-8")
#Abrir archivo Discurso. 

text = iconv(text, to="ASCII//TRANSLIT") # Convert Character Vector between Encodings

text

corpus <- Corpus(VectorSource(text)) # formato de texto
# lleva a minúsculas
d  <- tm_map(corpus, tolower)
# quita espacios en blanco
d  <- tm_map(d, stripWhitespace)
# quita la puntuación
d <- tm_map(d, removePunctuation)
# quita los números
d <- tm_map(d, removeNumbers)
stopwords("spanish")
# remueve palabras vacías genericas
d <- tm_map(d, removeWords, stopwords("spanish"))
# crea matriz de términos
tdm <- TermDocumentMatrix(d)
findFreqTerms(tdm, lowfreq=4)

frecuentes<-findFreqTerms(tdm, lowfreq=1)

findAssocs(tdm, "pandemia", 0.45)
findAssocs(tdm, frecuentes, rep(0.45, rep=5) )

#Sumarización
#Los siguientes pasos son para hacer la sumarización

m <- as.matrix(tdm) #lo vuelve una matriz
v <- sort(rowSums(m),decreasing=TRUE) #lo ordena y suma
df <- data.frame(word = names(v),freq=v) # lo nombra y le da formato de data.frame

### TRAZAR FRECUENCIA DE PALABRAS
barplot(df[1:10,]$freq, las = 2, names.arg = df[1:10,]$word,
        col ="lightblue", main ="PALABRAS MÁS FRECUENTES", ylab = "Frecuencia de palabras")

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

library(wordcloud2)
wordcloud2(df, size=1.2)

wordcloud2(df, size = 0.5, ellipticity = 0.1)

install.packages("ggwordcloud", repos="http://cran.r-project.org", dependencies=T)
library(ggwordcloud)

subdf<-df[df$freq>6,]
set.seed(42)

#Sencillo
ggplot(subdf, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal() 

ggplot(subdf, aes(label = word,
                  size=freq,   
                  color = factor(sample.int(10, nrow(subdf), replace =T)))) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  theme_minimal() 



