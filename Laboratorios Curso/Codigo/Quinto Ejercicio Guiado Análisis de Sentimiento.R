#tahap 1
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stringr")

#Sentimiento-Análisis-con-R

#esta vez compartiré sobre el análisis de sentimientos de las reseñas o comentarios de los clientes. 
#Los datos utilizados son los datos de revisión de Lion Air que se han descargado en la publicación anterior.

#Para obtener los resultados del análisis, por supuesto, hay etapas que no puedo explicar aquí en detalle. 
#Sin embargo, con gusto lo ayudaré si necesita una explicación y compartir sobre los pasos que se deben realizar. puedes contactarme en www.linkedin.com/in/spryd

#La conclusión de este análisis se puede ver en las imágenes de sentimientos negativos, neutrales y positivos. 
#En general, muchas críticas negativas hablan de retrasos en las aerolíneas Lion Air. mientras que lo positivo se relaciona con "bueno" y "tiempo" 
#para ver qué se relaciona con estas 2 palabras, se puede ver en la asociación de palabras en las críticas positivas.


setwd("~/Desktop/Laboratorios Curso/DATOS/Sentiment-Analysis-with-R-master/")

docs<-readLines("Datalion.csv")

# Load the data as a corpus
docs <- Corpus(VectorSource(docs))

#Inspect the content of the document
inspect(docs)

#Replacing "/", "@" and "|" with space:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Cleaning the text
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

#Remove punctuation
docs <- tm_map(docs, toSpace, "[[:punct:]]")

#Remove numbers
docs <- tm_map(docs, toSpace, "[[:digit:]]")

# add two extra stop words: "available" and "via"
myStopwords = readLines("stopword_en.csv")

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
head(d, 15)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


dataframe<-data.frame(text=unlist(sapply(docs, `[`)), stringsAsFactors=F)

write.csv(dataframe, "~/Desktop/Laboratorios Curso/DATOS/Sentiment-Analysis-with-R-master/lion2.csv")
save.image()




