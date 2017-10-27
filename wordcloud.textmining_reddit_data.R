

#packages reqd for text mining and word cloud:
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

library(jsonlite)

pizza <- fromJSON("~/Desktop/pizza_request_dataset.json")
class(pizza)

#create a varible to store my filepath
filePath <- ("~/Desktop/request_text1.txt")
#create a character vector containing all texts, had created a text file"reuqest_text1"containing all request texts in one place from excel
text <- readLines(filePath) 


#above imports the text file into a vector, below #breaks the vector to form one single element
onetext <- paste(text,collapse=" ")

#textmining:

#creating a corpus which is sort of a container/vector in the world of text in R reqd for data mining:
docs <- Corpus(VectorSource(onetext))
inspect(docs)


#creating a function for cleaning text operations:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

#TEXT CLEANING:
#Transformation is performed using tm_map() function to replace, for example, special characters from the text. Replacing “/”, “@” and “|” with space:

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)

#building a term-document matrix:

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 20)
docs <- tm_map(docs, removeWords, c("pizza", "get","just","will","can","week","day","now","like","time","ive","make"))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,20)

 word freq
pizza   pizza 4976
get       get 3073
just     just 2585
help     help 2254
will     will 2042
can       can 1936
realli realli 1770
work     work 1742
week     week 1725
food     food 1694
day       day 1623
money   money 1593
pay       pay 1549
thank   thank 1500
now       now 1375
like     like 1287
job       job 1276
time     time 1218
ive       ive 1143
make     make 1114

#remove words as shown above that dont make sense in our analysis:

> docs <- tm_map(docs, removeWords, c("pizza", "get","just","will","can","week","day","now","like","time","ive","make"))

#again:
> dtm <- TermDocumentMatrix(docs)
> m <- as.matrix(dtm)
> v <- sort(rowSums(m),decreasing=TRUE)
> d <- data.frame(word = names(v),freq=v)
> head(d,20)

         word freq
help     help 2254
realli realli 1770
work     work 1742
food     food 1694
money   money 1593
pay       pay 1549
thank   thank 1500
job       job 1276
love     love 1087
last     last 1050
one       one 1038
eat       eat 1016
month   month  988
want     want  976
much     much  973
someon someon  968
live     live  942
got       got  925
back     back  899
know     know  892

#could remove more words:

> docs <- tm_map(docs, removeWords, c("know", "one","much","someon","got","last"))
> dtm <- TermDocumentMatrix(docs)
> m <- as.matrix(dtm)
> v <- sort(rowSums(m),decreasing=TRUE)
> d <- data.frame(word = names(v),freq=v)
> head(d,20)

           word freq
help       help 2254
realli   realli 1770
work       work 1742
food       food 1694
money     money 1593
pay         pay 1549
thank     thank 1500
job         job 1276
love       love 1087
eat         eat 1016
month     month  988
want       want  976
live       live  942
back       back  899
need       need  884
anyon     anyon  879
appreci appreci  797
tri         tri  766
paid       paid  762
forward forward  757

# now we have more sensible words which predict pizza request fulfillment


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
 
#creating word cloud:

        
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) set.seed(1234)
          
#final_wordcloud after changes:          
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
          
#Barplot plotting frequencies:

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
       


