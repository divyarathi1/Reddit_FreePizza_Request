library(jsonlite)
library(ggplot2)
install.packages("reshape2")
library(reshape2)
#open the json file as a dataframe#
pizza <- fromJSON("~/Desktop/pizza_request_dataset.json")

#add column with 1/0 (yes pizza/ no pizza)
pizza$requester_received_pizza <- ifelse(pizza$requester_received_pizza ==TRUE, 1, 0)

#subset into pizza or not
YESpizza <- subset(pizza, pizza$requester_received_pizza == 1)
NOpizza <- subset(pizza, pizza$requester_received_pizza == 0)

-----------------------------------------------------------
summary(YESpizza$requester_account_age_in_days_at_request)
summary(NOpizza$requester_account_age_in_days_at_request)

##significantly different account age
t.test(YESpizza$requester_account_age_in_days_at_request,  NOpizza$requester_account_age_in_days_at_request, alternative = "greater")

boxplot(YESpizza$requester_account_age_in_days_at_request,  NOpizza$requester_account_age_in_days_at_request, xlab='Pizza Request Result', ylab='Account Age at Request (Days)')

ggplot(aes(y=requester_account_age_in_days_at_request, x =requester_received_pizza, group = requester_received_pizza, fill=requester_received_pizza), data = pizza) + 
	geom_boxplot() + ylab("Account Age at Request (Days)")+
	scale_x_discrete("Pizza Request Result", limits=c(0,1), 							labels=c("0"='No Pizza',"1"='Received')) +
	guides(fill=FALSE)


summary(YESpizza$requester_days_since_first_post_on_raop_at_request)
summary(NOpizza$requester_days_since_first_post_on_raop_at_request)
t.test(YESpizza$requester_days_since_first_post_on_raop_at_request,NOpizza$requester_days_since_first_post_on_raop_at_request)
table(pizza$requester_username)
-----------------------------------------------------------

-----------------------------------------------------------
#KARMA POINTS

summary(YESpizza$requester_upvotes_minus_downvotes_at_request)
summary(YESpizza$requester_upvotes_minus_downvotes_at_retrieval)

t.test(YESpizza$requester_upvotes_minus_downvotes_at_request, NOpizza$requester_upvotes_minus_downvotes_at_request)

t.test(YESpizza$requester_upvotes_minus_downvotes_at_retrieval, NOpizza$requester_upvotes_minus_downvotes_at_retrieval)



------------------------------------------------------------
#look at sentiments of the posts

library(syuzhet)

#get just the titles 
title<- pizza$request_title
class(title)

requesttext<- pizza$request_text
request
------------------------------------------------------------
#word count 
wordcountTitle<- sapply(gregexpr("[[:alpha:]]+",title),length)+1
pizza$wordcountTitle<- sapply(gregexpr("[[:alpha:]]+",title),length)+1
summary(wordcountTitle)

#create boxplot
ggplot(aes(y=wordcountTitle, x=requester_received_pizza, group = requester_received_pizza, fill=requester_received_pizza), data=pizza) + geom_boxplot() + ylab("Title Word Count")+
	scale_x_discrete("Pizza Request Result", limits=c(0,1), 							labels=c("0"='No Pizza',"1"='Received')) +
	guides(fill=FALSE)
	
#request word count	
pizza$wordcountRequest<- sapply(gregexpr("[[:alpha:]]+", requesttext),length)+1

#boxplot
ggplot(aes(y=wordcountRequest, x=requester_received_pizza, group = requester_received_pizza, fill=requester_received_pizza), data=pizza) + geom_boxplot() + ylab("Request Word Count")+
	scale_x_discrete("Pizza Request Result", limits=c(0,1), 							labels=c("0"='No Pizza',"1"='Received')) +
	guides(fill=FALSE)
	
t.test(pizza$wordcountTitle~pizza$requester_received_pizza)



alltypes<- melt(WC, variable.name="Type", value.name="Count")
all2<- cbind(alltypes, pizza$requester_received_pizza)

head(all2)
colnames(all2) = c("Type", "Count","PizzaResult")

ggplot(all2)+ geom_boxplot(aes(x=PizzaResult, y=Count, group=Type))
+facet_grid(Type~.)


boxplot(pizza$wordcountRequest~pizza$requester_received_pizza)

reg<-lm(requester_received_pizza~pizza$wordcountRequest*wordcountTitle, data=pizza)
summary(reg)

titleRequestWC<-melt(data.frame(pizza$wordcountRequest,pizza$wordcountTitle, pizza$requester_received_pizza), variable.name="Type", value.name="WordCount")
fullWC<- cbind(titleRequestWC, pizza$requester_received_pizza)

t.test(pizza$wordcountRequest ~ pizza$requester_received_pizza)

t.test(pizza$wordcountTitle ~ pizza$requester_received_pizza)

----------------------------------------------------------
# Posivity and Sentiment 
install.packages("syuzhet")
library(syuzhet)

sentiment<- get_nrc_sentiment (pizza$request_text)
anger <- sentiment[,"anger"]
anticipation <- sentiment[,"anticipation"]
disgust <- sentiment[,"disgust"]
fear <- sentiment[,"fear"]
joy <- sentiment[,"joy"]
sadness <- sentiment[,"sadness"]
surprise <- sentiment[,"surprise"]
trust <- sentiment[,"trust"]
negative <- sentiment[,"negative"]
positive <- sentiment[,"positive"]

sentimentCount <- data.frame(pizza$requester_received_pizza, anger, anticipation,disgust,fear, joy, sadness, surprise, trust, negative, positive)

sentimentCount1 <- data.frame(anger, anticipation,disgust,fear, joy, sadness, surprise, trust, negative, positive)
head(sentimentCount1)
sent<- melt(sentimentCount1, variable.name="emotion", value.name="count")
data.frame(sent)
sentAgg<-aggregate(count~emotion, data=sent, FUN=sum)

ggplot(sentAgg,aes(factor(emotion),count,fill=emotion))+ 
	geom_bar(stat="identity")+
	xlab("Sentiment")+
	ylab("Count")+
	scale_fill_manual(values=c("lightsteelblue", "lightsteelblue", 	"lightsteelblue","lightsteelblue","lightseagreen","lightsteelblue","lightseagreen","lightseagreen","lightsteelblue","lightseagreen"))+
	guides(fill=FALSE)+
	theme(axis.text.x=element_text(size=12, angle=-45))



-----------------------------------------------
#try to look for strings
money<- c("week", "ramen", "paycheck", "work", "couple" ,"rice" ,"check", "pizza", "grocery", "rent" ,"anyone", "favor", "someone", "bill", "money", "food", "money", "house", "bill", "rent", "stamp" ,"month", "today", "parent", "help", "pizza", "someone", "anything", "mom", "anyone")
sum(stri_detect_fixed(pizza$request_text[1],money))
situations<- data.frame()
situations$money<- length(which(stri_detect_fixed(pizza$request_text,money)>=1))
nrow(pizza)