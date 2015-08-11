setwd("C:/Users/yuki.katoh/TASKS/LDAtwitter")
library(RMongo)
library(RJSONIO)
library(tm)
library(wordcloud)
library(reshape2)
library(topicmodels)
library(LDA)
mongo <- mongoDbConnect("followersids")
dbShowCollections(mongo)
cm <- 'TSO_UK'
qr <- dbGetQuery(mongo, cm, '{}', 0, 3200)
save(qr,file='qr.RData')
lg <- length(qr$res)

func <- function (x) {
  return(tryCatch(fromJSON(qr$res[x]), error=function(e) NULL))
}
raw <- mapply(func,1:lg)

text <- data.frame(as.character(mapply(function(x){raw[[x]][[y]]$text},2:lg)))
names(text) <- 'text'

textFun <- function(x,y){
  tryCatch(
  if(length(raw[[x]])==0){
  return("NA")  
  }else{
  return(raw[[x]][[y]]$text)
},error=function(e) NA)}

a <- matrix(NA,lg,100)
for(i in 2:lg){
  lg2 <- length(raw[[i]])
  for(j in 1:lg2){
 a[i,j]=textFun(i,j)
 }}

a <- data.frame(a)
a$ind <- 1:nrow(a)
texts <- melt(a,'ind')
names(texts)[3] <- 'text'

idFun<- function(x,y){
  tryCatch(
    if(length(raw[[x]])==0){
      return("NA")  
    }else{
      return(raw[[x]][[y]]$user$id)
    },error=function(e) NA)}

a <- matrix(NA,lg,100)
for(i in 2:lg){
  lg2 <- length(raw[[i]])
  for(j in 1:lg2){
    a[i,j]=idFun(i,j)
  }}

a <- data.frame(a)
a$ind <- 1:nrow(a)
ids <- melt(a,'ind')
names(ids)[3] <- 'id'


langFun<- function(x,y){
  tryCatch(
    if(length(raw[[x]])==0){
      return("NA")  
    }else{
      return(raw[[x]][[y]]$lang)
    },error=function(e) NA)}

a <- matrix(NA,lg,100)
for(i in 2:lg){
  lg2 <- length(raw[[i]])
  for(j in 1:lg2){
    a[i,j]=langFun(i,j)
  }}

a <- data.frame(a)
a$ind <- 1:nrow(a)
lang <- melt(a,'ind')
names(lang)[3] <- 'lang'

df <- cbind(texts,ids,lang)
df <- na.omit(df[c(1,3,6,9)])
df <- df[df$lang=='en',]
save(df,file='df.RData')


myCorpus <- Corpus(VectorSource(df$text))
removeurl <- function(x)gsub("http[[:alnum:]]*","",x)
removeurl <- function(x)gsub("http.*","",x)
myCorpus <- tm_map(myCorpus,removeurl)
removeamp <- function(x)gsub("&amp;","",x)
myCorpus <- tm_map(myCorpus,removeamp)
myCorpus <- tm_map(myCorpus, tolower)

myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myDtm <- DocumentTermMatrix(myCorpus)
# myDtm$dimnames$Docs <- df$text
rowTotals <- apply(myDtm , 1, sum)
save(rowTotals,file='rowTotals.RData')
dtm.new   <- myDtm[rowTotals> 0,]
# dtm.new$dimnames$Docs <- df[rowTotals>0,]$text
save(dtm.new,file='dtm.new.RData')

set.seed(123)
lda <- LDA(dtm.new,15)
terms(lda)
# head(topics(lda),10)
res <- data.frame(df[rowTotals>0,]$text,topics(lda))
names(res) <- c('Tweet','Topic')
head(res[res$Topic==4,],40)
