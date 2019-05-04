##### load 'tm' package #####
library(tm)


##### read news files and convert them into a dataframe #####
newsFiles_dir <-"./data"
newsFiles <- list.files(newsFiles_dir , pattern = "*.txt" , full.names = TRUE)

txt_data<- lapply(newsFiles , function( x ) { strs <- read.delim (x , stringsAsFactors = FALSE)
paste(strs , collapse = ' ')})
data<-  data.frame( doc_id = newsFiles ,  text = as.character( unlist( do.call(rbind , txt_data))))

##### remove punctuations and control characters #####
data[[2]] <- gsub(" ها" , " " , data[[2]])
data[[2]] <- gsub("'" , " " , data[[2]])
data[[2]] <- gsub("[[:punct:]]" , " " , data[[2]])
data[[2]] <- gsub("[[:cntrl:]]" , " " , data[[2]])
data[[2]] <- gsub("^[[:space:]]+" , "" , data[[2]])
data[[2]] <- gsub("[[:space:]]+$" , "" , data[[2]])

##### create corpus object #####
docs <- Corpus (VectorSource(data[[2]]) ,  readerControl
                = list(language="fa_FAE",encoding = "UTF-8"))

##### remove stop words #####
persianStopwords_file_loc  = "/home/atieh/Desktop/topicmodeling/stopwords.txt"
persianStopwords<- readLines(persianStopwords_file_loc )

docs <- tm_map(docs , removeWords , persianStopwords)

##### create doctment-term matrix #####
dm <- DocumentTermMatrix(docs,control = list
                         (encoding = 'UTF-8'))
rownames(dm) <- data[[1]]

##### remove empty documents , if any #####
rowTotals <- apply(dm , 1 , sum)
dtm <- dm [rowTotals>0 , ]


##### load topicmodels library #####
library(topicmodels)

#model paramaters
burnin = 1000
iter = 1000 
keep = 50 
k = 10

##### train topic models #####
model <- LDA(dtm , k=k , method = "Gibbs" , control = list(burnin = burnin , iter = iter , keep = keep))


##### topics assigned to each document #####
model.topics <- as.matrix(topics(model))
model.topics

##### how many documents in each topic #####
table(model.topics)

##### print top 5 important words in each topic #####
print(terms(model , 30))


##### function to plot wordcloud for a given topic #####
plotWordCloud<- function(t){
  m<- as.matrix(dtm[rownames(model.topics)[which(model.topics==t)], ])
  require(wordcloud)
  v<- sort(colSums(m) , decreasing = TRUE)
  words<- names(v)
  d<- data.frame(words= words , frq=v)
  dark2<- brewer.pal(6,"Dark2")
  wordcloud(d$words , d$frq , min.freq = 2 , colors = dark2)
  
}

##### plot wordcloud of two topics and compare them! #####
par(mfrow=c(1,2))

plotWordCloud(1)
plotWordCloud(10)