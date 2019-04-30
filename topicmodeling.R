#x<- data.frame ("ids" , "titles")
library(tm)
#library(topicmodels) 
#library("Rmpfr")
#library(wordcloud)

#فایل داده های اولیه جهت یادپرفتن تاپیک ها

newsFiles_dir <-"/home/atieh/Desktop/topicmodeling/news"

newsFiles <- list.files(newsFiles_dir , pattern = "*.txt" , full.names = TRUE)

txt_data<- lapply(newsFiles , function( x ) { strs <- read.delim (x , stringsAsFactors = FALSE)
paste(strs , collapse = ' ')})

data<-  data.frame( doc_id = newsFiles ,  text = as.character( unlist( do.call(rbind , txt_data))))

#data[[2]] <- as.character(data[[2]])

#پاکسازی داده ها
data[[2]] <- gsub(" ها" , " " , data[[2]])
data[[2]] <- gsub("'" , " " , data[[2]])
data[[2]] <- gsub("[[:punct:]]" , " " , data[[2]])
data[[2]] <- gsub("[[:cntrl:]]" , " " , data[[2]])
data[[2]] <- gsub("^[[:space:]]+" , "" , data[[2]])
data[[2]] <- gsub("[[:space:]]+$" , "" , data[[2]])


#create corpus object
docs <- Corpus (VectorSource(data[[2]]) ,  readerControl
                = list(language="fa_FAE",encoding = "UTF-8"))


#remove stop words
persianStopwords_file_loc  = "/home/atieh/Desktop/topicmodeling/stopwords.txt"

persianStopwords<- readLines(persianStopwords_file_loc )

docs <- tm_map(docs , removeWords , persianStopwords)

# ایجاد ماتریس ترم -سند
dm <- DocumentTermMatrix(docs,control = list
                         (encoding = 'UTF-8'))

rownames(dm) <- data[[1]]
rowTotals <- apply(dm , 1 , sum)
#حذف سندهایی که خالی هستند 
dtm <- dm [rowTotals>0 , ]

#
#frq <- colSums(as.matrix(dtm))
#ord <- order(frq , decreasing = TRUE)


#پارامترهای مدل
burnin = 1000
iter = 1000 
keep = 50 
k = 10

model <- LDA(dtm , k=k , method = "Gibbs" , control = list(burnin = burnin , iter = iter , keep = keep))


save(model , file ="model.rda")

model.topics <- as.matrix(topics(model))

model.topics

print(terms(model , 30))


#####################------


harmonicMeans <- function(logLikelihoods , precision = 2000L) {
  library(topicmodels) 
  library("Rmpfr")
  llMed <- median (logLikelihoods) 
  as.double( llMed - log(mean(exp(-mpfr(logLikelihoods , prec = precision ) + llMed))))
}


#پارامترهای مدل
burnin = 1000
iter = 1000 
keep = 50 

#مقادیر مختلفی که به عنوان تعداد تاپیک ها در نظر خواهیم گرفت 
sequ <- seq(5 , 20,3 )
#به ازای هر یک از این مقادیر یک مدل ساخه و مدلها را در این لیست نگه می داریم 
fitted_many <- lapply( sequ , function (k) LDA(dtm , k=k , method = "Gibbs" , control = list(burnin = burnin , iter = iter , keep = keep)))



#مقداری که برازندگی را بر اساس آن برای هر مدل محاسبه خواهیم کرد
#extract loglokes from each topic 
logLikes_many <- lapply(fitted_many , function (L) L@logLiks[-c(1:(burnin/keep))] )

#محاسبه برازندگی هر مدل
#compute harmonic means 
hm_many <- sapply(logLikes_many , function(h) harmonicMeans(h) )

#رسم نمودار برازندگی مدلها بر اساسا تعداد تاپیک ها
#inspect 
jpeg("/home/atieh/Desktop/topicmodeling/harmony.jpeg")
plot(sequ , hm_many , type = "l")
dev.off()

# 
#بهترین مدل بر اساس تابع برازندگی
#compute optimum number of topics 
finalModel = fitted_many[which.max(hm_many)][[1]]































#file_loc <- "F:/R/Data/13941102/titles.txt"
#x<- read.csv(file = file_loc , header = FALSE ,  sep = ";" , encoding = "UTF-8" ,stringsAsFactors = FALSE )
#names(x) <- c("doc_id" , "text")
#x<- as.character(x)
#x<- x$V1
#Encoding(x) <- "UTF-8"