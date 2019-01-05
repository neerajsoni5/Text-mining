#STEP1-Text extraction & creating a corpus

install.packages("ggthemes")
install.packages("qdap")
install.packages("wordcloud")
install.packages("plotrix")
install.packages("quanteda")
install.packages("RWeka")
install.packages("dendexten")


library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)
library(SnowballC)

setwd("C:/Users/SoniNe02/Downloads/retail")
review=read.csv("retail.csv", stringsAsFactors = FALSE)

names(review)

## Make a vector source and a corpus
corpus_review=Corpus(VectorSource(review$Review.Text))

#1.Convert to lower case
#STEP2-Text Pre-processing
corpus_review=tm_map(corpus_review, tolower)

#2. Remove punctuation:
corpus_review=tm_map(corpus_review, removePunctuation)

#3.Remove stopwords
#"stopwords" is a very important concept to understand while doing text mining. 
#When we write, the text generally consists of a large number of prepositions, pronouns, conjunctions etc. 
#These words need to be removed before we analyse the text
#stopwords("en")     #we can view with this command

#Remove stopwords
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))

#We might also want to remove custom stopwords based on the context of the text mining

# Remove context specific stop words
corpus_review=tm_map(corpus_review, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))


##Stemming a document
#In linguistics, stemming is the process of reducing inflected (or derived) words to their word stem, 
#base or root form-generally a written word form
#The SnowballC package is used for document stemming. 
#For example "complicated", "complication" and "complicate" will be reduced to "complicat" after stemming.

## Stem document
corpus_review=tm_map(corpus_review, stemDocument)

##Viewing the corpus content
corpus_review[[1]][1]

#The corpus object in R is a nested list. We can use the r syntax for lists to view contents of the corpus.

#1Frequently used words
#We now have a text corpus which is cleaned and only contains the core words required for text mining.
#The next step is exploratory analysis.
# identify the most frequently used words in the overall review text.

# Find the 20 most frequent terms: term_count
term_count <- freq_terms(corpus_review, 20)

# Plot 20 most frequent terms
plot(term_count)

#STEP3Create the DTM & TDM from the corpus
#The pre-processed and cleaned up corpus is converted into a matrix called the document term matrix.
#A document-term matrix is a mathematical matrix that describes the frequency of terms that occur in a collection of documents.
#In a document-term matrix, rows correspond to documents in the collection and columns correspond to terms.
#The term-document matrix is a transpose of the document-term matrix. 
#It is generally used for language analysis. An easy way to start analyzing the information is to change the DTM/TDM 
#into a simple matrix using 


review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

##Using the TDM to identify frequent terms
# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]


#STEP4???-???Exploratory text analysis
# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:20], col = "steel blue", las = 2)

#Word clouds
#Word cloud is a common way of visualizing a text corpus to understand the frequently used words. 
#The word cloud varies the size of the words based on the frequency.

review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)

# Create a wordcloud for the values in word_freqs
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = "red")

# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))

#Comparison of corpus
#One of the main objectives of this study is to analyse the difference in keywords
#between those who recommend and those who don't recommend the product.
#For this purpose, we will create 2 corpora???-???one for Recommend-yes and another for Recommend-no

## Combine both corpora: all reviews
review_yes1<-review[which(review$Recommended.IND==1),]
review_no1<-review[which(review$Recommended.IND==0),]
review_yes<-review_yes1$Review.Text
review_no<-review_no1$Review.Text
all_yes <- paste(review_yes, collapse = "")
all_no <- paste(review_no, collapse = "")
all_combine <- c(all_yes, all_no)

#######Approach 2

## Creating corpus for combination
corpus_review_all=Corpus(VectorSource(all_combine)) 
## Pre-processing corpus - all
#Convert to lower-case
corpus_review_all=tm_map(corpus_review_all, tolower)
#Remove punctuation
corpus_review_all=tm_map(corpus_review_all, removePunctuation)
#Remove stopwords
corpus_review_all=tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all=tm_map(corpus_review_all, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress","just","i"))
#Stem document
corpus_review_all=tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Yes","No")
#Sum rows and frequency data frame
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)
#Make commonality cloud
commonality.cloud(all_m, 
                  colors = "steelblue1",
                  max.words = 50)


# Create comparison cloud
comparison.cloud(all_m,
                 colors = c("green", "red"),
                 max.words = 50)


#The comparison cloud gives a clear contrast of words used by people who are happy with the product compared to those who are not. 
#The people who have not recommended the product have used negative words like disappoint, return, cheap, look etc.

#######Approach 3
##Polarized tag plot
#A polarized tag plot is an improved version of the commonality cloud. 
#It determines the frequency of a term used in both the corpora under comparison. 
#The difference in frequencies of common words might be insightful in many cases.

# Identify terms shared by both documents
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
# calculate common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)


top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))
# Make pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 2000,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Yes",
                            "Words",
                            "No")
)



####Approach 4
##Simple word clustering
#Word clustering is used to identify word groups used together, based on frequency distance. 
#This is a dimension reduction technique. It helps in grouping words into related clusters. Word clusters are visualized with dendrograms.

review_tdm2 <- removeSparseTerms(review_tdm, sparse = 0.9)
hc <- hclust(d = dist(review_tdm2, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)


##The cluster dendogram shows how certain words are grouped together.
#For example, "soft", "material" & "comfort" have been used together. Since the clustering is based on the frequency distances,
#the cluster indicates which set of words are used together most frequently.

