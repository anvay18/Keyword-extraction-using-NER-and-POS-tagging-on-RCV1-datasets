library(RXKCD);
library(tm);
library(SnowballC);
library(wordcloud)
library(RColorBrewer);
library(Rcpp)
getwd()
a<-Corpus(DirSource("D:/Master Of Analytics Assessments/Text mining/Assignment 2 files/Assignment 2 2019 dataset CCAT/CCAT"),
          readerControl = list(language="en")
#specifies the exact folder where my text file(s) is for analysis with tm.
class(a)
x <- DirSource("D:/Master Of Analytics Assessments/Text mining/Assignment 2 files/Assignment 2 2019 dataset CCAT/CCAT")
#input path for documents
org_name = read.csv("Documents/organization.csv", header
                    = FALSE)
YourCorpus <- Corpus(x,
                     readerControl=list(reader=readPlain))
#load in documents
summary(YourCorpus)
#check what went in your corpus
docs <- Corpus(DirSource("D:/Master Of Analytics Assessments/Text mining/Assignment 2 files/Assignment 2 2019 dataset CCAT/CCAT"))
#load in documents
docs
summary(docs)
inspect(docs[1])
# get rid of html tags
pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'"
for (j in seq(docs))
{
  docs[[j]] <- gsub("-", " ", docs[[j]])
  docs[[j]] <- gsub("@", "", docs[[j]])
  docs[[j]] <- gsub("nn|", "", docs[[j]])
  docs[[j]] <- gsub("pattern", "", docs[[j]])
  
}
inspect(docs[2])
###################
#Remove punctuation
###################
docs <- tm_map(docs, removePunctuation)
inspect(docs[1])
#########################
#Remove English Stopwords
#########################
length(stopwords("english"))
stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs[1])
##################
#Strip whitespaces
##################
docs <- tm_map(docs, stripWhitespace)
inspect(docs[1])
################
#Remove numbers
################
docs <- tm_map(docs, removeNumbers)
inspect(docs[1])
###########################
#conversion to lower case
##########################
docs <- tm_map(docs, tolower)
inspect(docs[1])
###########################
#bad word list downloaded #
##########################
bads = readLines("Downloads/Terms-to-Block.csv")
docs <- tm_map(docs, removeWords, bads)
###############################################
# remove single letter and extar white spaces #
##############################################
docs <- tm_map(docs, removeWords, letters)
##########################
#remove non-relevant words
##########################
docs <- tm_map(docs, removeWords, c("per cent", "email",
                                    "NA"))
inspect(docs[1])
docs
#########################
#Part-of-speech tagging
########################
library(NLP)
library(openNLP)v
library("tm")
library("SnowballC")
library("RWeka")
library("wordcloud")
library("reshape2")
library("ggplot2")
library(rJava)
library(magrittr)
library(dplyr)
s <- as.String(docs)
## Need sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <-
  Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- NLP::annotate(s, list(sent_token_annotator,
                            word_token_annotator, pos_tag_annotator))
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tagtb <- table(tags)
summary(tagtb)
summary(a3ws)
a3ws <- annotations_in_spans(subset(a3, type == "word"),
                             subset(a3, type == "sentence")[3L])[[1L]]
## Determine the distribution of POS tags for word tokens.
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)
pof <- data.frame(table(tags))
colnames(pof) <- c("Tag", "Freq")
pof <- pof[order(pof$Freq, decreasing = TRUE),]
pof <- transform(pof, Tag = reorder(Tag, order(Freq,
                                               decreasing = TRUE)))
ggplot(pof, aes(x = reorder(Tag, -Freq), y = Freq, fill=Tag))
+ geom_bar(stat="identity")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5, colour = "gray"))
t3 <- NLP::annotate(s, list(sent_token_annotator,
                            word_token_annotator))
head(t3)
t3_doc <- AnnotatedPlainTextDocument(s, t3)
words(t3_doc) %>% head(10)
org <- Maxent_Entity_Annotator(kind = "organization")
t4 <- NLP::annotate(s, list(sent_token_annotator,
                            word_token_annotator, org))
t4_doc <- AnnotatedPlainTextDocument(s, t4)
library(ggplot2)
ggplot(data, aes(x = reorder(V1, -V2), y = V2, fill=V1)) +
  geom_bar(stat="identity")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5, colour = "gray"))+
  ylab("Count") +
  guides(fill=guide_legend(title="Organigation")) +
  scale_fill_brewer(palette="Set3")