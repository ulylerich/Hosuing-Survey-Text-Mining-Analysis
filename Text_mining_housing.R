# read the txt file for goal feedback                                                
readLines("goal.txt")
#collapse the text in one ligne
goal <- paste(readLines("goal.txt"), collapse = " ")
challenges <- paste(readLines("challenges.txt"), collapse = " ")
questions <- paste(readLines("questions.txt"), collapse = " ")
#remove punctuationS from the text and replace with spaces
goal1 <- gsub(pattern = "\\W", replace = " ", goal)
challenges1 <- gsub(pattern = "\\W", replace = " ", challenges)
questions1 <- gsub(pattern = "\\W", replace = " ", questions)
#remove number form the text and replace with spaces
goal1 <- gsub(pattern = "\\d", replace = " ", goal1)
challenges1 <- gsub(pattern = "\\d", replace = " ", challenges1)
questions1 <- gsub(pattern = "\\d", replace = " ", questions1)
#lower all the words in the text
goal1 <- tolower(goal1)
challenges1 <- tolower(challenges1)
questions1 <- tolower(questions1)
#load text mining package
library(tm)
#remove stop words such and,that, and else
goal1 <- removeWords(goal1, stopwords())
challenges1 <- removeWords(challenges1, stopwords())
questions1 <- removeWords(questions1, stopwords())
#check our test
goal1
#remove words of lenght one
goal1 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = "", goal1)
goal1
challenges1 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = "", challenges1)
questions1 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = "", questions1)
#remove whitespace to have single space between words
goal1 <- stripWhitespace(goal1)
goal1
challenges1 <- stripWhitespace(challenges1)
questions1 <- stripWhitespace(questions1)
#Sentiment analysis
library(stringr)
library(wordcloud)
goalbag <- str_split(goal1, pattern = "\\s+")
goalbag
goalbag <- unlist(goalbag)
challenbag <- str_split(challenges1, pattern = "\\s+")
challenbag <- unlist(challenbag)
questionbag <- str_split(questions1, pattern = "\\s+")
questionbag <- unlist(questionbag)
questionbag
#read positive words
posswords <- scan("positive.txt", what = "character", comment.char = ";")
posswords
#match postive words in the documents
pos <- match(goalbag,posswords)
pos
#read negative words
negwords <- scan("Negative.txt", what = "character", comment.char = ";")
negwords
#match negative words
neg <- match(goalbag,negwords)
sum(!is.na(neg))
#wordcloud
library(wordcloud)
wordcloud(goalbag, min.freq = 2, max.words = 300, random.order = F,  colors=brewer.pal(8, "Dark2"))
wordcloud(challenbag, min.freq = 2, max.words = 300, random.order = F,  colors=brewer.pal(8, "Dark2"))
warnings()
#work with corpus of the 2 text
filelist <- list.files(pattern = "*txt")
filelist
#read document
house <- lapply(filelist, FUN = readLines)
#collapse text
corpus <- lapply(house, FUN = paste, collapse = " ")
#remove punctuationS from the text and replace with spaces
corpus1 <- gsub(pattern = "\\W", replace = " ", corpus)
#remove number form the text and replace with spaces
corpus1 <- gsub(pattern = "\\d", replace = " ", corpus1)
#lower all the words in the text
corpus1 <- tolower(corpus1)
#remove stop words such and,that, and else
corpus1 <- removeWords(corpus1, stopwords())
#remove single word
corpus1 <- gsub(pattern ="\\b[A-z]\\b{1}", replace = " ",corpus1)
#remove whitespace to have single space between words
corpus1 <- stripWhitespace(corpus1)
#wordcloud
wordcloud(corpus1, min.freq = 2, max.words = 300, random.order = F,  colors=brewer.pal(8, "Dark2"))
#convert to corpus
corpus2 <- Corpus(VectorSource(corpus1))
#frequency of each term in each corpus
tdm <- TermDocumentMatrix(corpus2)
tdm
#convert to matrix
termhousing <- as.matrix(tdm)
#assign column names
colnames(termhousing) <- c("challenges", "goals")
termhousing
write.csv(termhousing, file = "corpus.csv")
#comparaison cloud
comparison.cloud(termhousing)
comparison.cloud(termhousing,scale = c(4, .5), max.words = 200, random.order = F,  
                 colors=brewer.pal(max (8,ncol(termhousing)), "Dark2"))

#sentiment analysis
bagcorp <- str_split(corpus1,pattern = " ")
bagcorppos <- lapply(bagcorp, function(x) {sum(!is.na(match(x, posswords)))})
score <- lapply(bagcorp, function(x) {sum(!is.na(match(x, posswords))) -
    sum(!is.na(match(x, negwords)))})
score  
score <- unlist(score)                                 
mean(score)
sd(score)
lapply(bagcorp, function(x) {!is.na(match(x, posswords))})
bagcorp

#find words associated with affordable
findAssocs (tdm, terms = "housing", corlimit = 0.3)

#plot top words frequency in corpus
termhousing1 <- sort(rowSums(termhousing), decreasing = T)
termhousing2 <- data.frame(word = names(termhousing1), freq = termhousing1 )
barplot(termhousing2[1:15,]$freq, las = 2, names.arg = termhousing2[1:15,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#words cloud positive and negative words
#change possword data column names to match termhousing for inner joint
pos1 <- as.data.frame(posswords)
colnames(pos1) <- c("word")
#change negwords data column to match termhousing for inner joint
neg1 <- as.data.frame(negwords)
colnames(neg1) <- c("word")
# Innner joint of pos1 and neg1 with corpus "termhousing"
library(dplyr)
posmatch <- inner_join(pos1, termhousing2, by = "word")
negmatch <- inner_join(neg1, termhousing2, by = "word")

#word cloude for positive words in the corpus
wordcloud(words = posmatch$word, freq = posmatch$freq,
          min.freq = 1, random.order = F,  colors=brewer.pal(8, "Dark2"))
#word cloud for negatives words in corpus
wordcloud(words = negmatch$word, freq = negmatch$freq,
          min.freq = 1, random.order = F,  colors=brewer.pal(8, "Dark2"))
#Classify words into categories and graph
library(ggplot2)
library(dplyr)
#group variables and sum
categorysum <- category %>% group_by(Category = sub("\\d+$", "", Category)) %>% summarise(Total = sum(Total))
#Graph with ggplot
ggplot(categorysum) + 
  geom_bar(
    aes(x = reorder(Category, Total), y = Total, fill = Category, group = Category), 
    stat='identity', position = 'dodge') +
  geom_text(aes(reorder(Category,Total), y = Total, label = Total),
            position = position_dodge(width = 0),
            vjust = 0.5, size = 3, hjust = -0.1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,110)) +
  theme(legend.position ="none", axis.text.x = element_text(angle = 48, hjust = 1)) + 
  xlab ("Category") +
  coord_flip()# flip graph
