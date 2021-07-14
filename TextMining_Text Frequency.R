##### text mining

setwd("c:/Rtest")

install.packages("KoNLP")
install.packages("RColorBrewer")
install.packages("wordcloud")

library(KoNLP)
library(RColorBrewer)
library(wordcloud)

result <- file("tax.txt", encoding="UTF-8")
result2 <- readLines(result)
head(result2, 3)

result3 <- sapply(result2, extractNoun, USE.NAMES=F)
head(unlist(result3), 20)

write(unlist(result3), "tax_word.txt")

myword <- read.table("tax_word.txt")
nrow(myword)

wordcount <- table(myword)
head(sort(wordcount, decreasing=T), 20)

palete <- brewer.pal(9, "Set1")

x11()

wordcloud(
  names(wordcount),
  freq=wordcount,
  scale=c(5, 1),
  rot.per=0.5,
  min.freq=4,
  random.order=F,
  random.color=T,
  colors=palete
  )

result2 <- gsub("ê²?", "", result2)
result2 <- gsub("??€", "", result2)
result2 <- gsub("?›", "", result2)
