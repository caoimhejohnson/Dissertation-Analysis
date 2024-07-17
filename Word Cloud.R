install.packages("wordcloud")
install.packages("tm")
install.packages("readxl")
install.packages("tokenizers")

library(wordcloud)
library(readr)
library(readxl)
library(tokenizers)
library(tm)

wordcloud.df <- read_xlsx("Desktop/Masters/Dissertation/Textual Data (Feelings).xlsx")
words <- wordcloud.df$Feelings
word_counts <- table(words)
print(word_counts)

colours <- brewer.pal(n=6, name = "PuBu")

wordcloud <- wordcloud(words = names(word_counts),
          freq = word_counts,
          min.freq = 3,
          scale = c(5,0.5),
          colors = colours)














