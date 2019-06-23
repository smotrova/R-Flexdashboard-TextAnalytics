WordCloudPlot <- function(tdm) {
  library(wordcloud)
  library(tm)
  library(RColorBrewer)
  
  m <- as.matrix(tdm)
  v <- sort(colSums(m),decreasing=TRUE)
  d <- data.frame(Word = names(v),Freq=v)
  
  wordcloud(words = d$Word, freq = d$Freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  }


Top20Words <- function(tdm) {
  library(tm)
  m <- as.matrix(tdm)
  v <- sort(colSums(m),decreasing=TRUE)
  d <- data.frame(Word = names(v),Freq=v, row.names = NULL)
  head(d, 20)
}