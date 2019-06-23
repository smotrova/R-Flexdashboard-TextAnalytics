# Term Document Matrix

TermMatrix <- function(TextData) {
  # TextData is a character vector
  library(tm)
  
  # create a collection of documents 
  # (technically referred to as a Corpus) in the R environment.
  
    corpus = Corpus(VectorSource(TextData))
  
  # Convert to lower case
    corpus = tm_map(corpus, content_transformer(tolower))
    
  # Replacing “/”, “@” and “|” with space:
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    corpus <- tm_map(corpus, toSpace, "/")
    corpus <- tm_map(corpus, toSpace, "@")
    corpus <- tm_map(corpus, toSpace, "\\|")
    
  # Remove numbers
    corpus <- tm_map(corpus, removeNumbers)
  
  # Remove english common stopwords
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
  # Remove punctuation
    corpus = tm_map(corpus, removePunctuation)
    
  # eliminate extra white spaces
    corpus <- tm_map(corpus, stripWhitespace)
  
  # Stem document 
    corpus = tm_map(corpus, stemDocument)
  
  # create term-document matrix
    dtm = DocumentTermMatrix(corpus)
    
    return(dtm)
}

TextDataFrame <- function(dtm) {
  library(tm)
  
  # dtm is a term document matrix
  
  # rows = number of documents
  # columns = number of words in vocabulary 
  # inspect(dtm)

  # Remove sparse terms
  # i.e., terms occurring only in very few documents.
  sparse = removeSparseTerms(dtm, 0.95)
  
  # Convert to a data frame
  textDF = as.data.frame(as.matrix(sparse))
  
  # Make all variable names R-friendly
  colnames(textDF) = make.names(colnames(textDF))

  return(textDF)
}


