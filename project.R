mergeFiles <- function() {
  # if (!file.exists("Coursera-SwiftKey/final/en_US/mergedfile.txt")) {
  conn <- file("Coursera-SwiftKey/final/en_US/mergedfile.txt", "r")
  fulltext <- readLines(conn, encoding = "UTF-8", skipNul = TRUE)
  nlines <- length(fulltext)
  close(conn)
  
  conn <- file("Coursera-SwiftKey/final/en_US/sampledfile.txt", "w")
  selection <- rbinom(nlines, 1, .1)
  for (i in 1:nlines) {
    if (selection[i] == 1) {
      cat(fulltext[i], file = conn, sep = "\n")
    }
  }
  close(conn)
  paste(
    "Saved",
    sum(selection),
    "lines to file",
    "Coursera-SwiftKey/final/en_US/sampledfile.txt"
  )
  # }
}

makeCorpus <- function() {
  mytf3 <-
    readLines("Coursera-SwiftKey/final/en_US/sampledfile.txt",
              encoding = "UTF-8", skipNul = TRUE)
  myCorpus <- corpus(mytf3)
}

makeSentences <- function(filecorpus) {
  sentences <-
    tokens(
      filecorpus,
      what = "sentence",
      remove_punct = TRUE,
      remove_twitter = TRUE
    )
  sentences <-  tokens_remove(sentences, getProfanities())
  unlist(lapply(sentences, function(a)
    char_tolower(a)))
}

makeNgrams <- function(sentences, n = 1L) {
  words <-
    tokens(
      sentences,
      ngrams = n,
      remove_url = TRUE,
      remove_separators = TRUE,
      remove_punct = TRUE,
      remove_twitter = TRUE,
      what = "word",
      remove_hyphens = TRUE,
      remove_numbers = TRUE
    )
  words <-  tokens_remove(words, getProfanities())
}

getProfanities <- function() {
  profanityFile <- "profanities.txt"
  if (!file.exists(profanityFile)) {
    download.file('http://www.cs.cmu.edu/~biglou/resources/bad-words.txt',
                  profanityFile)
  }
  profanities <-
    read.csv("profanities.txt",
             header = FALSE,
             stringsAsFactors = FALSE)
  profanities$V1
}

createnGramLookupTable <- function(ngram) {
  tokenList <-  unlist(ngram, recursive = FALSE, use.names = FALSE)
  wordFreq <- table(tokenList)
  nGramTable <- as.data.frame(wordFreq)
  colnames(nGramTable) <- c("nGrams", "Frequency")
  nGramTable$nGrams <- as.character(nGramTable$nGrams)
  nGramTable$nGrams <- gsub("^_*|_*$|,*", "", nGramTable$nGrams)
  nGramTable$nGrams <- gsub("_+", "_", nGramTable$nGrams)
  nGramTable <-
    mutate(nGramTable, Pred = sapply(strsplit(nGramTable$nGrams, split = '_', fixed =
                                                TRUE), function(x)
                                                  (tail(unlist(x), 1))))
  nGramTable <-
    mutate(nGramTable, nGrams = sapply(strsplit(nGramTable$nGrams, '_', fixed = TRUE), function(x)
      (paste(head(unlist(
        x
      ),-1), collapse = "_"))))
  
  nGramTable %>%
    group_by(nGrams) %>%
    top_n(n = 3, wt = Frequency)
}
main <- function() {
  library(quanteda)
  require(readtext)
  library(sqldf)
  library(dplyr)
  set.seed(1288)
  mergeFiles()
  fileCorpus <- makeCorpus()
  sentences <- makeSentences(fileCorpus)
  twoGrams <- makeNgrams(sentences, 2)
  twoGramTable <- createnGramLookupTable(twoGrams)
  write.table(
    twoGramTable,
    "twoGramTable.csv",
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  twoGrams <- NULL
  threeGrams <- makeNgrams(sentences, 3)
  threeGramTable <- createnGramLookupTable(threeGrams)
  write.table(
    threeGramTable,
    "threeGramTable.csv",
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  threeGrams <- NULL
  fourGrams <- makeNgrams(sentences, 4)
  fourGramTable <- createnGramLookupTable(fourGrams)
  write.table(
    fourGramTable,
    "fourGramTable.csv",
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  fourGrams <- NULL
  fiveGrams <- makeNgrams(sentences, 5)
  fiveGramTable <- createnGramLookupTable(fiveGrams)
  write.table(
    fiveGramTable,
    "fiveGramTable.csv",
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  fiveGrams <- NULL
}