# install.packages("wordcloud")
library(wordcloud)

generate.word.cloud <- function(input_text, number_of_elements) {
    # this function gets some text and generates a wordcloud of the most frequent words used
    wordcloud_rawstring <- paste(input_text, collapse = " ")
    wordcloud_source <- VectorSource(wordcloud_rawstring)
    wordcloud_corpus <- Corpus(wordcloud_source)
    wordcloud_corpus <- tm_map(wordcloud_corpus, content_transformer(tolower)) # lower case everything
    wordcloud_corpus <- tm_map(wordcloud_corpus, removePunctuation) # remove punctioation 
    wordcloud_corpus <- tm_map(wordcloud_corpus, stripWhitespace) # strip whitespace
    wordcloud_corpus <- tm_map(wordcloud_corpus, removeWords, stopwords("english")) # common words w/o any "content"
    wordcloud_dtm <- DocumentTermMatrix(wordcloud_corpus) # create document term matrix
    dtm2 <- as.matrix(wordcloud_dtm)
    frequency <- colSums(dtm2)
    frequency <- sort(frequency, decreasing = T)

    print(head(frequency, n = 50))
    words <- names(frequency)
    wordcloud(words[1:number_of_elements], frequency[1:number_of_elements])
}

remove.bond.issuers <- function(keywords, bond_issuers) {
    # this function removes all entries from the bond_issuer df based on keyword detection
    for (i in 1:length(keywords)) {
        keyword <- as.character(keywords[i])
        bond_issuers <- bond_issuers[!grepl(keyword, bond_issuers$iq_issuer),]
    }
    return(bond_issuers)
}


# TODO: Function, that reads packages & co
