library(XML)
library(stringr)
library(RCurl)
library(slam)
library(NLP)
library(tm)
library(SnowballC)
library(RWeka)
library(RTextTools)


#started around halloween and nick cave has created some very scary music in his time, so seemed appropos
seedURL <-"http://www.allmusic.com/artist/nick-cave-mn0000397880"
setwd("L:\\school\\cuny\\607\\hw10")

getRootHTML <- function(artistURL)
{
    html <- getURL(artistURL,
                   useragent = str_c(R.version$platform, 
                                     R.version$version.string,
                                     set=", "))
    return(xmlRoot(htmlParse(html)))
}

getArtistRepository <- function(artistURL)
{
    artistRootHTML <- getRootHTML(str_c(artistURL, "/discography"))
    
    artistName <- xpathApply(artistRootHTML,'//*[@id="cmn_wrap"]/div[1]/div[2]/header/div/hgroup/h1', xmlValue )
    artistName <- str_replace_all(artistName, "\n +|  +", "")
    #header <- xpathSApply(artistRootHTML, "//table/tr/th", xmlValue)
    
    #get album names
    albums <- xpathSApply(artistRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section/table/tbody/tr/td[4]/a[1]', xmlValue)
    
    #reviews is first attribute
    reviewLinks <- xpathSApply(artistRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section/table/tbody/tr/td[4]/a[1]', xmlAttrs)[1,]
    
    reviews <- lapply(reviewLinks ,getReviewText)
    
    #rating is 2nd attr of this section
    ratings <- xpathSApply(artistRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section/table/tbody/tr/td[6]', xmlAttrs)[2,]
    #ratings is first numeric value, and is from 0-9, so add 1 to make 1-10  (actual values on screens are 5 stars, which can have 1/2 values)
    ratings <- as.numeric(str_extract(ratings, "[[0-9]]")) + 1
    # make ratings "good or bad", as opposed to a scale of 1-10 1-5 is bad, 5-10 is good
    #ratings <- ceiling(ratings/5)  
    
    df <- data.frame(artistName, albums, reviewLinks, ratings, unlist(reviews), stringsAsFactors = FALSE)
    colnames(df) <- c("Artist", "Album", "Review_Links", "Rating", "Review")
    return(df)
}


getRelatedArtists <- function(artistURL)
{
    relatedRootHTML <- getRootHTML(str_c(artistURL, "/related"))
    relatedAddr <- xpathSApply(relatedRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section[1]/ul/li/a', xmlAttrs)[1,]
    return(relatedAddr)
}
 
    
getReviewText <- function(reviewURL)
{
    
    rootReview <- getRootHTML(reviewURL)
    #this gets review text
    reviewList <- xpathSApply(rootReview, '//*[@id="cmn_wrap"]/div[1]/div[2]/section[1]/div/p', xmlValue)
    review <-NULL
    for (r in reviewList)
    {
        review <- str_c(review,r)
    }
    review <- ifelse (is.null(review), NA, review)
    return(review)
}


writeArtistDataToFile <- function(dfArtist, fileName = "artistInfo.tbl")
{
    write.table(dfArtist, file = fileName)
}

readArtistDataFromFile <- function(fileName)
{
    return(read.table(fileName, stringsAsFactors = FALSE))
}


popReviews <- function(fileName = NULL)
{
    
    relatedToSeed <- getRelatedArtists(seedURL)
    artistRep <- getArtistRepository(seedURL)
    
    related <- NULL
    for(artist in relatedToSeed)
    {
        aRep <- getArtistRepository(artist)
        artistRep <- rbind(artistRep, aRep)
        related <- c(related, getRelatedArtists(artist))
    }
    
    idx <- 1
    while(length(artistRep) <= 1500 & idx < length(related))
    {
        #if we fail on one keep going, don't really care if some get missed for this exercise
        try({
            aRep <- getArtistRepository(related[idx])
            artistRep <- rbind(artistRep, aRep)
            artistRep <- artistRep[!is.na(artistRep$Review),]
            artistRep <- artistRep[!is.na(artistRep$Rating),]
        })
        idx <- idx+1
    }
    
    colnames(artistRep) <- c("Artist", "Album", "Review_Links", "Rating", "Review")
    
    if (!is.null(fileName))        
    {
        writeArtistDataToFile(artistRep, fileName)
    }
    
    return(artistRep)
}

#creates a corpus from the passed in filename, and does various clean up to make the corpus more "trainable"
createCorpus <-function(artistsFileName)
{
    
    artists <- readArtistDataFromFile(artistsFileName)
    #remove duplicates, improvement would be to have this done during scraping, as that would be more time efficient
    artists <- unique(artists)
    
    #change the 0-10 rating to a "pass/fail" rating of 1/2
    #note allmusic seems to have grade inflation problem, which seems to make training dificult
    #as most everything ranks better than 3 stars, so change the ranking a little and making under 3 1/2
    #stars considered "bad" and 3 1/2 and above good
    artists$Rating  <- ceiling((artists$Rating-2)/5)
    artists$Rating[artists$Rating == 0] <- 1  # make 1 the few that end up zero by above manipulation
    
    #make the corpus and clean up by removing extraneous items, e.g., punctuation, numbers etc
    reviews <- VectorSource(artists$Review)
    artists_corpus <- Corpus(reviews)
    artists_corpus <- tm_map(artists_corpus, removeNumbers)
    artists_corpus <- tm_map(artists_corpus, str_replace_all, pattern = '[[: punct:]]', replacement = ' ')
    artists_corpus <- tm_map(artists_corpus, tolower)
    artists_corpus <- tm_map(artists_corpus, removeWords, words =  stopwords("en"))
    
    #some prior transformations may change the document type, so default it back to plaintextdocument
    artists_corpus <- tm_map(artists_corpus, PlainTextDocument)
    #"stem" various words, to reduce variations of words with essentially duplicate meanings
    artists_corpus <- tm_map(artists_corpus, stemDocument)
   
    for (i in 1:length(artists_corpus))
    {
        meta(artists_corpus[[i]], "rating") <- artists$Rating[i]
    }
    return (artists_corpus)
}

#returns DTM, either with a bigram, with lots of tokens or simpler dtm with no tokenizer function
createDTM <- function(corp, bigram = TRUE)
{
    
    if (bigram)
    {
        #this makes for big dtm, but helps the training
        BigramTokenizer <- function(x){NGramTokenizer(x, Weka_control(min = 1, max = 2))}
        dtmBigram <- DocumentTermMatrix(corp, control = list(tokenize = BigramTokenizer))
        
        #drop terms not appearing in at least 10 documents
        dtmBigram <- removeSparseTerms(dtmBigram, 1 - (10/length(corp)))
        return(dtmBigram)
    }
    else
    {
        dtm <- DocumentTermMatrix(corp)
        dtm <- removeSparseTerms(dtm, 1 - (10/length(corp)))
        return(dtm)
    }
}   

#returns a data frame with results of training against our DTM using SVM, Forest and MaxEntropy methods
trainandGetResults <- function(dtm, corp)
{
    rating_labels <- unlist(meta(corp, "rating"))
    
    container <- create_container(dtm, 
                                  labels = rating_labels,
                                  trainSize = 1:(length(rating_labels)*.75),
                                  testSize = ((length(rating_labels)*.75)+1):length(rating_labels),
                                  virgin = FALSE
                                  )
    
    svm_model <- train_model(container, "SVM")
    tree_model <- train_model(container, "TREE")
    maxent_model <- train_model(container, "MAXENT")
    
    svm_out <-classify_model(container, svm_model)
    tree_out <- classify_model(container, tree_model)
    maxent_out <- classify_model(container, maxent_model)
    
    labels_out <- data.frame(
        correct_label = rating_labels[((length(rating_labels)*.75)+1):length(rating_labels)],
        svm = as.character(svm_out[,1]),
        tree = as.character(tree_out[,1]),
        maxent = as.character(maxent_out[,1]),
        stringsAsFactors = F)
}


#scrape all music, and create a datafile with artists and review info
#commented out because takes really long time to execute, saved data in artistinfo.tbl is 
#what is needed for working on sentiment analysis
#(Note this one line, along with setting the seedurl, is all that is needed to do the scraping)

###popReviews("artistinfo.tbl")



#Load and execute using saved datafile
artistCorpus <- createCorpus("artistinfo.tbl")
dtm <- createDTM(artistCorpus)
results <- trainandGetResults(dtm, artistCorpus)

##SVM Results
table(results[,1] == results[,2]) / nrow(results)
## Random forest performance
table(results[,1] == results[,3]) /nrow(results)
## Maximum entropy performance
table(results[,1] == results[,4]) / nrow(results)

#show results just for "bad" albums
resultsBad <- results[results$correct_label == 1,]
#SVM Results
table(resultsBad[,1] == resultsBad[,2]) / nrow(resultsBad)
## Random forest performance
table(resultsBad[,1] == resultsBad[,3]) / nrow(resultsBad)
## Maximum entropy performance
table(resultsBad[,1] == resultsBad[,4]) / nrow(resultsBad)

#show results just for "good" albums
resultsGood <- results[results$correct_label == 2,]
#SVM Results
table(resultsGood[,1] == resultsGood[,2]) / nrow(resultsGood)
## Random forest performance
table(resultsGood[,1] == resultsGood[,3]) / nrow(resultsGood)
## Maximum entropy performance
table(resultsGood[,1] == resultsGood[,4]) / nrow(resultsGood)


    
