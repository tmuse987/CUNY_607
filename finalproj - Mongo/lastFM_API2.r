setwd("L:\\school\\cuny\\607\\finalproj - Mongo")
library(RCurl)
library(XML)
library(stringr)
library(RJSONIO)
library(mongolite)
library(parallel)
source("utilities.R")


#sample syntax
#http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key=YOUR_API_KEY&artist=Cher&album=Believe&format=json

baseURL <- "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key="
setwd("L:\\school\\cuny\\607\\finalproj - Mongo")
apiFile <- "lastfmapi.txt"
apiKey <- (scan(apiFile, what = "character", quiet = T))


# Mongo Section -------

mongoURL <- "mongodb://localhost:27017/MusicScrapings"
mongoDB <- "MusicScrapings"
fmCollection <- "LastFMData"

getLastFM <- function()
{
    return(mongoConnection("LastFMData")$find())
}


#Scraping Section ---



parseJSON <-function (contents)
{
    results <- NULL
    try( results <-  fromJSON(contents, simplify = FALSE))
    
    return(results)
}

getAPIString <- function(artist, album) 
{
    concatURI <- str_c(baseURL, apiKey,  "&artist=", artist, "&album=", album, "&format=json")
    concatURI <- str_replace_all(concatURI, " ", "%20")  #replace space with encoding 
    return(concatURI)
}

readLastFM_JSON <-function(vArtistAlbum)
{
    concatURI <- str_c(baseURL, apiKey,  "&artist=", vArtistAlbum[1], "&album=", vArtistAlbum[2], "&format=json")
    concatURI <- str_replace_all(concatURI, " ", "%20")   #replace space with encoding
    return(parseJSON(getURI(concatURI)))
}

#populates table in mongo with results from the album info method of the lastfm api
populateLastFMCollection <- function(vArtistAlbum, mConn)
{
    results <- readLastFM_JSON(vArtistAlbum)
    if(!is.null(results))
    {
        insertMongo(mConn, results)
    }
}


#populates the count of listens and plays for a given artist/album combination
#currently not used in project as whole
getListenPlayCounts <- function(artist, album)
{
    #key <- readApiKeyFromFile("lastfmapi.txt")
    results <- readLastFM_JSON(artist, album)
    return(c(results$album$listeners, results$album$playcount))
    
}


# Testing Code -----

populateFMMongoFromFile <- function()
{
    insertMongo(mongoConnection(fmCollection), read.csv("meshed.csv", stringsAsFactors = F), T)
}


#exportdata
#mongoConnection(fmCollection)$export(file("mongoData.txt"))

populateFMDataFromFile <-function()
{
    dmd <- mongoConnection(fmCollection)
    dmd$import(file("mongoData.txt"))
}



