setwd("L:\\school\\cuny\\607\\finalproj - Mongo")
source("lastFM_API2.r")
source("readAllMusic.r")
source("utilities.r")
library(parallel)
#library(snow)

seedURL <-"http://www.allmusic.com/artist/nick-cave-mn0000397880"
setwd("L:\\school\\cuny\\607\\finalproj - Mongo")
#mongoURL <- "mongodb://localhost:27017/MusicScrapings"
#mongoDB <- "MusicScrapings"


getCompleteFMCases <- function()
{
    df <- mongoConnection(fmCollection)$find(fields = '{"_id" : 0, "album.name" : 1, "album.artist" :1, 
                                             "album.listeners": 1, "album.playcount" : 1  }')
    #above returns df of 1 column that is vector of 4, transform
    df <- data.frame(cbind(unlist(df[,1][1], use.names = F),
                           unlist(df[,1][2], use.names = F),
                           unlist(df[,1][3], use.names = F),
                           unlist(df[,1][4], use.names = F)),
                     stringsAsFactors = F)
    cNames <- c("Album", "Artist", "Listeners",  "Plays")
    colnames(df) <- cNames
    
    df <- df[complete.cases(df),]
    return(df[complete.cases(df),])
}


# Calls function in readAllMusic.r, to get a list of related artists and then pops in mongo collection
# params:
#   * seed_URL, starting allmusic artist page
#   * collection, mongo colleciton name to insert the generated list
#   * depth, how many levels do we want go down, for related artists
#   * overwrite, overwrite or append to mongo collection
populateArtistCollection <- function(seedURL, collection, depth, overWrite = F)
{
    artists <- buildArtistList(seedURL, depth)
    dfArtists <- as.data.frame(artists, stringsAsFactors = F)
    colnames(dfArtists) <- "ArtistsURL"  
    #pull artist name from URL
    dfArtists$ArtistName <- lapply(dfArtists[,1], function(x) str_extract(x, "artist/.*") %>% 
                                       str_replace("artist/", "") %>% 
                                       str_replace("-mn.*", "") %>% 
                                       str_replace_all("-", " ") %>%
                                       simpleCap())
    
    conn <- mongoConnection(collection)
    insertMongo(conn, dfArtists, overWrite)
}


#calls function in readAllMusic.r to get a list of albums per artist, and critic ratings
#populates mongo collection with that info
# params
# * vArtistUrls, char vector of artist urls to allmusic
# * collection is name of mongo collection to store results
# * overwrite, overwrite or append to mongo collection
populateArtistRepository <- function(vArtistURLs, collection, overWrite = F)
{
    dfArtistRepository <- addRatingsToArtists(vArtistURLs)
    conn <- mongoConnection(collection)
    insertMongo(conn, dfArtistRepository, overWrite)
}

#can't resist the pun
meshAMFM <-function(dfAM, dfFM)
{
    
    conn <- mongoConnection("AMFMCombined2")
    
    dfAMUpper <- dfAM
    dfAMUpper$Artist <- toupper(dfAM$Artist)
    dfAMUpper$Album <- toupper(dfAM$Album)
    dfMesh <- NULL
    for(idx in 1:nrow(dfFM))
    {
        AMfull <-unique(dfAM[dfAMUpper$Artist == toupper(dfFM$Artist[idx])
                       & dfAMUpper$Album == toupper(dfFM$Album[idx])
                       ,])[1,]
        AM <- cbind(AMfull$critcRatings, AMfull$Year)
        
        if(complete.cases(AMfull))
        {
            amfm <- cbind(dfFM[idx,], AM)
            colnames(amfm)[colnames(amfm) == 1] <- "CriticRating"
            colnames(amfm)[colnames(amfm) == 2] <- "Year"
            insertMongo(conn, amfm)
        }
    }
}


# ----

#uncomment later
populateFMDataFromAMTable <- function()
{
    albums <- getAlbums()
    cluster <- makeCluster(9)
    clusterEvalQ(cluster, {
        source("lastFM_API2.r")
        source("readAllMusic.r")
        source("utilities.r")
        library(parallel)
    })
    parApply(cluster, albums[,c("Artist", "Album")], 1, populateLastFMCollection, mongoConnection(fmCollection))
    stopCluster(cluster)
}

#These 6 statements below do all the populating of the tables in mongo, with arist, and album info, 
#with six levels of artists references, i.e, start with seed artist, get all artists which allmusic 
#thinks are related, get all the related artists to those artists, and do 5 more times

#populateArtistCollection(seedURL, "Artists", 6, T)
#populateArtistRepository(getArtistURLs()[,1], "AllMusic")
#populateFMDataFromAMTable()
#dfAllMusic <- getAlbums()
#dfLastFM <- getCompleteFMCases()
#meshAMFM(dfAllMusic,dfLastFM)

            
            
