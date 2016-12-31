library(XML)
library(stringr)
library(RCurl)
library(selectr)
library(rvest)
library(mongolite)
#library(jsonlite)
source("utilities.r")


seedURL <-"http://www.allmusic.com/artist/nick-cave-mn0000397880"
setwd("L:\\school\\cuny\\607\\finalproj - Mongo")

mongoURL <- "mongodb://localhost:27017/MusicScrapings"
mongoDB <- "MusicScrapings"
collection <- "AllMusic"

# Mongo Section -------


getAlbumsBefore2000 <- function()
{
    mongoConnection("AllMusic")$find('{"Year": { "$lt": "2000" } }')
}

getAlbums <- function()
{
    mongoConnection("AllMusic")$find()
}


getArtistURLs <- function()
{
    return(mongoConnection("Artists")$find(fields = '{"_id" : 0, "ArtistsURL" : 1}'))
}

# allmusic Scraping Section ----

getRootHTML <- function(artistURL)
{
    html <- getURL(artistURL,
                   useragent = str_c(R.version$platform,
                                     R.version$version.string,
                                     set=", "))
    root <- NULL
    try(root <- xmlRoot(htmlParse(html)))

    return(root)
}

getArtistRepository <- function(artistURL)
{
    artistRootHTML <- getRootHTML(str_c(artistURL, "/discography"))
    #exit if nothing returned
    if (is.null(artistRootHTML)) return(NULL)
    
    artistName <- xpathApply(artistRootHTML,'//*[@id="cmn_wrap"]/div[1]/div[2]/header/div/hgroup/h1', xmlValue )
    artistName <- str_replace_all(artistName, "\n +|  +", "")
    #header <- xpathSApply(artistRootHTML, "//table/tr/th", xmlValue)
    
    #get year
    #year <- xpathApply(artistRootHTML, '//*[contains(concat( " ", @class, " " ), concat( " ", "year", " " ))]', xmlValue)
    year <- str_trim(xpathApply(artistRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section/table/tbody/tr/td[3]', xmlValue))
    
    #get album names
    albums <- xpathSApply(artistRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section/table/tbody/tr/td[4]/a[1]', xmlValue)
    
    #rating is 2nd attr of this section
    criticRatings <- xpathSApply(artistRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section/table/tbody/tr/td[6]', xmlAttrs)[2,]
    #ratings is first numeric value, and is from 0-9, so add 1 to make 1-10  (actual values on screens are 5 stars, which can have 1/2 values)
    criticRatings <- as.numeric(str_extract(criticRatings, "[[0-9]]")) + 1
    
    
    # #user ratings are not "in the html_doc...", they are a css class, that is probably "jqueried" when the page is loaded
    # userRatings <- xpathSApply(artistRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section/table/tbody/tr/td[7]', xmlAttrs)[2,]
    # userRatings <- as.numeric(str_extract(userRatings, "[[0-9]]")) + 1
    
    
    df <- data.frame(artistName, year, albums, criticRatings, stringsAsFactors = FALSE)
    colnames(df) <- c("Artist", "Year", "Album", "critcRatings")#, "userRatings")
    return(df[complete.cases(df),])
}


#pass in list of artist allmusic URL's returns df with artists albums, and ratings, both critics and user
addRatingsToArtists <- function(lArtistsURL)
{
    df <- getArtistRepository(lArtistsURL[1])
    for(idx in 2:length(lArtistsURL))
    {
         df<- rbind(df, getArtistRepository(lArtistsURL[idx]))
         if ((idx %% 100) == 0) print (str_c("iteration " , idx))  #since this can take forever...status
    }
    
    return(df)

}


#returns a list of artists "related" the artist passed in
getRelatedArtists <- function(artistURL)
{
    relatedRootHTML <- getRootHTML(str_c(artistURL, "/related"))
    relatedAddr <- NULL
    if (!is.null(relatedRootHTML))
    {
        relatedAddr <- xpathSApply(relatedRootHTML, '//*[@id="cmn_wrap"]/div[1]/div[2]/section[1]/ul/li/a', xmlAttrs)[1,]
    }
    return(relatedAddr)
}


#recursive, list will grow fast as well as execution time if depth is much higher than small single digits
#note artists param is meant to be used by the function itself during recursion, the initial call does not need to populate
buildArtistList <- function(url, depth, artists = url)
{
    #print("call build artist2")
    related <- getRelatedArtists(url)
    if (is.null(related) | depth ==0) 
        return(related)
    
    depth <- depth -1
    
    while(length(related) > 0)
    { 
        if(length(artists[artists == related[1]]) < 1)
        {
            artists <- unique(c(artists, buildArtistList3(related[1], depth, c(related[1], artists))))
    
        }
        related <- related[-1]
    }
    
    return(artists)
}



#6degreesofBacon ----

perform6DegreesOfBacon <- function()
{
    baconList <- buildArtistList("http://www.allmusic.com/artist/the-bacon-brothers-mn0000044295/", 6)
    dfBacon <- as.data.frame(baconList, stringsAsFactors = F)
    colnames(dfBacon) <- "ArtistsURL"
    dfBacon$ArtistName <- str_extract(dfBacon[,], "artist/.*") %>% 
        str_replace("artist/", "") %>% 
        str_replace("-mn.*", "") %>% 
        str_replace_all("-", " ") %>%
        simpleCap()
    conn <- mongoConnection("SixDegreeBacon")
    insertMongo(conn, dfBacon, T)
    
}

# test ----


#perform6DegreesOfBacon()




#dfAllMusic <- getArtistRepository(seedURL)


#related <- getRelatedArtists(seedURL)

# artists <- readCSV()
# conn <- mongoConnection()
# insertArtists(conn, artists, TRUE)

           
