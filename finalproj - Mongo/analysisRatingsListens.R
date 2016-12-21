library(mongolite)
library(stringr)
setwd("L:\\school\\cuny\\607\\finalproj - Mongo")
source("utilities.r")
        

mongoURL <- "mongodb://localhost:27017/MusicScrapings"
mongoDB <- "MusicScrapings"
fmCollection <- "LastFMData"


getAlbumYear <- function(album, artist)
{
    #mongoConnection("AllMusic")$find('{"Year": { "$lt": "2000" } }')
    
    artist<- str_replace_all(artist, '"', '')
    album <- str_replace_all(album, '"', '')
    
    q <- str_c('{"Artist" : { "$eq" : "' , artist, '"}}' )
    
    
    df <- mongoConnection("AllMusic", verbosity = F)$find(query = q,
                                     fields = '{"_id" : 0, "Year": 1, "Album": 1 }')
    
    if(nrow(df)>0)
        return(as.numeric(unique(df[toupper(df$Album) == toupper(album),][1,1])))
    else
        return(NA);
    
    
        
}


#This is a horrible way to do it :(  Mongo is way to slow for this kind of work (at least without indexing and other tuning)
#will do once and add a table (aka collection)
#not actually used
addYears <- function(df)
{
    df$Year <- NA
    df$Year <- mapply(getAlbumYear, df$Album, df$Artist)
    return(df)
    
}



subsetByLargeAlbumCount <- function()
{
    dfReturn <- NULL
    distinctArtists <- unique(artists$Artist)
    for(idx in 1:length(distinctArtists))
    {
        dfArtist <- artists[artists$Artist == distinctArtists[idx],]
        if(nrow(dfArtist)>7)
        {
            dfReturn <- rbind(dfReturn, dfArtist)
        }
    }
    return(dfReturn)
}


individualRegression <- function(dfArtists)
{
    dfArtistsCor <- NULL
    
    distinctArtists <- unique(dfArtists$Artist)
    
    
    for(idx in 1:length(distinctArtists))
    {
        df <- dfArtists[dfArtists$Artist == distinctArtists[idx],]
        lmL <- lm(df$Listeners ~ df$CriticRating)
        lmP <- lm(df$Plays ~ df$CriticRating)
        sumL <- summary(lmL)
        sumP <- summary(lmP)
        sumL$df[2]
        dfCor <- cbind(distinctArtists[idx],  cor(df$CriticRating, df$Listeners), cor(df$CriticRating, df$Plays), 
                       lmL$coefficients[1], lmL$coefficients[2], sumL$df[2], sumL$r.squared,
                       lmP$coefficients[1], lmP$coefficients[2], sumP$df[2], sumP$r.squared)
        
        
        dfArtistsCor <- rbind(dfArtistsCor, dfCor)
    }
    cNames <- c("Artist", "CriticVListnerCorr", "CriticVPlayCorr", "ListenerIntercept", 
                "ListenerCoefficient", "DFListener", "RSQListener",
                "PlayIntercept", "PlayCoefficient", "DFPlay", "RSQPlay")
    colnames(dfArtistsCor) <- cNames
    return(as.data.frame(dfArtistsCor, stringsAsFactors = F))
}

# sample analysis code ----

# 
# artists <- mongoConnection("AMFMCombined")$find()
# artists$Year <- as.numeric(artists$Year)
# artists$Listeners <- as.numeric(artists$Listeners)
# artists$Plays <- as.numeric(artists$Plays)
# artists$CriticRating <- as.numeric(artists$CriticRating)
# cor(artists$CriticRating, artists$Listeners)
# cor(artists$CriticRating,artists$Plays)
# lmArtistsL <- lm(artists$Listeners ~ artists$CriticRating)
# lmArtistsP <- lm(artists$Plays ~ artists$CriticRating)
# summary(lmArtistsL)
# summary(lmArtistsP)
# 
# median(artists$Listeners)
# median(artists$Plays)
# 
# options(scipen = 5)
# hist(artists$Listeners, breaks= 30, ylim = c(0, 10000), xlim = c(0,1000000), xlab = "Listeners")
# hist(artists$Plays, breaks= 50, ylim = c(0, 10000), , xlim = c(0,10000000), xlab = "Plays")
#multiAlbumArtists <- subsetByLargeAlbumCount()
# artistsCor <- individualRegression(multiAlbumArtists)
# sum(as.numeric(artistsCor$CriticVListnerCorr)) / nrow(artistsCor)
# sum(as.numeric(artistsCor$CriticVPlayCorr)) / nrow(artistsCor)
# sum(as.numeric(artistsCor$RSQListener)) / nrow(artistsCor)
# sum(as.numeric(artistsCor$RSQPlay)) / nrow(artistsCor)



