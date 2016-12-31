library(mongolite)
mongoURL <- "mongodb://localhost:27017/MusicScrapings"
mongoDB <- "MusicScrapings"
#from "R" help in toupper function
simpleCap <- function(x) 
{
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}


mongoConnection <- function(coll = collection, verbosity = T)
{
    return(mongo(coll, mongoDB, mongoURL, verbose = verbosity ))
}

insertMongo <-function(mongConn, df, drop = F)
{
    if(drop)
    {
        try(mongConn$drop())
    }
    mongConn$insert(df)
    
}