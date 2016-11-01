library(RCurl)
library(XML)
library(stringr)
library(RJSONIO)


readApiKeyFromFile <-function(fileName)
{
    apiKey <- scan(fileName, what = "character")
}

readTimesAPI <- function(URL, key, q, pageNumber)
{
    params <- str_c(URL, "q=", q, "&page=", pageNumber, "&apiKey=", key)
    return(getURI(params))
}


parseJSON <-function (results)
{
    return(fromJSON(results, simplify = FALSE))
}


#This puts all the fields from a given row into a one row df
pullFields <- function(docRow) 
{           #rbind, gets around dataframe creation errors, even though we aren't joining anyrows together here
    as.data.frame(rbind(docRow[seq_along(1:length(docRow))]), stringsAsFactors = FALSE)
}

#This takes the passed in "document section" from the parsed JSON and creates a dataframe of it 
makeDF <- function(doc)
{
    
    df <- pullFields(doc[[1]])
    for(i in 2:length(doc))
    {
        df <- rbind(df, pullFields(doc[[i]]))
    }
    return(df)
}

timesURL <- "https://api.nytimes.com/svc/search/v2/articlesearch.json?"
apiKey <- readApiKeyFromFile("timesAPIKey.txt")
query <- "brexit"
page <- 2

parsed <- parseJSON(readTimesAPI(timesURL, apiKey, query, page))

df <- makeDF(parsed$response$docs)
head(df)

df$web_url[1:5]

df$news_desk[1]


query <- "comey"
page <- 0
parsed <- parseJSON(readTimesAPI(timesURL, apiKey, query, page))

df <- makeDF(parsed$response$docs)
head(df)

df$web_url[1:5]





