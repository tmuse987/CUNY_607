##Initialization Function, loads needed library
intialization <- function()
{
    library(stringr)
}

##Creates a Data Frame from the passed in or deafaulted tournament file
createPlayerDataFrame <- function(fileName = "")
{
    if (is.null(fileName)|| fileName=="")
    {
        rawTournament <- readLines("tournamentinfo.txt")
    }
    else
    {
        rawTournament <- readLines(fileName)
    }
    ##the file has three lines repeated, two of are interest,  filter to the two sets of lines we care about
    player1stLine <- rawTournament[str_detect(rawTournament, "^ *\\d+ | Pair")]
    player2ndLine <- rawTournament[str_detect(rawTournament, "^ *[[:alpha:]]{2} | Num ")]  ## gets player 2nd line
    
    ##read the delmited lines into variables
    player1stLineDelim <- read.delim(text = player1stLine, sep = "|", stringsAsFactors = FALSE)
    player2ndLineDelim <- read.delim(text = player2ndLine, sep = "|", stringsAsFactors = FALSE)
    
    ##merge the two delmited variables into one dataframe
    dfPlayers <- data.frame(player1stLineDelim, player2ndLineDelim)
    dfPlayers <- subset(dfPlayers, select = c(-X, -X.1))  #(remove the "X" column")
    
    
    
    ##using regex, find the prerating value of each player and append to end of dataframe
    preRating <- str_replace(str_extract(dfPlayers[["USCF.ID...Rtg..Pre..Post."]], "R: +\\d{1,4}"),"R: *", "")
    dfPlayers <- data.frame(dfPlayers, preRating, stringsAsFactors = FALSE)
    dfPlayers <- populateHeaders(dfPlayers)
    return(dfPlayers) 
}

##function returns a numeric of the opp average rating from the passed in vector 
##of rounds which contains entries into the vector of oppsRatings
getOppAvgRating <- function(vRounds, vOppsRating)
{
    round(mean(vOppsRating[unlist(vRounds)], na.rm = TRUE))
}

##funciton takes in the players data.frame, and computes a vector of the
## average opponent ratings
computeOppAvgRating <- function(dfPlayers)
{
    
    rounds <- data.frame(lapply(lapply(lapply
                        (dfPlayers[,grepl("ROUND", names(dfPlayers))], str_replace, "[[:alpha:]]", "")
                        ,str_trim)
                        ,as.numeric)
                        ,stringsAsFactors = FALSE)
    
    apply(rounds, 1, getOppAvgRating, as.numeric(dfPlayers$PRERATING))
}



##creates dataframe of player information required in the asignment
createSubsettedPlayerDF <-function(dfPlayers, vOppsAvgRating)
{
        df <- data.frame(dfPlayers$PLAYER_NAME, dfPlayers$STATE, 
                         dfPlayers$TOTAL_PTS, dfPlayers$PRERATING, vOppsAvgRating)
        names(df) <- c( "Player Name", "State", "Total Points", "Pre-Tournament Rating", "Average Opp Rating")
        return(df)
}

##generic wrapper for csv, doesn't really do anything useful, 
##other than provide a function to demonstrate in the markdown file
writeCSV <- function(df, fileName)
{
    write.csv(df, file = fileName)
}

populateHeaders <- function(dfp)
{
     names(dfp) <-(c("NAME",
      "PLAYER_NAME",
      "TOTAL_PTS",
      "ROUND1",
      "ROUND2",
      "ROUND3",
      "ROUND4",
      "ROUND5",
      "ROUND6",
      "ROUND7",
      "STATE",
      "ID_AND_PREPOST_RATING",
      "PTS",
      "RESULT1",
      "RESULT2",
      "RESULT3",
      "RESULT4",
      "RESULT5",
      "RESULT6",
      "RESULT7",
      "PRERATING"))
    return(dfp)

}

intialization()
dfP <- createPlayerDataFrame("tournamentInfo.txt")
writeCSV(createSubsettedPlayerDF(dfP, computeOppAvgRating(dfP)), file = "chessResults.csv")
