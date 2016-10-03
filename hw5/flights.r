initialize <-function()
{
    library(stringr)
    library(tidyr)
    library(dplyr)
}


getFlightDF <- function(filename)
{
    flights <- read.csv(filename) %>%      #use piping
        gather(destination, counts, -X, -X.1, na.rm = TRUE) %>%  #rotate the Destination Columns
        rename(delayed = X.1) %>%    # name the column with delayed or ontime indicators
                #second row col 1 should always have 1st row col 1 values, so use lag function to move values down one
                #1row and assign to  rows 2,4,6.... (note is.na needed because 1st row from lag is na and need to chg to space)
        mutate(airline= str_c(X, unlist(lapply(lag(X), function(x) {x[is.na(x)] <-"" ; x})))) 

    flights$X <- NULL  #(easier than listing column names in mutate)
    flights <- flights[c("airline", "delayed", "destination", "counts")]  #just rearrange columns
    flights$delayed <- (!flights$delayed == "on time")                    # set delayed column to bool vals
    return(flights)
}


getMeanFlights <- function(df, bDelayed = TRUE)
{
    return(df%>% 
        filter(delayed == bDelayed) %>%
        summarise(mean(counts)))
}


getWorstDestinationByCount <- function(df)
{
    byDest <- df %>% group_by(destination)  %>%
                     filter(delayed == TRUE) %>%
                     summarise(cityCounts = sum(counts))
    return(byDest[which.max(byDest$cityCounts),])
}

getBestDestinationByCount <- function(df)
{
    byDest <- df %>% group_by(destination)  %>%
        filter(delayed == TRUE) %>%
        summarise(cityCounts = sum(counts))
    return(byDest[which.min(byDest$cityCounts),])
}



getBestWorstDestinationByPctDelayed <- function(df, worst = TRUE)
{
    grouped <- df %>% group_by(destination)
    delayed <- grouped %>% filter(delayed == TRUE) %>%
                           summarise(cityCounts = mean(counts))
    notDelayed <- grouped %>% filter(delayed == FALSE) %>%
                              summarise(cityCounts = mean(counts))
    pctDelayed <- (delayed[,2]/notDelayed[,2])
   
    if (worst)
    {
        delayed$destination[which(pctDelayed == max(pctDelayed))]
    }
    else
    {
        delayed$destination[which(pctDelayed == min(pctDelayed))]
    }
}




