initialize <-function()
{
    library(stringr)
    library(tidyr)
    library(dplyr)
    library(plyr)
}


getEarningsAsDF <- function(filename = "earnings.csv")
{

    df <- read.csv(filename, stringsAsFactors = FALSE, skip =5, header=FALSE)
    
    #make col3 = col2 and so forth by 2
    df[1,seq(3, length(df), by = 2)] <- df[1,seq(2, length(df), by = 2)]
    df[1,] <- str_c(df[1,], " ", df[2,])
    df <- df[-2,]
    df <- df[1:39,]
    df[1,1] <- "Ages"
    colnames(df) <- df[1,]
    return(df)
}


getEarningsAsTidyDF <- function(df)
{
    df <- filter(df, Ages != "")   #remove blank rows
    df <- df[-1,]
    
    # add column at end containg gender of corresponding rows
    df["Sex"] <- unlist(list(rep("B", nrow(df)/3),rep("M", nrow(df)/3),rep("F", nrow(df)/3)))
    
    ##make df with just "number cols"
    dfNums <- select(df, Ages, Sex, contains("number"))
    
    ##make df with just  "pct cols"
    dfPct <- select(df, Ages, Sex, contains("percent"))
    
    #rotate both of them
    dfEarnNumRot <- gather(dfNums, Income, Amt, -Ages, -Sex)
    dfEarnPctRot <- gather(dfPct, Income, Pct, -Ages, - Sex)
    
    #put those rotated columns together
    dfEarnNumPct <- bind_cols(dfEarnNumRot, dfEarnPctRot[length(dfEarnPctRot)])
    
    #some general text cleanup
    dfEarnNumPct$Income <- str_replace(dfEarnNumPct$Income, "Number", "")
    dfEarnNumPct$Ages <- str_replace(dfEarnNumPct$Ages, "Both sexes|Male|Female", "Total")
    dfEarnNumPct$Amt <- (lapply(dfEarnNumPct$Amt, function(x) {str_replace_all(x, ",*", "")}))
    return(dfEarnNumPct)
}

initialize()
df <- getEarningsAsDF()

dfTidy <- getEarningsAsTidyDF(df)









