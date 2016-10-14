initialize <-function()
{
    library(stringr)
    library(tidyr)
    library(dplyr)
    library(plyr)
}
 

Get7ColsGatheredAndDrop4C <- function(df)
 {
     dfGather <- df[,1:7]                                 #take 1st 7 cols
     dfGather <- gather(dfGather, ColKey, ColVal, -1:-3)   #gather around first 3 cols
     df <- df[,-4:-7]                                     #drop col 4-7 in original df
     return(list(dfGather, df))                           #return list of 2 dataframes one "gathered, 
                                                          #one original with columns processed removed
}


#reads in filename and creates uncluttered DF (i.e., extraneous lines and columns and so forth removed)
GetMortalityDF <- function(filename = "SexSpecificMortality.csv")
{
    dfCMort<- read.csv(filename, stringsAsFactors = FALSE, skip =6, header=FALSE)
    dfCMort <-dfCMort[,1:19]  ##drop trailing empty cols which were part of .csv
    dfCMort[1,4:length(dfCMort)] <- unlist(rep(dfCMort[1, seq(4, length(dfCMort), 4)], each=4))  ##assign blank header values
    dfCMort[1,] <- str_c(dfCMort[1,], dfCMort[2,])
    colnames(dfCMort) <- (dfCMort[1,])
    
    dfCMort <- dfCMort[c(-1,-2),]
    dfCMort <- subset(dfCMort, Country != "") #drop closing comments which are in col 1
    return(dfCMort)
}

#makes the df tidy for analysis
GetaTidyDeathDF <-function(df)
{    
    dfTidy <- NULL
    dfWorking <- df
    
    # this is going to loop through the four sections and with one section at a time
    while(length(dfWorking) >= 7)
    {
        #get one section of data
        listDF <- Get7ColsGatheredAndDrop4C(dfWorking)
        if (is.null(dfTidy))  #i.e., first time thru loop
        {
            #make temp col names of male1990, male2000 or female 1990 etc.
            cNames <- c(str_extract(colnames(dfWorking)[1:4],"[[:alpha:]]*"),
                        str_c(str_extract(colnames(dfWorking)[4],"[[:alpha:]]*"), " Values"))
            
            ##dfTidy gets fist df in list...the "tidy" one
            dfTidy <- data.frame(listDF[1])  
        }
        else
        {
            cNames <- c(colnames(dfTidy), 
                        str_c(str_extract(colnames(dfWorking)[4],"[[:alpha:]]*"), " Values"))
            
            #add two rotated columns
            #dfTidy <- bind_cols(dfTidy, data.frame(listDF[1])[,5]) does not work???, so add 2 cols and delete one
            dfTidy <- bind_cols(dfTidy, data.frame(listDF[1])[,4:5])
            dfTidy <- dfTidy[,-(length(dfTidy)-1)]
            
        }
        colnames(dfTidy) <- cNames
        dfWorking <- data.frame(listDF[2])  #dfworking is original with cols slowly being removed as processed
    }
    colnames(dfTidy)[4] = "Year"  
    colnames(dfTidy)[5] = "Mort Male"
    colnames(dfTidy)[6] = "Mort Female"
    colnames(dfTidy)[6] = "Sex Ratio"
    dfTidy$Year <- str_replace(dfTidy$Year, "Male", "")
    return(dfTidy)
}

dfPartTidy <- GetMortalityDF()
dfTidy <-GetaTidyDeathDF(dfPartTidy)







