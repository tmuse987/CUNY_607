initialize <-function()
{
    library(stringr)
    library(tidyr)
    library(dplyr)
    library(plyr)
}

    initialize()

getCensusDF <- function(fileName = "census_disability.csv")
{
    ##dfCensus <- read.csv(fileName, stringsAsFactors = FALSE)
    dfCensus <- read.csv("census_disability.csv", stringsAsFactors = FALSE)
    ##remove top comment rows and bottom comment rows which appear after "independent living difficulty" row
    ##dfCensus <- slice(dfCensus, 6:which(str_trim(dfCensus[,1]) == "Independent living difficulty"))
    dfCensus <- slice(dfCensus, 6:(which(str_trim(dfCensus[,1]) == "SEX") -2) )
    dfCensus <- subset(dfCensus, select = c(-X,-X.1,-X.3, -X.4))
    
    #colnames(dfCensus) <- seq(1:length(dfCensus)) #makes columns names numbers
    
    
    dfCensus[1,2:length(dfCensus)] <- unlist(rep(dfCensus[1, seq(2, length(dfCensus), 6)], each = 6))  # file in blank header cells
    dfCensus[2,2:length(dfCensus)] <- unlist(rep(dfCensus[2, seq(2, length(dfCensus), 2)], each = 2))  # fill in blank subheader cells
    dfCensus[2,] <- str_c(dfCensus[2,], " ",  dfCensus[3,]) #combine header/subheader
    colnames(dfCensus) <- dfCensus[1,]    #make header row into columnames
    dfCensus <- dfCensus[c(-1,-3),]       #remove rows that were header/subheader
    
    return(subset(dfCensus, (str_trim(dfCensus[,2])!= "")))  # remove blank lines and return
}  

getDataRotatedViaState <-function(df)
{    
    dfTranspose <- NULL
    
    #loop thru to do one state (or us as whole) at a time
    while (length(dfCensus) >=7)
    {
        dfOneLoc <- dfCensus[,1:7]  # get df of one location
        #insert location into df as col2
        dfOneLoc <- cbind(Subject = dfOneLoc[,1],  Location = rep(colnames(dfCensus[2]), nrow(dfCensus)), dfOneLoc[,2:7])   
        colnames(dfOneLoc) <- c(colnames(dfOneLoc[1:2]), dfOneLoc[1,3:8])  #rename some cols
        dfOneLoc <- dfOneLoc[-1,]  #remove header row that is now colnames
        
        if (is.null(dfTranspose))
        {   
            dfTranspose <- dfOneLoc
        }
        else  #if 2+ loop, append
        {   
            dfTranspose <- bind_rows(dfTranspose, dfOneLoc)
        }
        if(!length(dfCensus) <8)  #if still another state left to process, chop off front that we just transposed
        {
            dfCensus <- select(dfCensus, Subject, 8:length(dfCensus))    
        }
        else
        {
            dfCensus <- dfCensus[,-7]  #drop a column so fall out of loop
        }
    }
    return(dfTranspose)
}

createDFWithAgeCol <- function(dfTranspose, dfOrig)
{
    #get list of ages, e.g., "population under 5 years
    ageGroup <- unique(as.character(filter(dfTranspose, grepl("Population", Subject))[,1]))
    #get list of disabilities e.g., with a hearing difficulty
    disGroup <- c("Total", unique(as.character(filter(dfTranspose, grepl("With", Subject))[,1])))
    
    
    #create df with setup with appropriate number of rows for an individual location
    dfDis <- data.frame(Disability = rep(disGroup, length(ageGroup)), Age = rep(ageGroup, 1, each = length(disGroup)), 
                        stringsAsFactors = FALSE)
    cols <-  colnames(dfTranspose)[2:length(dfTranspose)]
    dfDis[, cols] <- " "    ##leave spaces instead of nulls or NA's do to problem assigning values below
    dfDis <- rbind(" ", dfDis)
    dfDis[[1,1]] <- "Total for Location"
    dfDis[[1,2]] <- "Total All Groups"
    
    dfTemp <- NULL
    locations <- unique(dfTranspose$Location)  #i.e., the states and US 
    #loop for unique number of locations to create empty rows (mostly) for each location
    for (i in 1:(length(unique(dfTranspose$Location))))
    {
        if (is.null(dfTemp))
        {
            dfDis$Location <- locations[i]
            dfTemp <- dfDis
        }
        else
        {
            dfDis$Location <- locations[i]
            dfTemp <- rbind(dfTemp, dfDis)
        }
    }
    dfDis <- dfTemp
    rm(dfTemp)
    return(dfDis)
}   
    

#combines mostly empty dfAge df, with partially tidy dftidy
createFinalTidy <-function(dfTidy, dfAge)
{
    #populate total lines
    dfAge[str_detect(dfAge$Age, "Total"),3:length(dfAge)]  <- dfTidy[str_detect(dfTidy$Subject, "Total"),2:length(dfTidy)]

    #populate all rows with actual data
    for(loop in 2:nrow(dfTidy))
    {
        #if we are at a new population set the total line 
        if (!is.na(str_extract(dfTidy[loop,"Subject"], "^Population")))
        {
            ageGroup <- as.character(dfTidy[loop,"Subject"])
            dfAge[dfAge$Disability == "Total"
                  & dfAge$Age == ageGroup
                  & dfAge$Location == dfTidy$Location[loop]
                  ,3:9
                 ] <- dfTidy[loop, 2:8]
        }
        else  # set the values for other lines
        {
            dfAge[dfAge$Disability == as.character(dfTidy$Subject[loop])
                  & dfAge$Age == ageGroup
                  & dfAge$Location == as.character(dfTidy$Location[loop])
                 ,3:9
                 ] <- dfTidy[loop,2:8]
        }
    }
    
    #"private" function to make all blanks or (x) to be NA
    updateBlanksToNA  <-function(col)
    {
        col[str_detect(col, "^ ")] <- NA
        col[str_detect(col, "(X)")] <- NA
        return(col)
    }

    dfAge <- data.frame(lapply(dfAge, function(x) {str_replace_all(x, ",*", "")}))
    dfAge$Percent.with.a.disability.Estimate <- 
            unlist(lapply(dfAge$Percent.with.a.disability.Estimate, function(x) {str_replace(x, "%", "")} ))
    dfAge$Total.Margin.of.Error <- 
             unlist(lapply(dfAge$Total.Margin.of.Error, function(x) {str_replace(x, "\\+/-", "")} ))
     
    dfAge$Percent.with.a.disability.Margin.of.Error <-
         unlist(lapply(dfAge$Percent.with.a.disability.Margin.of.Error, function(x) {str_replace(x, "\\+/-", "")} ))
    
    dfAge$With.a.disability.Margin.of.Error <-
        unlist(lapply(dfAge$With.a.disability.Margin.of.Error, function(x) {str_replace(x, "\\+/-", "")} ))


    
    #update blanks function changed to matrix, so coerce into df
    dfAge <- data.frame(apply(dfAge, 2, updateBlanksToNA),stringsAsFactors = FALSE)
    return(dfAge)
    
    
}
#     
    
    
    

dfCensus <- getCensusDF()
dfTidy <- getDataRotatedViaState(dfCensus)
dfAge <- createDFWithAgeCol(dfTidy, dfCensus)
dfTidy <- createFinalTidy(dfTidy, dfAge)







