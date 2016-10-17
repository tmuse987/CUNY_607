#bookChilds <- xmlChildren(root)

library(XML)
library(jsonlite)
library(stringr)


getDfFromXML <-function(filename)
{
    xml <- xmlParse(filename)
    root <- xmlRoot(xml)
    
    #get first "column" of data (eventually transposed to a row below)
    dfXML <- data.frame(xmlSApply(root[[1]], xmlValue))
    
    for (i in 2:xmlSize(root))
    {
        dfXML <- cbind(dfXML, data.frame(xmlSApply(root[[i]], xmlValue)))
    }
    #transpose, since rows were loaded as columns
    dfXML <- data.frame(t(dfXML),stringsAsFactors = FALSE)
    
    #just make rows numeric...rather than defaulted un-uesful names
    rownames(dfXML) <- seq(1:nrow(dfXML))
    
    return(dfXML)
}

getDfFromHTML <- function(filename)
{
    h <- htmlParse(filename)
    root <- xmlRoot(h)
    
    #pull header row which we will use as column names 
    header <- xpathSApply(root, "//table/tr/th", xmlValue)
    
    
    dfH <- data.frame(xpathSApply(root, "//table/tr/td[1]", xmlValue), stringsAsFactors = FALSE)
    #loop through all the table rows  (# note we make assumption the table we want will be first in page...)
    for(i in 2:length(xpathSApply(root, "//table/tr")))
    {
        dfH <- cbind(dfH, xpathSApply(root, str_c("//table/tr/td[", i, "]"), xmlValue))
    }
    colnames(dfH) <- header
    return(dfH)
}

getDfFromJSON <- function(filename)
{
    js <- fromJSON("books.json")
    dfJS <- data.frame(js, stringsAsFactors = FALSE)
    colnames(dfJS) <- str_replace(colnames(dfJS), "Uncoventional_Literature.", "") 
    return(dfJS)
}


dfBooksXML <- getDfFromXML("books.xml")
dfBooksHTML <- getDfFromHTML("books.html")
dfBooksJSON <- getDfFromJSON("books.json")

head(dfBooksXML)
head(dfBooksHTML)
head(dfBooksJSON)


