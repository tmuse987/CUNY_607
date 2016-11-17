library(jsonlite)
library(mongolite)
library(DBI)
library(RMySQL)
library(stringr)

rDBName <- "flights"
passWordFile <- "L:\\school\\cuny\\dbPassword.txt"


##this will drop any existing collections with the same names we wish to load...beware :)
dropMongo <- function(collections)
{
    #we will get error messages if we run this clean up and the collection doesn't exist in mongo
    ##so turn off messages
    options(show.error.messages = FALSE)
    
    for(c in collections)
    {
        m <- mongo(collection = c)
        try(m$drop())   ##returns error if doesn't exist, could do existance checks, but why bother
    }

    options(show.error.messages = TRUE)
}

##just a function to return a mysql db connection
getDBConn <- function(dbName)
{
    db <- dbDriver("MySQL")
    dbConn <-dbConnect(db, user='root',scan(passWordFile, what = "character"),dbname= dbName);
    return(dbConn)
}

##takes in a table name that exists on mySQL db, and a mysql connection, and copies data from one db to the other
popMongoTable <- function(tableName, dbConn)
{
    mongo(collection = tableName)$insert(dbGetQuery(dbConn, str_c("select * from ", tableName)))
}

##this takes in my sql schema name, connects to it, and then loops through the list of passed in tables
##calling popMongoTable to copy the data from mysql to mongo
convertMySQLToMongo <- function(db, tables)
{
    conn <- getDBConn(db)
    lapply(tables, popMongoTable, conn)
    dbDisconnect(conn)
}

#get a list of tables for a given schema on mysql
getMySqlTables <- function(db)
{
    conn <- getDBConn(db)
    tables <- dbListTables(conn)
    dbDisconnect(conn)
    return(tables)
}


#function that will compare (and print) counts from mysql and mongo, so we can see all data transferred
#also prints head of collection from mongo for visual verification that data looks right
compareDBs <- function(sqlDB, table)
{
    conn <- getDBConn(sqlDB)
    print(str_c("Count from mySql table: ", table, " is ", dbGetQuery(conn, str_c("select count(*) from ", table))))
    c <- mongo(collection = table)
    print(str_c("Count from Mongo Collection: ", table, " is ", c$count()))
    print(head(c$find(),5))
    dbDisconnect(conn)
    
}



# tables <- 
# dropMongo(tables)getMySqlTables("flights")
# convertMySQLToMongo("flights", tables)
# 
# 

# head(c$find())
# test <- c$iterate()
# test

# 
# c <- mongo(collection = "airlines")
# c$find('{"carrier" : "AA"} ')
# 
# compareDBs(rDBName, "airlines")
# compareDBs(rDBName, "airports")
# compareDBs(rDBName, "flights")
# compareDBs(rDBName, "planes")
# compareDBs(rDBName, "weather")
# 
# convertMySQLToMongo("diamonds", "diamonds")

#convertMySQLToMongo("flights", getMySqlTables("flights"))


 #m <- mongo(collection = "diamonds")
 #data(diamonds, package = "ggplot2")
# 
# m$insert(diamonds)
# 
# m$count()
# 
# out <- m$find('{"cut": "Premium", "price": { "$lt" : 1000 } } ')
# 
# head(out)






#selected <-dbGetQuery(getDBConn(rDBName), "select * from airlines")

#m$insert(selected)
#m$find('{"carrier" : "AA"} ')

#dropMongo("diamonds")
#convertMySQLToMongo("diamonds", "diamonds")
# compareDBs(rDBName, "diamonds")
      
