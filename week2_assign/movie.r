# this short r file makes a connection to a mysql db
# and connects to an db called movie_ratings and does a
# super simple query to return all rows from that eponymous table

#comment install line as needed
install.packages("RMySQL")
library("RMySQL", lib.loc="~/R/win-library/3.3")
db <- dbDriver("MySQL")
dbConn <-dbConnect(db, user='root',password='noPassword',dbname='movie_ratings');
selected <-dbGetQuery(dbConn, "select movie, rating from movieratings")
selected