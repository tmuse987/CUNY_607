initialize <-function()
{
    library(stringr)
    library(tidyr)
    library(dplyr)
    library(plyr)
}


dfEarnings <- read.csv("earnings.csv", stringsAsFactors = FALSE, skip =5, header=FALSE)

#make col3 = col2 and so forth by 2
dfEarnings[1,seq(3, length(dfEarnings), by = 2)] <- dfEarnings[1,seq(2, length(dfEarnings), by = 2)]
dfEarnings[1,] <- str_c(dfEarnings[1,], " ", dfEarnings[2,])
dfEarnings <- dfEarnings[-2,]
dfEarnings <- dfEarnings[1:39,]
dfEarnings[1,1] <- "Ages"

colnames(dfEarnings) <- dfEarnings[1,]

dfEarnings <- filter(dfEarnings, Ages != "")
dfEarnings <- dfEarnings[-1,]

dfEarnings["Sex"] <- unlist(list(rep("B", nrow(dfEarnings)/3),rep("M", nrow(dfEarnings)/3),rep("F", nrow(dfEarnings)/3)))


##take "number cols"
dfEarningsNums <- select(dfEarnings, Ages, Sex, contains("number"))

##take "pct cols"
dfEarningsPct <- select(dfEarnings, Ages, Sex, contains("percent"))

#rotate
dfEarnNumRot <- gather(dfEarningsNums, Income, Amt, -Ages, -Sex)
dfEarnPctRot <- gather(dfEarningsPct, Income, Pct, -Ages, - Sex)
dfEarnNumPct <- bind_cols(dfEarnNumRot, dfEarnPctRot[length(dfEarnPctRot)])

dfEarnNumPct$Income <- str_replace(dfEarnNumPct$Income, "Number", "")
dfEarnNumPct$Ages <- str_replace(dfEarnNumPct$Ages, "Both sexes|Male|Female", "Total")

head(dfEarnNumPct)







