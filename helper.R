library(plyr)
library(dplyr)
library(tidyr)
library(digest)
library(DBI)
library(RSQLite)
library(curl)
library(RCurl)
library(rjson)
library(jsonlite)
library(httr)
library(stringr)
library(magrittr)
library(shiny)
library(pander)
library(ggplot2)
library(DT)
library(RSQLite)
library(stringr)
library(psych)
library(scales)


db <- dbConnect(SQLite(), dbname= 'Achievement')

TABLES <- dbListTables(db)

sumTables <- str_match_all(TABLES, 'SumData')
keyTables <- str_match_all(TABLES, '_key')

sum_types <- TABLES[which(sumTables == 'SumData')]

assess_types <- TABLES[which(sumTables != 'SumData' & keyTables != '_key')]

gtests <- str_match_all(assess_types, '_G')
gramT <- assess_types[which(gtests == '_G')]

rtests <- str_match_all(assess_types, '_R')
readT <- assess_types[which(rtests == '_R')]

wtests <- str_match_all(assess_types, '_W')
writT <- assess_types[which(wtests == '_W')]

lstests <- str_match_all(assess_types, '_L')
lstspT <- assess_types[which(lstests == '_L')]


dbDisconnect(db)
