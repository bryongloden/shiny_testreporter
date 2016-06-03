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


db <- dbConnect(SQLite(), dbname= 'CESLachievement')

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

Levels <- factor(c(1,2,3,4,5,6))

dbDisconnect(db)

COR <- function(x){
  round(cor(x, use = 'complete.obs'),2)
}

t <- dbReadTable(db, 'SP16_S1_R5')


# Weighted item difficulty
# Weighted item difficulty
t %>%
  select(-c(1:5)) %>%
  filter(!is.na(.[1])) %>%
  mutate_each(funs((.[1:length(.)]/.[1]))) %>%
  slice(-1) -> SLO

  cols <- str_match_all(names(SLO), regex('SLO[0-9]', TRUE))
  
  colnames(SLO)<- t(cols)
  SLO <- t(SLO)
  SLOr <- rownames(SLO)
  SLO <-as.data.frame(SLO)
  SLO$SLO <- SLOr
  
  SLO %>%
    gather(key, value, -SLO, na.rm = TRUE) %>%
    group_by(SLO) %>%
    summarise('N Students' = length(unique(key)),
              'N Items' = length(value)/length(unique(key)),
              Mean = percent(MEAN(value)),
              # Median = (MEDIAN(value)),
              SD = percent(STDEV(value))
              # Min = MIN(value),
              # Max = percent(MAX(value))
              )

  

