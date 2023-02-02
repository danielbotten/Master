
#Sending query to wrds
rm(list=ls()) 
library(RPostgres)
library(dplyr, warn.conflicts = FALSE)
Sys.setenv(PGUSER="danielbotten")
Sys.setenv(PGPASSWORD=askpass::askpass())
Sys.setenv(PGDATABASE="wrds",PGPORT=9737,
           PGHOST="wrds-pgdata.wharton.upenn.edu")
wrds <- dbConnect(RPostgres::Postgres())

#Extracting data from Wharton

#1. Company identifiers

Company_identifiers <-tbl(wrds, sql("SELECT * FROM reprisk_std.std_company_identifiers"))

Company_identifiers <- as.data.frame(Company_identifiers)

#2. Rep Risk index 

RRI <-tbl(wrds, sql("SELECT * FROM reprisk_std.std_rri_data "))

RRI <- as.data.frame(RRI)

#3. News data

news <-tbl(wrds, sql("SELECT * FROM reprisk_std.std_news_data"))

news <- as.data.frame(news)

test <- news %>% 
  group_by(reprisk_story_id, 
           reprisk_substory_id, 
           news_date, 
           related_issues, 
           source_language, 
           related_topic_tags, 
           related_ungc_principles, 
           related_countries, 
           related_countries_codes, 
           severity, novelty, reach) %>%
  summarise(n = n())


#Merging company identifiers to  news to see which companies

name_id_HQ_URL <- Company_identifiers[,c(1:3,6)]
news1 <- merge(name_id_HQ_URL, news,by ="reprisk_id")
news <- news1

#Group by US firms
news1 <- news1[news1$news_date == "2019-01-15",]
news <- news[news$headquarter_country == "United States of America",]
t <- news1[,c("name","news_date","reprisk_story_id","reprisk_substory_id","related_issues","severity","novelty","reach")]
#Group to count # of firms in USA reprisk data

groupedUSA <- news %>% 
  group_by(name) %>% 
  summarise(sum = n()) #OBS: n are not individual events 
                        #due to several subevents under the same main event


#Group severity from 1-3 by name

Grouped_severity <- news %>% 
  group_by(name,severity,reach) %>%
  summarize(n = n())

#####Group severity by 1-3 which can be tested as dependent variable####

#Group by severity 1

severity1 <- news[news$severity == 1,]
summary(severity1)

#Group by severity 2

severity2 <- news[news$severity == 2,]
summary(severity2)

#Group by severity 3

severity3 <- news[news$severity == 3,]
summary(severity3)

######### Making summary statistics ##########
#RRI summary

RRI <- merge(name_id_HQ_URL, RRI,by ="reprisk_id")
RRI_sum <- RRI %>%
  select(-c(name, headquarter_country,url,reprisk_id,date))

library(kableExtra)
library(summarytools)
library(gt)
view(dfSummary(RRI_sum))

