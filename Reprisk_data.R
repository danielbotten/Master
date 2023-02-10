
#Sending query to wrds
rm(list=ls()) 
library(RPostgres)
require(plm)
require(stargazer)
require(DescTools)
require(lmtest)
require(sandwich)
require(ggplot2)
require(dplyr)
require(MatchIt)
require(tidyr)
library(stringdist)
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

#RRI <-tbl(wrds, sql("SELECT * FROM reprisk_std.std_rri_data "))

#RRI <- as.data.frame(RRI)

#3. News data

news <-tbl(wrds, sql("SELECT * FROM reprisk_std.std_news_data"))

news <- as.data.frame(news)

grouped <- news %>% 

#Merging company identifiers to  news to see which companies - News

name_id_HQ_URL <- Company_identifiers[,c(1:3,5,6,8)]
news1 <- merge(name_id_HQ_URL, news,by ="reprisk_id")
news <- news1

#Filter for public parent companies

res <- dbSendQuery(wrds, "select a.gvkey, a.sic, a.isin, b.reprisk_id, 
b.primary_isin, b.name, b.headquarter_country, 
b.headquarter_country_code, b.sectors, b.url
                   from compg.g_names a join reprisk.std_company_identifiers b
                   on a.isin = b.primary_isin")
data <- dbFetch(res, n = -1)
dbClearResult(res)
data

news <- news[is.na(news$primary_isin),]

grouped <- news %>% 
  group_by(name) %>% 
  summarise(n = n())
#Limit the data for severity 2 & 3

news <- news[news$severity == 3 | news$severity == 2,]

#Limit the data for reach 2 & 3

news <- news[news$reach == 3 | news$reach == 2,]


#Investigate the Reprisk data # of incidents

news$year <- format(as.Date(news$news_date, format="%d/%m/%Y"),"%Y")

GroupedSeverity <- news %>% 
  group_by(severity,year) %>% 
  summarise(Incidents = n())

GroupedSeverity$severity <- as.factor(GroupedSeverity$severity)

p<- ggplot(data = GroupedSeverity, aes(x = year, y = Incidents, fill = severity))+ 
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("lightblue", "blue")) +
  theme_classic()+
  scale_y_continuous(name="Number of Incidents") +
  labs(fill="Severity", title= "Total numbers of RepRisk Incidents with severity 2 and 3")+
  labs(x="Year")
p

#Group to count # of firms in the reprisk data

numberofFirms <- news %>% 
  group_by(name, url) %>% 
  summarise(sum = n()) #OBS: n are not individual events 
                        #due to several subevents under the same main event

#Fill in dates to balance data and we get dates where no incidents has happened
news$news_date <- format(as.Date(news$news_date, format="%d/%m/%Y"))

news$news_date <- as.Date(news$news_date)
news_balanced <- news %>%
  group_by(name) %>%
  complete(news_date = seq(from = as.Date("2007-01-03"), to = as.Date("2020-12-30"), by = "1 day"))

#Fill in NA's with 0, indicating no severe event has happened

news_balanced$severity[is.na(news_balanced$severity)] <- 0
news_balanced$reach[is.na(news_balanced$reach)] <- 0

#Extract year for new observations
news_balanced$year <- format(as.Date(news_balanced$news_date, format="%d/%m/%Y"),"%Y")

#Declare panel data
news_panel <- pdata.frame(news_balanced, index=c("name","news_date"))
pdim(news_panel)


#Filter for companies in EDGARD

load("EDGAR_all_companies.Rdata")

allcompaniesEDGAR <- df_all_companies
rm(df_all_companies)


