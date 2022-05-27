# get the current location
#library(here)
#here::set_here()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd(getSrcDirectory()[1])
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)


#Sys.setlocale(category = "LC_ALL", locale = "zh_TW")

# get my GA account
library(googleAnalyticsR)
library(googleAuthR)
library(tidyverse)
library(TTR) # for moving average
library(writexl)
library(readxl)
library(ggpubr) # combine ggplot
library(highcharter) # highchart plot
library(googlesheets4)
library(jsonlite)
#ga_auth()
#ga_account_list("ga4")
#gar_deauth()
gar_auth(token = readRDS("token.rds"))
#token <- ga_auth()
#saveRDS(token, file = "token.rds")
options(gargle_oauth_email = TRUE)

#gs4_auth()

# google sheet id
ss <- "1hTfspCDSNFCLp5qOlayq0wwr8gJfTVBbAyWsKaR7Ai8"

# import wix data
wix_page_view <- read_xlsx("wix_page_view.xlsx") %>% mutate(date = as.Date(date), month = str_pad(month, 2, "left", "0"), year = as.character(year))
wix_page_view_unique <- read_xlsx("wix_page_view_unique.xlsx") %>% mutate(date = as.Date(date), month = str_pad(month, 2, "left", "0"), year = as.character(year))

# import recovery JSON (April-01 - 27)
recovery <- fromJSON("goaccess-1653459380295.json")[["visitors"]][["data"]][1:27,] %>% 
  mutate(date = lubridate::ymd(data), month = str_pad((lubridate ::month(date)), 2, "left", "0"), year = as.character(lubridate ::year(date)), eventName = "page_view", eventCount = visitors["count"]) %>%
  select(date, year, month, eventCount, eventName)
recovery_pageview <- do.call(data.frame, recovery) %>% mutate(eventCount = count) %>% select(-count)

# raw data
data <- ga_data(ga_account_list("ga4")$propertyId,
        date_range = c("2022-01-13", "today"),
        metrics = c("eventCount","eventCountPerUser","activeUsers","userEngagementDuration"),
        dimensions = c("date","pageTitle","fullPageUrl","pagePath","eventName","hostName"),
        dim_filters = ga_data_filter(hostName == c("refresh.bokss.org.hk", "www.refresh.bokss.org.hk")),
        limit = -1)

# clean raw data
data_1 <- data %>% arrange(date) %>% mutate(pageTitle = str_replace(pageTitle, pattern = "平臺", replacement = "平台"), month = str_pad((lubridate ::month(date)), 2, "left", "0"), year = as.character(lubridate ::year(date))) %>% group_by(date, pageTitle)

# separate different event
page_view <- data_1 %>% filter(eventName == "page_view") %>% group_by(date, month, year, eventName) %>% summarize(eventCount = sum(eventCount), activeUsers = sum(activeUsers)) %>% mutate(eventCountPerUser = eventCount / activeUsers) %>% 
  arrange(date, desc(eventCount)) %>% bind_rows(wix_page_view) %>% ungroup() 

page_view_unique <- data_1 %>% filter(eventName == "first_visit") %>% group_by(date, month, year, eventName) %>% summarize(eventCount = sum(eventCount), activeUsers = sum(activeUsers)) %>% mutate(eventCountPerUser = eventCount / activeUsers) %>% 
  arrange(date, desc(eventCount)) %>% bind_rows(wix_page_view_unique) %>% ungroup() 

page_view_time <- data_1 %>% filter(eventName == "user_engagement") %>% group_by(date, month, year, eventName) %>% summarize(eventCount = sum(eventCount), activeUsers = sum(activeUsers), userEngagementDuration = sum(userEngagementDuration)) %>% 
  mutate(eventCountPerUser = eventCount / activeUsers, average_by_event = userEngagementDuration / eventCount / 60, average_by_user = userEngagementDuration / activeUsers / 60) %>% arrange(date, desc(eventCount)) %>% ungroup()

entrance <- data_1 %>% filter(eventName == "session_start") %>% group_by(date, month, year, eventName) %>% summarize(eventCount = sum(eventCount), activeUsers = sum(activeUsers)) %>% mutate(eventCountPerUser = eventCount / activeUsers) %>% 
  arrange(date, desc(eventCount)) %>% ungroup()

#moving_average = runMean(page_view$eventCount)

############ monthly data ###############
page_view_month <- page_view %>% bind_rows(recovery_pageview) %>% group_by(month, year, eventName) %>% summarize(eventCount = sum(eventCount)) %>% ungroup() %>% arrange(year, month) %>% mutate(
  previous = lag(eventCount), 
  change = eventCount - previous,
  changePercentage  = (change/previous) * 100,
  year.month = as.character(str_c(year, month, sep = "/"))) %>% rowid_to_column()

page_view_unique_month <- page_view_unique %>% group_by(month, year, eventName) %>% summarize(eventCount = sum(eventCount)) %>% ungroup() %>% arrange(year, month) %>% mutate(
  previous = lag(eventCount), 
  change = eventCount - previous,
  changePercentage  = (change/previous) * 100,
  year.month = as.character(str_c(year, month, sep = "/"))) %>% rowid_to_column()

#combined different event
participant_frequency_long <- page_view_month %>% bind_rows(page_view_unique_month)
write_csv(participant_frequency_long, "ga_month_data_long.csv")
write_sheet(participant_frequency_long, ss = ss, sheet = "participant_frequency_long")

participant_frequency_wide <- page_view_month %>% left_join(page_view_unique_month, by = c("month", "year", "year.month", "rowid"), suffix = c(".frequency", ".participant")) %>% select(year, everything(), year.month, -rowid, -eventName.frequency, -eventName.participant)
write_csv(participant_frequency_wide, "ga_month_data_wide.csv") %>% sheet_write(ss = ss, sheet = "participant_frequency_wide")

#render the rmd
Sys.setlocale(category = "LC_ALL", locale = "cht") # windows
Sys.setenv(RSTUDIO_PANDOC="C:/Users/chankalong/AppData/Local/Programs/RStudio/bin/pandoc")
rmarkdown::render("refresh_website_usage.Rmd",encoding = "UTF-8")
