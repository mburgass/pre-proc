library(tidyr)
library(dplyr)
library(readxl)

##Read in exchange rate data
xchange_rate= read_excel("Livelihoods/World_Bank/Exchange_rate.xls", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

xchange_rate<- filter(xchange_rate, `Country Name` %in% c("Canada", "Greenland", "Norway", "Russian Federation")) %>%
  gather("year", "rate", 5:60)
xchange_rate<- select(xchange_rate, `Country Name`, year, rate)%>%
  rename(rgn_id= `Country Name`)

##Read in Canada

canada_gdp= read.csv("Livelihoods/Economies/csv/canada_gdp.csv") ##Canada = 2004 to 2013
canada_rate<- filter(xchange_rate, rgn_id == "Canada")
canada_rate<- filter(canada_rate, year %in% c(2004, 20005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
