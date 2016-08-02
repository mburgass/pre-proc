library(tidyr)
library(dplyr)
library(readxl)

##Read in exchange rate data
xchange_rate= read_excel("Livelihoods/World_Bank/Exchange_rate.xls", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

xchange_rate<- filter(xchange_rate, `Country Name` %in% c("Canada", "Greenland", "Norway", "Russian Federation")) %>%
  gather("year", "rate", 5:60)
xchange_rate<- select(xchange_rate, `Country Name`, year, rate)%>%
  rename(country= `Country Name`)
xchange_rate<- data.frame(xchange_rate)
xchange_rate$year<- as.integer(xchange_rate$year)

##Read in PPP factor
ppp_factor= read_excel("Livelihoods/World_Bank/PPP_factor.xls", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

ppp_factor<- filter(ppp_factor, `Country Name` %in% c("Canada", "Greenland", "Norway", "Russian Federation")) %>%
  gather("year", "rate", 5:60)
ppp_factor<- select(ppp_factor, `Country Name`, year, rate)%>%
  rename(country= `Country Name`)
ppp_factor<- data.frame(ppp_factor)
ppp_factor$year<- as.integer(ppp_factor$year)

##Read in Canada

canada_gdp= read.csv("Livelihoods/Economies/csv/canada_gdp.csv") ##Canada = 2004 to 2013
canada_rate<- filter(xchange_rate, country == "Canada")
canada_ppp<- filter(ppp_factor, country == "Canada")
canada_ppp<- filter(canada_ppp, year %in% c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
canada_rate<- filter(canada_rate, year %in% c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
canada_gdp$rgn_id<- as.character(canada_gdp$rgn_id)
canada_gdp$sector<- as.character(canada_gdp$sector) ##tidy up data frame.
canada_adjusted= full_join(canada_gdp, canada_ppp, by="year")

canada_adjusted<- mutate(canada_adjusted, value2= value/rate)
canada_adjusted<- select(canada_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)

##Read in Russia
russia_gdp= read.csv("Livelihoods/Economies/csv/Russia_gdp.csv") ##russia = 2008-2014
russia_rate<- filter(xchange_rate, country == "Russian Federation")
russia_ppp<- filter(ppp_factor, country == "Russian Federation")
russia_ppp<- filter(russia_ppp, year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014))
russia_rate<- filter(russia_rate, year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014))
russia_gdp$rgn_id<- as.character(russia_gdp$rgn_id)
russia_gdp$sector<- as.character(russia_gdp$sector) ##tidy up data frame.
russia_adjusted= full_join(russia_gdp, russia_ppp, by="year")

russia_adjusted<- mutate(russia_adjusted, value2= value/rate)
russia_adjusted<- select(russia_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)

##Read in Norway
norway_gdp= read.csv("Livelihoods/Economies/csv/Norway_gdp.csv") ##norway = 2008-2013
norway_rate<- filter(xchange_rate, country == "Norway")
norway_ppp<- filter(ppp_factor, country == "Norway")
norway_ppp<- filter(norway_ppp, year %in% c(2008, 2009, 2010, 2011, 2012, 2013))
norway_rate<- filter(norway_rate, year %in% c(2008, 2009, 2010, 2011, 2012, 2013))
norway_gdp$rgn_id<- as.character(norway_gdp$rgn_id)
norway_gdp$sector<- as.character(norway_gdp$sector) ##tidy up data frame.
norway_adjusted= full_join(norway_gdp, norway_ppp, by="year")

norway_adjusted<- mutate(norway_adjusted, value2= value/rate)
norway_adjusted<- select(norway_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)

## Read in Greenland
greenland_gdp= read.csv("Livelihoods/Economies/csv/Greenland_gdp.csv") ##greenland = 2003-2013
greenland_rate<- filter(xchange_rate, country == "Greenland")
greenland_ppp<- filter(ppp_factor, country == "Greenland")
greenland_ppp<- filter(greenland_ppp, year %in% c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
greenland_rate<- filter(greenland_rate, year %in% c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
greenland_gdp$rgn_id<- as.character(greenland_gdp$rgn_id)
greenland_gdp$sector<- as.character(greenland_gdp$sector) ##tidy up data frame.
greenland_adjusted= full_join(greenland_gdp, greenland_ppp, by="year")

greenland_adjusted<- mutate(greenland_adjusted, value2= value/rate)
greenland_adjusted<- select(greenland_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)