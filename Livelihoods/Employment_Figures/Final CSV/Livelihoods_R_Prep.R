library(tidyr)
library(dplyr)
Troms_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Troms_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Troms_Employment2 = data.frame(Troms_Employment)
Troms_Fish = Troms_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Troms_Food = Troms_Employment2 %>% filter(Job.Type == "10 Food products")
Troms_Water = Troms_Employment2 %>% filter(Job.Type == "50 Water transport")
Troms_Travel = Troms_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related
# Join together
Troms_Group = rbind(Troms_Fish, Troms_Food, Troms_Water, Troms_Travel)
Troms_Group2 = select(Troms_Group, -Job.Type)
Troms_Group2 = gather(Troms_Group, "Year", "Marine.Jobs", 3:8)
Troms_Group3 = select(Troms_Group2, -Job.Type)
Troms_Group3
Troms_Jobs = Troms_Group3 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(total_jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Troms_Jobs$Region <- "Troms"
troms_jobs_final=Troms_Jobs[c(3,1,2)]
troms_jobs_final

# Nordland Jobs -----------------------------------------------------------

Nordland_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Nordland_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nord_Employment2 = data.frame(Nordland_Employment)
Nord_Fish = Nord_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Nord_Food = Nord_Employment2 %>% filter(Job.Type == "10 Food products")
Nord_Water = Nord_Employment2 %>% filter(Job.Type == "50 Water transport")
Nord_Travel = Nord_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Nord_Group = rbind(Nord_Fish, Nord_Food, Nord_Water, Nord_Travel)
Nord_Group2 = select(Nord_Group, -Job.Type)
Nord_Group2 = gather(Nord_Group, "Year", "Marine.Jobs", 3:8)
Nord_Group3 = select(Nord_Group2, -Job.Type)
Nord_Group3

Nord_Jobs = Nord_Group3 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(total_jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Nord_Jobs$Region <- "Nordland"
Nord_jobs_final=Nord_Jobs[c(3,1,2)]
Nord_jobs_final

# Finnmark Jobs -----------------------------------------------------------

Finnmark_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Finnmark_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Finn_Employment2 = data.frame(Finnmark_Employment)
Finn_Fish = Finn_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Finn_Food = Finn_Employment2 %>% filter(Job.Type == "10 Food products")
Finn_Water = Finn_Employment2 %>% filter(Job.Type == "50 Water transport")
Finn_Travel = Finn_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Finn_Group = rbind(Finn_Fish, Finn_Food, Finn_Water, Finn_Travel)
Finn_Group2 = select(Finn_Group, -Job.Type)
Finn_Group2 = gather(Finn_Group, "Year", "Marine.Jobs", 3:8)
Finn_Group3 = select(Finn_Group2, -Job.Type)
Finn_Group3

Finn_Jobs = Finn_Group3 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(total_jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Finn_Jobs$Region <- "Finnmark"
Finn_jobs_final=Finn_Jobs[c(3,1,2)]
Finn_jobs_final

# Norway Mainland Jobs ----------------------------------------------------

norway_main_jobs = rbind(troms_jobs_final, Nord_jobs_final, Finn_jobs_final)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(total_jobs, na.rm=T))%>%
  ungroup()
norway_main_jobs$Region <- "Norway Mainland"
norway_mainland_jobs=norway_main_jobs[c(3,1,2)]
norway_mainland_jobs

# Svalbard ----------------------------------------------------------------
Svalbard_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Svalbard_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Sval_Employment2 = data.frame(Svalbard_Employment)%>%
  rename(c("Sector"="Job.Type"))
Sval_Employment2<-filter(Sval_Employment2, !(Job.Type %in% c('B Mining and quarrying', 'C-D-E Manufacturing', 'F Construction',
                                                             'G Wholesale and retail trade: repair of motor vehicles and motorcycles',
                                                             'J-K Information and communication', 'L Real estate activities',
                                                             'M Professional, scientific and technical activities', 'N Administrative and support service activities',
                                                             'O Public administration and defence', 'Q Human health and social work activities',
                                                             'S Other service activities', '00 Unspecified')))
## Ocean based employment considering main ocean related employment on Svalbard is tourism and science.

# Join together

Sval_Employment2<-gather(Sval_Employment2, "Year", "Marine.Jobs", 2:8)
Sval_Employment2<-select(Sval_Employment2, -Job.Type)
Sval_Employment2

Sval_Jobs = Sval_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Sval_Jobs$Region <- "Svalbard"
sval_jobs_final=Sval_Jobs[c(3,1,2)]
sval_jobs_final

# Norway Jobs bind --------------------------------------------------------


norway_jobs = rbind(sval_jobs_final, norway_mainland_jobs)
norway_jobs$Marine.Jobs<- round(norway_jobs$Marine.Jobs, digits=0)
norway_jobs
write.csv(norway_jobs, "Norway_Jobs.csv")

# Canada ------------------------------------------------------------------


# NWT ---------------------------------------------------------------------
NWT_Employment = read.csv('Livelihoods/Employment_Figures/Canada/NWT_Employment_2001_2015.csv')
NWT_Employment2 = data.frame(NWT_Employment)
NWT_Employment2<-dplyr::filter(NWT_Employment2, (Job.Type %in% c("Transportation and warehousing", "Accommodation and food services", "Information, culture and recreation")))
## Assume related to ship transport, tourism, ship business

NWT_Employment2<-gather(NWT_Employment2, "Year", "Marine.Jobs", 2:16)
NWT_Employment2<-select(NWT_Employment2, -Job.Type)
NWT_Employment2

NWT_Jobs = NWT_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
NWT_Jobs$Region <- "Beaufort"
NWT_jobs_final=NWT_Jobs[c(3,1,2)]
NWT_jobs_final

##Beaufort Delta area accounts for ~14.2% of NWT employment based on data from
##C:\Users\MB4514\Documents\github\pre-proc\Livelihoods\Employment_Figures\Raw files\Canada\Community Labour Force Activity, 1986 to 2014.xlsx
##Now calculate Beaufort Delta region based on 14.2% of NWT_jobs_final

NWT_jobs_final=dplyr::mutate(NWT_jobs_final, Marine.Jobs = (NWT_jobs_final$Marine.Jobs/100)*14.2)
NWT_jobs_final$Marine.Jobs<- round(NWT_jobs_final$Marine.Jobs, digits=0)

# Nunavut -----------------------------------------------------------------

Nunavut_Employment = read_excel("Livelihoods/Employment_Figures/Canada/Nunavut_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nunavut_Employment<-data.frame(Nunavut_Employment)
Nunavut_Employment2 = dplyr::filter(Nunavut_Employment, (Job.Type %in% c("Fishing, Hunting, Trapping, Mining and Quarrying", "Transportation and Warehousing", "Accommodation and Food Services")))
## Job types quite general - taken ones with most likely marine connections.

Nunavut_Employment2<-gather(Nunavut_Employment2, "Year", "Marine.Jobs", 2:8)
Nunavut_Employment2<-select(Nunavut_Employment2, -Job.Type)
Nunavut_Jobs = Nunavut_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%
  select(Year, Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Nunavut_Jobs
Nunavut_Jobs$Region <- "Nunavut"
Nunavut_jobs_final=Nunavut_Jobs[c(3,1,2)]
Nunavut_jobs_final


# Canada Join -------------------------------------------------------------

canada_jobs = rbind(NWT_jobs_final, Nunavut_jobs_final)
canada_jobs
write.csv(canada_jobs, "Canada_Jobs.csv")

# Greenland ---------------------------------------------------------------

## Only figures for whole country
Greenland_Employment = read_excel("Livelihoods/Employment_Figures/Greenland/Greenland_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Greenland_Employment<-data.frame(Greenland_Employment)
Greenland_Employment2 = dplyr::filter(Greenland_Employment, (Job.Type %in% c("Fishing, hunting & agriculture", "Transportation", "Hotels and restaurants")))
##Jobs quite general - kept with theme of fishing/ship transport and tourism
Greenland_Employment2<-select(Greenland_Employment2, -Job.Type)
Greenland_Jobs = Greenland_Employment2 %>% select(Year, Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(Jobs, na.rm=T))%>%
  ungroup()
Greenland_Jobs$Region <- "Greenland"
Greenland_jobs_final=Greenland_Jobs[c(3,1,2)]
Greenland_jobs_final

##Check how best to separate out between east and west Greenland? Roughly 80% of people live in West Greenland

write.csv(Greenland_jobs_final, "Greenland_Jobs.csv")

# USA/Alaska --------------------------------------------------------------


# 2014 --------------------------------------------------------------------

Alaska_Employment14 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment14<-data.frame(Alaska_Employment14)
Alaska_Employment14 = dplyr::filter(Alaska_Employment, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment14 <- dplyr::tbl_df(Alaska_Employment14)
Alaska_Employment14 <- dplyr::select(Alaska_Employment14, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment14 <- dplyr::filter(Alaska_Employment14, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment14 <- dplyr::filter(Alaska_Employment14, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment14

##Not really any marine jobs listed in these areas. National figures don't seem to be matched by the regional figures though..?

# 2013 --------------------------------------------------------------------
Alaska_Employment13 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment13<-data.frame(Alaska_Employment13)
Alaska_Employment13 = dplyr::filter(Alaska_Employment13, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment13 <- dplyr::tbl_df(Alaska_Employment13)
Alaska_Employment13 <- dplyr::select(Alaska_Employment13, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment13 <- dplyr::filter(Alaska_Employment13, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment13 <- dplyr::filter(Alaska_Employment13, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment13


# 2012 --------------------------------------------------------------------
Alaska_Employment12 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment12<-data.frame(Alaska_Employment12)
Alaska_Employment12 = dplyr::filter(Alaska_Employment12, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment12 <- dplyr::tbl_df(Alaska_Employment12)
Alaska_Employment12 <- dplyr::select(Alaska_Employment12, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment12 <- dplyr::filter(Alaska_Employment12, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment12 <- dplyr::filter(Alaska_Employment12, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment12

# 2011 --------------------------------------------------------------------
Alaska_Employment11 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 6, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment11<-data.frame(Alaska_Employment11)
Alaska_Employment11 = dplyr::filter(Alaska_Employment11, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment11 <- dplyr::tbl_df(Alaska_Employment11)
Alaska_Employment11 <- dplyr::select(Alaska_Employment11, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment11 <- dplyr::filter(Alaska_Employment11, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment11 <- dplyr::filter(Alaska_Employment11, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
##Note slight difference in Job types i.e. use of & rather than and. To correct later
Alaska_Employment11

# 2010 --------------------------------------------------------------------
Alaska_Employment10 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment10<-data.frame(Alaska_Employment10)
Alaska_Employment10 = dplyr::filter(Alaska_Employment10, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment10 <- dplyr::tbl_df(Alaska_Employment10)
Alaska_Employment10 <- dplyr::select(Alaska_Employment10, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment10 <- dplyr::filter(Alaska_Employment10, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment10 <- dplyr::filter(Alaska_Employment10, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment10

# 2009 --------------------------------------------------------------------
Alaska_Employment09 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 8, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment09<-data.frame(Alaska_Employment09)
Alaska_Employment09 = dplyr::filter(Alaska_Employment09, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment09 <- dplyr::tbl_df(Alaska_Employment09)
Alaska_Employment09 <- dplyr::select(Alaska_Employment09, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment09 <- dplyr::filter(Alaska_Employment09, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment09 <- dplyr::filter(Alaska_Employment09, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment09

# 2008 --------------------------------------------------------------------
Alaska_Employment08 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 9, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment08<-data.frame(Alaska_Employment08)
Alaska_Employment08 = dplyr::filter(Alaska_Employment08, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment08 <- dplyr::tbl_df(Alaska_Employment08)
Alaska_Employment08 <- dplyr::select(Alaska_Employment08, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment08 <- dplyr::filter(Alaska_Employment08, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment08 <- dplyr::filter(Alaska_Employment08, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment08

# 2007 --------------------------------------------------------------------
Alaska_Employment07 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 10, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment07<-data.frame(Alaska_Employment07)
Alaska_Employment07 = dplyr::filter(Alaska_Employment07, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment07 <- dplyr::tbl_df(Alaska_Employment07)
Alaska_Employment07 <- dplyr::select(Alaska_Employment07, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment07 <- dplyr::filter(Alaska_Employment07, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment07 <- dplyr::filter(Alaska_Employment07, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment07

# 2006 --------------------------------------------------------------------
Alaska_Employment06 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 11, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment06<-data.frame(Alaska_Employment06)
Alaska_Employment06 = dplyr::filter(Alaska_Employment06, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment06 <- dplyr::tbl_df(Alaska_Employment06)
Alaska_Employment06 <- dplyr::select(Alaska_Employment06, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment06 <- dplyr::filter(Alaska_Employment06, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment06 <- dplyr::filter(Alaska_Employment06, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment06

# 2005 --------------------------------------------------------------------
Alaska_Employment05 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment05<-data.frame(Alaska_Employment05)
Alaska_Employment05 = dplyr::filter(Alaska_Employment05, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment05 <- dplyr::tbl_df(Alaska_Employment05)
Alaska_Employment05 <- dplyr::select(Alaska_Employment05, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment05 <- dplyr::filter(Alaska_Employment05, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment05 <- dplyr::filter(Alaska_Employment05, (NA. %in% c("NATURAL RESOURCE & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment05
##Slight name change in Resource rather than Resources

# 2004 --------------------------------------------------------------------
Alaska_Employment04 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment04<-data.frame(Alaska_Employment04)
Alaska_Employment04 = dplyr::filter(Alaska_Employment04, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment04 <- dplyr::tbl_df(Alaska_Employment04)
Alaska_Employment04 <- dplyr::select(Alaska_Employment04, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment04 <- dplyr::filter(Alaska_Employment04, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment04 <- dplyr::filter(Alaska_Employment04, (NA. %in% c("NATURAL RESOURCE & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment04

