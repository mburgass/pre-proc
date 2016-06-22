library(tidyr)
library(dplyr)
Troms_Employment = read_excel("Livelihoods/Employment_Figures/Norway_Mainland/Troms_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
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

Nordland_Employment = read_excel("Livelihoods/Employment_Figures/Norway_Mainland/Nordland_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
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

Finnmark_Employment = read_excel("Livelihoods/Employment_Figures/Norway_Mainland/Finnmark_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
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


