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
Troms_Group2 = gather(Troms_Group, "Year", "Marine.Jobs", 3:8)
Troms_Group3 = select(Troms_Group2, -Job.Type)
Troms_Group3
