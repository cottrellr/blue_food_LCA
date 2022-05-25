library(here)
library(tidyverse)

#Emissions calculations for Farmed Prawns

#GHG Produced by Farmed Prawns

lca_data <- read_csv(here("Outputs/lca_model_data_no-summary.csv")) %>% filter(taxa == "shrimp") %>% select(study_id, iso3c, clean_sci_name, taxa, intensity, system, total_ghg ) %>% arrange(iso3c)

(lca_by_country <- lca_data %>% 
    group_by(iso3c,clean_sci_name) %>% 
    summarise(mean_ghg = mean(total_ghg),
              sd_ghg = sd(total_ghg)))

(total_ghg <- lca_data %>% 
    summarise(mean_ghg = mean(total_ghg),
              sd_ghg = sd(total_ghg)) %>% 
    add_column(iso3c = "Farmed") %>% 
    select(iso3c, mean_ghg, sd_ghg))

#GHG Produced by Wild-caught Prawns

wild_ghg_data <- read_csv(here("Outputs/wild_data_GHG_Model.csv")) %>% filter(taxa == "Shrimps") %>% select(species, gear, ghg, clean_sci_name, taxa)


(wild_ghg <- wild_ghg_data %>% 
    group_by(clean_sci_name) %>% 
    summarise(mean_ghg = mean(ghg),
              sd_ghg = sd(ghg)) %>% add_column(iso3c = "Wild_Caught")) %>% relocate(iso3c)


(total_ghg_wild <- wild_ghg_data %>% 
    summarise(mean_ghg = mean(ghg),
              sd_ghg = sd(ghg)) %>%
    add_column(iso3c = "Wild") %>% select(iso3c,mean_ghg))


#Combine both to be side by side

GHG_data <- rbind(lca_by_country,wild_ghg)

#Mean for each country (Farmed Prawns Only)

Country_Mean_1 <- lca_data %>% group_by(iso3c) %>% summarise(mean_ghg = mean(total_ghg))

#Overall mean for other countries (Farmed Prawns)

target <- c("BGD","IDN","THA","VNM")

#We use China as an average as they represent a range of different prawn farming systems (Intensive & Semi-intensive)

Country_mean_2 <- read_csv(here("Outputs/lca_model_data_no-summary.csv"))%>% filter(taxa == "shrimp") %>% filter(!iso3c %in% target) %>% 
  select(study_id, iso3c, clean_sci_name, taxa, intensity, system, total_ghg ) %>% arrange(iso3c) %>% group_by(iso3c) %>% summarise(mean_ghg = mean(total_ghg))%>%
  mutate(iso3c = replace(iso3c, iso3c == "CHN", "Other_Countries_Farmed"))

Country_Mean_Farmed <- rbind(Country_Mean_1,Country_mean_2)

Mean_GHG_farmed <- Country_Mean_Farmed %>% summarise(mean_ghg = mean(mean_ghg)) %>% mutate(iso3c = "Farmed") %>% select(iso3c,mean_ghg)

#combine with wild caught

GHG_EF <- bind_rows(total_ghg_wild,Mean_GHG_farmed)

#Transportation

#Add in emissions factors from the Fourth IMO GHG Study, we used values from 2013 for container ships.

#Link: https://wwwcdn.imo.org/localresources/en/OurWork/Environment/Documents/Fourth%20IMO%20GHG%20Study%202020%20-%20Full%20report%20and%20annexes.pdf

#Add in redundant trade values per country with Australia 

transport_distance_volume_spp <- read_csv("data/Transportation_Distance_Per_Species.csv")

emissions_factors <- read_csv("data/Carrier_Size_GHG.csv")

transport_distance_volume_spp_list <- transport_distance_volume_spp %>% 
  group_split(iso3c, species_name, prod_method, quantity, distance)


transport_output_list <- list()
total_output_list <- list()

calc_df %>% select(iso3c) %>% unique()

set.seed(333)

for(i in 1:500){
  
  ef <- runif(n=1, min = min(emissions_factors$mean_ef), max = max(emissions_factors$mean_ef))
  
  (calc_df <- transport_distance_volume_spp %>% 
    mutate(ef = ef) %>% 
    mutate(emissions_nautical_mile = quantity*distance*ef*2) %>% 
    mutate(total_emissions = emissions_nautical_mile*0.54/1000) %>% 
    mutate(rep = i))
  
  output_df_list[[i]] <- calc_df

#add in production data
  
Country_Mean_Farmed <- Country_Mean_Farmed %>% mutate(prod_method = "Farmed")
  
redundant_trade_emissions <- merge(Country_Mean_Farmed,calc_df, by = c("iso3c","prod_method"), all.y = T) %>% view()
  
#Add in production GHG for wild caught

total_ghg_wild <- total_ghg_wild[,c(1,3,2)]

#remove the iso3c

total_ghg_wild <- select(total_ghg_wild, -c(iso3c))

redundant_trade_emissions <- left_join(redundant_trade_emissions,total_ghg_wild, by = c("prod_method")) %>% view()

redundant_trade_emissions <- redundant_trade_emissions %>% unite(mean_ghg,c("mean_ghg.y", "mean_ghg.x"), na.rm = TRUE) %>% view()

#fill blanks 

redundant_trade_emissions <-redundant_trade_emissions %>% mutate(mean_ghg = ifelse(mean_ghg %in% "", 5647.98129728448
,mean_ghg)) %>% view()

  
  total_output_list[[i]] <- redundant_trade_emissions
  
}
  
write_csv(x = redundant_trade_emissions, file = here("data/output/emissions_output_w_reps.csv"))

#Emission per species
emissions_per_species <-redundant_trade_emissions %>% 
  group_by(species_name, prod_method) %>% 
  summarise(species_emission = sum(total_emissions, na.rm= TRUE)) %>% view()

sum(emissions_per_species$species_emission)

#Total emissions produced from transportation is 4730576 kg CO2

redundant_trade_emissions %>% 
  group_by(species_name, prod_method) %>% 
  summarise(estimate = mean(total_emissions, na.rm= TRUE),
            sd = sd(total_emissions, na.rm = TRUE)) %>% view()

#Plots

#In the interest of time, I might just plot using excel first instead of R
  