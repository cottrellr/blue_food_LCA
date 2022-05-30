library(here)
library(tidyverse)
library(data.table)

#Emissions calculations for Farmed Prawns

#GHG Produced by Farmed Prawns

lca_data <- read_csv(here("Outputs/lca_model_data_no-summary.csv")) %>% filter(taxa == "shrimp") %>% select(study_id, iso3c, clean_sci_name, taxa, intensity, system, total_ghg ) %>% arrange(iso3c)

(lca_by_country <- lca_data %>% 
    group_by(iso3c,clean_sci_name) %>% 
    summarise(mean_ghg = mean(total_ghg),
              sd_ghg = sd(total_ghg)) %>% mutate(sd_ghg = if_else(is.na(sd_ghg), 0, sd_ghg)))

#Here three rows display a standard deviation of 0 as they all have the same GHG emissions values, we will leave it as zero

(total_ghg <- lca_data %>% 
    summarise(mean_ghg = mean(total_ghg),
              sd_ghg = sd(total_ghg)) %>% 
    add_column(iso3c = "Farmed") %>% 
    select(iso3c, mean_ghg, sd_ghg))

#GHG Produced by Wild-caught Prawns

wild_ghg_data <- read_csv(here("Outputs/wild_data_GHG_Model.csv")) %>% filter(taxa == "Shrimps") %>% select(species, gear, ghg, clean_sci_name, taxa)


(total_ghg_wild <- wild_ghg_data %>% 
    summarise(mean_ghg = mean(ghg),
              sd_ghg = sd(ghg)) %>%
    add_column(iso3c = "Wild") %>% select(iso3c,mean_ghg,sd_ghg))


#The reason why we take the mean and sd of all prawn species involved is due to the fact that they contain prawns that are not included in redundant trade

#Mean for each country (Farmed Prawns Only)

Country_Mean_1 <- lca_data %>% group_by(iso3c) %>% summarise(mean_ghg = mean(total_ghg), sd_ghg = sd(total_ghg))

#Overall mean for other countries (Farmed Prawns)

target <- c("BGD","IDN","THA","VNM")

#We use China as an average as they represent a range of different prawn farming systems (Intensive & Semi-intensive)

Country_mean_2 <- read_csv(here("Outputs/lca_model_data_no-summary.csv"))%>% filter(taxa == "shrimp") %>% filter(!iso3c %in% target) %>% 
  select(study_id, iso3c, clean_sci_name, taxa, intensity, system, total_ghg ) %>% arrange(iso3c) %>% group_by(iso3c) %>% summarise(mean_ghg = mean(total_ghg), sd_ghg = sd(total_ghg))%>%
  mutate(iso3c = replace(iso3c, iso3c == "CHN", "Other_Countries_Farmed"))

Country_Mean_Farmed <- rbind(Country_Mean_1,Country_mean_2)

#Not sure if I should be taking the mean of SD or SD of SD here. But first i'll go with SD

Mean_GHG_farmed <- Country_Mean_Farmed %>% summarise(mean_ghg = mean(mean_ghg), sd_ghg = sd(sd_ghg)) %>% mutate(iso3c = "Farmed") %>% select(iso3c,mean_ghg,sd_ghg)

#combine with wild caught

GHG_EF <- bind_rows(total_ghg_wild,Mean_GHG_farmed)

#Transportation

#Add in emissions factors from the Fourth IMO GHG Study, we used values from 2017 for container ships.

#Link: https://wwwcdn.imo.org/localresources/en/OurWork/Environment/Documents/Fourth%20IMO%20GHG%20Study%202020%20-%20Full%20report%20and%20annexes.pdf


transport_distance_volume_spp <- read_csv("data/Transportation_Distance_Per_Species.csv")

emissions_factors <- read_csv("data/Carrier_Size_GHG.csv")

transport_distance_volume_spp_list <- transport_distance_volume_spp %>% 
  group_split(iso3c, species_name, prod_method, quantity, distance)


transport_output_list <- list()
total_output_list <- list()

set.seed(333)

for(i in 1:500){
  
  ef <- runif(n=1, min = min(emissions_factors$mean_ef), max = max(emissions_factors$mean_ef))
  
  (calc_df <- transport_distance_volume_spp %>% 
    mutate(ef = ef) %>% 
    mutate(emissions_nautical_mile = quantity*distance*ef*2) %>% 
    mutate(transport_emissions = emissions_nautical_mile*0.54/1000) %>% mutate(rep = i))
  
}

write.csv(calc_df, file = "data/transport_emissions.csv")

#add in production data
  
Country_Mean_Farmed <- Country_Mean_Farmed %>% mutate(prod_method = "Farmed")

#Add in GHG values for missing countries based on values from Other_Countries_Farmed

ghg_values_other_countries_production <- data.table(iso3c = c("IND","JPY", "MYS","SGP"), mean_ghg = c(5648,5648,5648,5648), sd_ghg = c(1973,1973,1973,1973), prod_method = c("Farmed","Farmed","Farmed","Farmed"))

Country_Mean_Farmed <- rbind(Country_Mean_Farmed,ghg_values_other_countries_production)

Country_Mean_Farmed <- Country_Mean_Farmed[-c(6),]
  
redundant_trade_emissions <- merge(Country_Mean_Farmed,calc_df, by = c("iso3c","prod_method"), all.y = T) %>% mutate(sd_ghg = if_else(is.na(sd_ghg), 0, sd_ghg)) %>% view()

#Add in production GHG for wild caught

total_ghg_wild <- total_ghg_wild %>% mutate(prod_method = "Wild")

total_ghg_wild <- total_ghg_wild[,c(4,1,2,3)]

#remove the iso3c

total_ghg_wild <- select(total_ghg_wild, -c(iso3c))

redundant_trade_emissions <- left_join(redundant_trade_emissions,total_ghg_wild, by = c("prod_method")) %>% view()

#change all zero values to NA
redundant_trade_emissions <- na_if(redundant_trade_emissions, 0)

redundant_trade_emissions <- redundant_trade_emissions %>% unite(mean_ghg,c("mean_ghg.y", "mean_ghg.x"), na.rm = TRUE) 

redundant_trade_emissions <- redundant_trade_emissions %>% unite(sd_ghg,c("sd_ghg.y", "sd_ghg.x"), na.rm = TRUE)

#fill blanks 

redundant_trade_emissions <-redundant_trade_emissions %>% mutate(sd_ghg = ifelse(sd_ghg %in% "", 0 ,sd_ghg)) %>% view()


#Include Emissions from production

redundant_trade_emissions <- redundant_trade_emissions %>% mutate(production_emissions = quantity*as.numeric(mean_ghg))
                                                                  
redundant_trade_emissions <- redundant_trade_emissions %>% mutate(total_emissions = production_emissions + transport_emissions)

total_output_list[[i]] <- redundant_trade_emissions

redundant_trade_emissions <- redundant_trade_emissions[,c(1,2,3,4,5,6,13,7,8,9,10,11,14,12)]

#Emission per species
emissions_per_species <-redundant_trade_emissions %>% 
  group_by(species_name, prod_method) %>% 
  summarise(species_emission = sum(total_emissions, na.rm= TRUE)) %>% view()

sum(emissions_per_species$species_emission)

colnames(redundant_trade_emissions)[which(names(redundant_trade_emissions) == "total_emissions")] <- "transport_emissions"

#Total emissions produced from transportation is 4730576 kg CO2

redundant_trade_emissions %>% 
  group_by(species_name, prod_method) %>% 
  summarise(estimate = mean(transport_emissions, na.rm= TRUE),
            sd = sd(transport_emissions, na.rm = TRUE)) %>% view()

write.csv(redundant_trade_emissions, file = "data/redundant_trade_emissions.csv")

redundant_trade_emissions %>% group_by(species_name,prod_method) %>% summarise(transport_emissions) %>% view()

#Plots


  