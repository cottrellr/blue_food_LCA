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

#add in production data
  
Country_Mean_Farmed <- Country_Mean_Farmed %>% mutate(prod_method = "Farmed")

#Add in GHG values for missing countries based on values from Other_Countries_Farmed

ghg_values_other_countries_production <- data.table(iso3c = c("IND","JPY", "MYS","SGP"), mean_ghg = c(5648,5648,5648,5648), sd_ghg = c(1973,1973,1973,1973), prod_method = c("Farmed","Farmed","Farmed","Farmed"))

Country_Mean_Farmed <- rbind(Country_Mean_Farmed,ghg_values_other_countries_production)

Country_Mean_Farmed <- Country_Mean_Farmed[-c(6),]
  
redundant_trade_emissions <- merge(Country_Mean_Farmed,calc_df, by = c("iso3c","prod_method"), all.y = T) %>% mutate(sd_ghg = if_else(is.na(sd_ghg), 0, sd_ghg))

#Add in production GHG for wild caught

total_ghg_wild <- total_ghg_wild %>% mutate(prod_method = "Wild")

total_ghg_wild <- total_ghg_wild[,c(4,1,2,3)]

#remove the iso3c

total_ghg_wild <- select(total_ghg_wild, -c(iso3c))

redundant_trade_emissions <- left_join(redundant_trade_emissions,total_ghg_wild, by = c("prod_method"))

#change all zero values to NA
redundant_trade_emissions <- na_if(redundant_trade_emissions, 0)

redundant_trade_emissions <- redundant_trade_emissions %>% unite(mean_ghg,c("mean_ghg.y", "mean_ghg.x"), na.rm = TRUE) 

redundant_trade_emissions <- redundant_trade_emissions %>% unite(sd_ghg,c("sd_ghg.y", "sd_ghg.x"), na.rm = TRUE)

#fill blanks 

redundant_trade_emissions <-redundant_trade_emissions %>% mutate(sd_ghg = ifelse(sd_ghg %in% "", 0 ,sd_ghg))


#########Transportation###############

#Add in emissions factors from the Fourth IMO GHG Study, we used values from 2017 for container ships.

#Link: https://wwwcdn.imo.org/localresources/en/OurWork/Environment/Documents/Fourth%20IMO%20GHG%20Study%202020%20-%20Full%20report%20and%20annexes.pdf


transport_distance_volume_spp <- read_csv("data/Transportation_Distance_Per_Species.csv")

emissions_factors <- read_csv("data/Carrier_Size_GHG.csv")

transport_distance_volume_spp_list <- transport_distance_volume_spp %>% 
  group_split(iso3c, species_name, prod_method, quantity, distance)


transport_output_list <- list()


set.seed(42)

for(i in 1:1000){
  
  ef <- runif(n=1, min = min(emissions_factors$mean_ef), max = max(emissions_factors$mean_ef))
  
  (calc_df <- transport_distance_volume_spp %>% 
      mutate(ef = ef) %>% 
      mutate(emissions_nautical_mile = quantity*distance*ef*2) %>% 
      mutate(transport_emissions = emissions_nautical_mile*0.54/1000) %>% mutate(rep = i))
  
  transport_output_list[[i]] <- calc_df
  
}

all_transport_iterations <- bind_rows(transport_output_list)


write.csv(all_transport_iterations, file = "data/transport_emissions.csv")


########### Below is the summary for transportation emissions by country and species #######

all_transport_iterations <- read_csv("data/transport_emissions.csv")

country_species_transport_emissions <- 
  all_transport_iterations |> 
  group_by(iso3c, species_name, rep) |>
  summarise(sum_emissions = sum(transport_emissions, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(iso3c, species_name,prod_method) %>% 
  summarise(mean_emissions = mean(sum_emissions, na.rm=TRUE),
            SEM_emissions = sd(sum_emissions, na.rm = TRUE))

write_csv(country_species_transport_emissions, "data/transport_emissions_by_country_species_summary.csv")

############ Below is the summary for transportation emissions by just country #############

country_transport_emissions <- 
  all_transport_iterations |> 
  group_by(iso3c, species_name, rep,prod_method) |>
  summarise(sum_emissions = sum(transport_emissions, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(iso3c, species_name, prod_method) %>% 
  summarise(mean_emissions = mean(sum_emissions, na.rm=TRUE),
            SEM_emissions = sd(sum_emissions, na.rm = TRUE))

write_csv(country_transport_emissions, "data/transport_emissions_by_country_summary.csv")


#calculating GHG footprint from transportation

country_transport_emissions_GHG_footprint <- 
  all_transport_iterations %>% group_by(iso3c) %>%
  summarise(mean_emissions = mean(transport_emissions, na.rm=TRUE),distance = mean(distance, na.rm = TRUE), quantity = sum(quantity)) %>% 
  mutate(footprint = mean_emissions/quantity/1000)

write_csv(country_transport_emissions_GHG_footprint, "data/country_transport_emissions_GHG_footprint.csv")

###########Adding it all together#################

#Include Emissions from production

#Remove previous transportation calculations & rename + arrange all columns for easier readability

redundant_trade_emissions <- select(redundant_trade_emissions, -c(distance,ef,emissions_nautical_mile,transport_emissions))

redundant_trade_emissions %>% group_by(iso3c)

Total_Emissions <- left_join(redundant_trade_emissions,country_species_transport_emissions, by = c("iso3c","species_name"))

Total_Emissions <- rename(Total_Emissions,production_emissions = mean_ghg, production_sd = sd_ghg,transport_emissions = mean_emissions,transport_sd = SEM_emissions)

Total_Emissions <- Total_Emissions[,c(1,2,5,6,3,4,8,9,7)]

#Add total emissions columns and production

Total_Emissions$production_emissions <- as.integer(Total_Emissions$production_emissions)

Total_Emissions$quantity <- as.integer(Total_Emissions$quantity)

Total_Emissions$production_sd <- as.integer(Total_Emissions$production_sd)

Total_Emissions$transport_sd <- as.integer(Total_Emissions$transport_sd)

Total_Emissions <- Total_Emissions %>% mutate(total_production_emissions = production_emissions * quantity) %>% mutate(total_emissions = total_production_emissions + transport_emissions) %>% mutate(total_sd = production_sd+transport_sd)

Total_Emissions <- Total_Emissions[,c(1,2,3,4,5,6,7,8,11,9,12,10)]

write_csv(Total_Emissions, "data/total_emissions.csv")

#Plot

library(ggplot2)
library(scales)

ggplot_ghg_per_country <- ggplot(country_transport_emissions, aes(x = reorder(iso3c, -mean_emissions), y = mean_emissions)) +
  geom_bar(stat="identity", color="white", position=position_dodge()) + 
  geom_errorbar(aes(ymin=mean_emissions-SEM_emissions, ymax=mean_emissions+SEM_emissions), width=.2, position=position_dodge(.9)) +
  theme(axis.title.x=element_blank()) + scale_y_continuous(labels = label_number(suffix = " tonnes", scale = 1e-3), trans ="log10")


print(ggplot_ghg_per_country)

#GHG by country and species

breaks = 10**(1:10)

country_species_transport_emissions <- country_species_transport_emissions %>% group_by(iso3c) %>% arrange(sum(-mean_emissions))

test_ggplot <- 
  ggplot(country_species_transport_emissions, aes(x = reorder(iso3c, -mean_emissions), y = mean_emissions, fill = species_name)) +
  geom_col(width = 0.5,position = position_dodge(width=0.5)) +
  geom_errorbar(aes(x=iso3c, ymin= mean_emissions-SEM_emissions, ymax= mean_emissions+SEM_emissions), position = position_dodge(width=0.5))+
  ylab("CO2e emissions (tonnes)") +
  xlab("")+ 
  scale_y_continuous(breaks = breaks, labels = comma(breaks), trans= "log10") +
  scale_fill_manual(values = c("red","sky blue","orange","light green","yellow")) + coord_flip() +
  theme_classic()

print(test_ggplot) 
ggsave(file = "Outputs/emissions_species_country.png")

test_ggplot_2 <- 
  ggplot(country_transport_emissions, aes(x = reorder(iso3c, -mean_emissions), y = mean_emissions, fill = "blue")) +
  geom_col(position = position_dodge(width=0.5)) +
  geom_errorbar(aes(x=iso3c, ymin= mean_emissions-SEM_emissions, ymax= mean_emissions+SEM_emissions), position = "dodge")+
  ylab("CO2e emissions") +
  xlab("")+ 
  scale_y_continuous(labels = label_number(suffix = " tonnes", scale = 1e-3), trans= "log10")

print(test_ggplot_2)

#Having a zero in the data frame doesn't let us plot SD accordingly hence we add a minimal value of 0.1 to it

Total_Emissions[6,6]=0.1

test_ggplot_3 <- 
  
  ggplot(Total_Emissions, aes(x = reorder(iso3c, -total_emissions), y = total_emissions)) +
  geom_bar(stat="identity", color="white", position=position_dodge()) +
  geom_errorbar(aes(x = iso3c, ymin= total_emissions-total_sd, ymax= total_emissions+total_sd), position = position_dodge(0.9))+
  ylab("CO2e emissions") + xlab("") + 
  theme(axis.title.x=element_blank()) + scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks = breaks, labels = comma(breaks))

print(test_ggplot_3)
