library(here)
library(tidyverse)

#Emissions calculations for Farmed Prawns

#GHG Produced by Farmed Prawns

write_csv(lca_model_dat, file = here("Outputs/lca_model_data_no-summary.csv"))

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

write_csv(wild_dat_new_weights, file = here("Outputs/wild_data_GHG_Model.csv"))


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

target <- c("CHN","IDN","THA","VNM")

#WHAT IS THE REASON FOR USING BANGLADESH AS AVERAGE?

Country_mean_2 <- read_csv(here("Outputs/lca_model_data_no-summary.csv"))%>% filter(taxa == "shrimp") %>% filter(!iso3c %in% target) %>% 
  select(study_id, iso3c, clean_sci_name, taxa, intensity, system, total_ghg ) %>% arrange(iso3c) %>% group_by(iso3c) %>% summarise(mean_ghg = mean(total_ghg))%>%
  mutate(iso3c = replace(iso3c, iso3c == "BGD", "Other_Countries_Farmed"))

Country_Mean_Farmed <- rbind(Country_Mean_1,Country_mean_2)

Mean_GHG_farmed <- Country_Mean_Farmed %>% summarise(mean_ghg = mean(mean_ghg)) %>% mutate(iso3c = "Farmed") %>% select(iso3c,mean_ghg)

#combine with wild caught

GHG_EF <- bind_rows(total_ghg_wild,Mean_GHG_farmed)

#Transportation



#Lower quartile: 15.2, Upper Quartile: 20.1 as per IMO GHG Report


#The emissions should go in its own csv and be read in
Emissions_Range <- c(35.2, 27.3,
                     20.3, 17.7, 16.6, 13.4, 10.6, 7.3)




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
    mutate(next_column = quantity*distance*ef*2) %>% 
    mutate(final_column = next_column/0.5*1000) %>% 
    mutate(rep = i))
  
  output_df_list[[i]] <- calc_df

  #add in production
  
  total_calc_df <- calc_df %>% 
    envioronmental_data. #whatever this is
  
  total_output_list[[i]] <- total_calc_df
  
}
  
emissions_outputfile <- bind_rows(output_df_list)  

write_csv(x = emissions_outputfile, file = here("data/output/emissions_output_w_reps.csv"))


emissions_outputfile %>% 
  group_by(iso3c, species_name, prod_method) %>% 
  summarise(estimate = mean(final_column, na.rm= TRUE),
            sd = sd(final_column, na.rm = TRUE))
  
  


#SD is 1.457738

mean(Emissions_Range)

#mean is 17.65

#COnvert to Kg CO2e tonne-1 km-1 

Emissions_Range <- Emissions_Range*0.54/1000

mean(Emissions_Range)

#0.009531

sd(Emissions_Range)

#0.0007871

emissions_factor <- rnorm(n=2000, mean = 0.009531, sd=0.0007871)

mean(emissions_factor)


for(i in 1:1000){
  
total_km_travelled <- (763977*2)
  
total_trade_volume <- 58296.2354
  
calc <- emissions_factor*total_trade_volume*total_km_travelled

output <- c(output, calc)
  
}

simple_ouput <- unlist(output)

mean(simple_ouput)
sd(simple_ouput)