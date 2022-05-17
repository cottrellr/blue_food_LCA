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
    add_column(iso3c = "Wild") %>% select(iso3c,mean_ghg,sd_ghg))


#Combine both to be side by side
                          
Total_GHG_Prawns <- rbind(total_ghg,total_ghg_wild)

GHG_data <- rbind(lca_by_country,wild_ghg)

#Mean for each country (Farmed Prawns Only)

Country_Mean_1 <- lca_data %>% group_by(iso3c) %>% summarise(mean_ghg = mean(total_ghg))

#Overall mean for other countries (Farmed Prawns)

target <- c("CHN","IDN","THA","VNM")

Country_mean_2 <- read_csv(here("Outputs/lca_model_data_no-summary.csv"))%>% filter(taxa == "shrimp") %>% filter(!iso3c %in% target) %>% 
  select(study_id, iso3c, clean_sci_name, taxa, intensity, system, total_ghg ) %>% arrange(iso3c) %>% group_by(iso3c) %>% summarise(mean_ghg = mean(total_ghg))%>%
  mutate(iso3c = replace(iso3c, iso3c == "BGD", "Other_Countries"))

Country_Mean_Farmed <- rbind(Country_Mean_1,Country_mean_2)

#Transportation

output <- list()

for(i in 1:1000){
  
  
  
  emissions_factor <- rnorm(n=1, mean = 12.41, sd=1.83)
  
  trade_volume <- 4
  
  km_travelled <- 3
  
  calc <- emissions_factor*trade_volume*km_travelled
  
  output <- c(output, calc)
  
  
}

simple_ouput <- unlist(output)

mean(simple_ouput)
sd(simple_ouput)


