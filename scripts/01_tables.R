# -------------------------------------------------------------------------------------------

# Author: Allan Davids
# Email: allan.davids@uct.ac.za
# Date created: 1 December 2020
# Date last revised: 23 March 2021
# Purpose: Writes tables in paper

# -------------------------------------------------------------------------------------------


# TABLE 3  ----------------------------------------------------------------------------------

simulation_output <- list()
results <- list()
peak_results <- list()
compliance_list <- list()
contact_list <- list()

i <- 1

# Loop through cities and lockdown scenarios

for (city in cities) {
  
  for(scenario in scenarios){
    
    simulation_results <- paste(base_data_directory, city, learning[1], scenario, sep = "/")
    files_to_read <- list.files(simulation_results, pattern = "*.csv")
    
    simulation_list <- sapply(paste(simulation_results, files_to_read, sep = "/"), FUN = function(x) {
      out <- fread(x)}, simplify = FALSE)
    
    for(l in seq(simulation_list)){
      
      simulation_list[[l]]$seed_id <- files_to_read[l]
      
    }
    
    simulation_output[[i]] <- do.call(rbind, simulation_list)
    
    if(city == "cape_town"){
      population <- populations[1]
      city_name <- "Cape Town"
      capacity <- cpt_hospital_capacity
    } else{
      population <- populations[2]
      city_name <- "Johannesburg"
      capacity <- jhb_hospital_capacity
    }
    
    end_of_sim_results <- simulation_output[[i]] %>%
      mutate(i1 = i1 / model_population * population,
             i2 = i2 / model_population * population,
             c = c / model_population * population,
             d = d / model_population * population,
             r = r / model_population * population,
             s = s / model_population * population,
             e = e / model_population * population) %>%
      filter(V1 < 180) %>%
      filter(V1 == max(V1)) %>%
      mutate(cum_infections = i1 + i2 + d + c + r,
             infections = i1 + i2 + c) %>%
      group_by(Day = V1) %>%
      summarise(sd_d = sd(d),
                sd_r = sd(r),
                sd_cum_infections = sd(cum_infections),
                max_d = max(d),
                min_d = min(d),
                max_r = max(r),
                min_r = min(r),
                min_cum_infections = min(cum_infections),
                max_cum_infections = max(cum_infections),
                d = mean(d),
                r = mean(r),
                cum_infections = mean(cum_infections),
                inf_last_day = mean(infections),
                min_inf_last_day = min(infections),
                max_inf_last_day = max(infections),
                sd_inf_last_day = sd(infections)) %>%
      mutate(City = city_name)
    
    results[[i]] <- data.frame(`Outcome` = character(),
                               `Min`= numeric(),
                               `Mean`= numeric(),
                               `Max`= numeric(),
                               `SD`= numeric(),
                               stringsAsFactors=FALSE) 
    
    results[[i]][1,] <- c("Total Infections", 
                          round(end_of_sim_results$min_cum_infections),
                          round(end_of_sim_results$cum_infections),
                          round(end_of_sim_results$max_cum_infections),
                          round(end_of_sim_results$sd_cum_infections))
    
    
    results[[i]][2,] <- c("Total Deaths", 
                          round(end_of_sim_results$min_d),
                          round(end_of_sim_results$d),
                          round(end_of_sim_results$max_d),
                          round(end_of_sim_results$sd_d))
    
    results[[i]][3,] <- c("Total Recoveries", 
                          round(end_of_sim_results$min_r),
                          round(end_of_sim_results$r),
                          round(end_of_sim_results$max_r),
                          round(end_of_sim_results$sd_r))
    
    
    max_of_sim_results <- simulation_output[[i]] %>%
      mutate(i1 = i1 / model_population * population,
             i2 = i2 / model_population * population,
             c = c / model_population * population,
             d = d / model_population * population,
             r = r / model_population * population,
             s = s / model_population * population,
             e = e / model_population * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r,
             infections = i1 + i2 + c) %>%
      filter(V1 < 180) %>%
      group_by(Day = V1) %>%
      summarise(sd_c = sd(c),
                sd_infections = sd(infections),
                max_c = max(c),
                min_c = min(c),
                min_infections = min(infections),
                max_infections = max(infections),
                c = mean(c),
                infections = mean(infections)) %>%
      mutate(City = city_name)
    
    results[[i]][4,] <- c("Peak Infections", 
                          round(max_of_sim_results$min_infections[which(
                            max_of_sim_results$infections == max(max_of_sim_results$infections))]),
                          round(max_of_sim_results$infections[which(
                            max_of_sim_results$infections == max(max_of_sim_results$infections))]),
                          round(max_of_sim_results$max_infections[which(
                            max_of_sim_results$infections == max(max_of_sim_results$infections))]),
                          round(max_of_sim_results$sd_infections[which(
                            max_of_sim_results$infections == max(max_of_sim_results$infections))]))
    
    
    results[[i]][5,] <- c("Peak Critical", 
                          round(max_of_sim_results$min_c[which(
                            max_of_sim_results$c == max(max_of_sim_results$c))]),
                          round(max_of_sim_results$c[which(
                            max_of_sim_results$c == max(max_of_sim_results$c))]),
                          round(max_of_sim_results$max_c[which(
                            max_of_sim_results$c == max(max_of_sim_results$c))]),
                          round(max_of_sim_results$sd_c[which(
                            max_of_sim_results$c == max(max_of_sim_results$c))]))
    
    results[[i]][,c(2:5)] <- map_df(results[[i]][,c(2:5)], prettyNum,  big.mark = ",")
    
    
    temp <- simulation_output[[i]] %>% 
      group_by((row_number()-1) %/% (n()/49)) %>%
      nest %>% pull(data)
    
    get_peak_infections <- function(df, outcome){
      
      peak <- df %>%
        mutate(Day = V1) %>%
        mutate(infections = i1 + i2 + c) %>%
        filter(infections == max(infections)) %>%
        mutate(Day = V1)
      
      return(peak$Day[1])
      
    }
    
    infections_peak <- as.integer(lapply(temp, get_peak_infections))
    
    
    results[[i]][6,] <- c("Peak Infections (Day)", 
                          round(min(infections_peak)),
                          round(mean(infections_peak)),
                          round(max(infections_peak)),
                          round(sd(infections_peak)))
    
    get_peak_critical <- function(df, outcome){
      
      peak <- df %>%
        mutate(Day = V1) %>%
        filter(c == max(c)) %>%
        mutate(Day = V1)
      
      return(peak$Day[1])
      
    }
    
    critical_peak <- as.integer(lapply(temp, get_peak_critical))
    
    
    results[[i]][7,] <- c("Peak Critical (Day)", 
                          round(min(critical_peak)),
                          round(mean(critical_peak)),
                          round(max(critical_peak)),
                          round(sd(critical_peak)))
    
    get_days_overcapcity <- function(df){
      
      over_capacity <- df %>%
        mutate(c = c / model_population * population) %>%
        group_by(Day = V1) %>%
        summarise(c = mean(c)) %>%
        filter(c > capacity)
      
      
      return(nrow(over_capacity))
      
    }
    
    
    results[[i]][8,] <- c("End of simulation infections", 
                          round(end_of_sim_results$min_inf_last_day),
                          round(end_of_sim_results$inf_last_day),
                          round(end_of_sim_results$max_inf_last_day),
                          round(end_of_sim_results$sd_inf_last_day))
    
    results[[i]][8,c(2:5)] <- map_df(results[[i]][8,c(2:5)], prettyNum,  big.mark = ",")
    
    overcapacity <- as.integer(lapply(temp, get_days_overcapcity))
    
    results[[i]][9,] <- c("Critical > Capacity (Days)", 
                          round(min(overcapacity)),
                          round(mean(overcapacity)),
                          round(max(overcapacity)),
                          round(sd(overcapacity)))
    
    
    
    compliance_list[[i]] <- simulation_output[[i]] %>%
      group_by(seed_id) %>%
      summarise(compliance = mean(compliance))
    
    contact_list[[i]] <- simulation_output[[i]] %>%
      group_by(seed_id) %>%
      summarise(contacts = mean(contacts))
    
    results[[i]][10,] <- c("Contacts", 
                           round(min(contact_list[[i]]$contacts),2),
                           round(mean(contact_list[[i]]$contacts),2),
                           round(max(contact_list[[i]]$contacts),2),
                           round(sd(contact_list[[i]]$contacts),2))
    
    results[[i]][11,] <- c("Compliance", 
                           round(min(compliance_list[[i]]$compliance),2),
                           round(mean(compliance_list[[i]]$compliance),2),
                           round(max(compliance_list[[i]]$compliance),2),
                           round(sd(compliance_list[[i]]$compliance),2))
    
    
    i <- i + 1
    
    
    
    
    
    
    
  }
  
  
}

# Write Table 3 to file

print(xtable(results[[1]], type = "latex", align = c("l", "l", rep("c",4))), include.rownames=FALSE, file = "results/tables/lockdown.tex")

print(xtable(results[[2]], type = "latex", align = c("l", "l", rep("c",4))), include.rownames=FALSE, file = "results/tables/no_intervention.tex")


# TABLES 4 AND 5 ----------------------------------------------------------------------------

i <-  1

sensitivity.df <- list()
results_sensitivity <- list()

sensitivity_scenarios <- c("lockdown", "no_intervention")
city <- cities[1]
p <- "parameters_wps"
wps <- seq(0, 1, 0.05)


for (city in cities) {
  
  for(s in sensitivity_scenarios){
    
    sensitivity_directory <- paste(base_data_directory, city, learning[1], "sensitivity", s, p, sep = "/")
    simulation_runs <- list.files(sensitivity_directory)
    files_to_read <- list.files(paste(sensitivity_directory, simulation_runs[1], sep = "/"), pattern = "*.csv")
    sensitivity.list <- sapply(paste(sensitivity_directory, simulation_runs[1], files_to_read, sep = "/"), FUN = function(x) {
      out <- fread(x)
    }, simplify = FALSE)
    
    sensitivity.df[[i]] <- do.call(rbind, sensitivity.list)
    
    if(city == "cape_town"){
      population <- populations[1]
      city_name <- "Cape Town"
    } else{
      population <- populations[2]
      city_name <- "Johannesburg"
    }
    
    sensitivity.df[[i]]  <- sensitivity.df[[i]]  %>%
      mutate(i1 = i1 / 100000 * population,
             i2 = i2 / 100000 * population,
             c = c / 100000 * population,
             d = d / 100000 * population,
             r = r / 100000 * population,
             s = s / 100000 * population,
             e = e / 100000 * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
      mutate(infections = i1 + i2 + c) %>%
      group_by(Day = V1) %>%
      summarise(v_d = var(d),
                v_cum_infections = var(cum_infections),
                e = mean(e),
                i1 = mean(i1),
                i2 = mean(i2),
                s = mean(s),
                d = mean(d),
                c = mean(c),
                r = mean(r),
                compliance = mean(compliance),
                v_d = mean(v_d),
                cum_infections = mean(cum_infections),
                v_cum_infections = mean(v_cum_infections),
                infections = mean(infections),
                contacts = mean(contacts)) %>%
      mutate(private_signal = as.character(sub("^\\D+", "", sub("^\\D+", "", wps[1]))))
    
    
    for(j in 2:length(simulation_runs)){
      
      files_to_read <- list.files(paste(sensitivity_directory, simulation_runs[j], sep = "/"), pattern = "*.csv")
      
      sensitivity.list <- sapply(paste(sensitivity_directory, simulation_runs[j], files_to_read, sep = "/"), FUN = function(x) {
        out <- fread(x)
      }, simplify = FALSE)
      
      sensitivity_temp.df <- do.call(rbind, sensitivity.list)
      
      sensitivity_temp.df <- sensitivity_temp.df  %>%
        mutate(i1 = i1 / 100000 * population,
               i2 = i2 / 100000 * population,
               c = c / 100000 * population,
               d = d / 100000 * population,
               r = r / 100000 * population,
               s = s / 100000 * population,
               e = e / 100000 * population) %>%
        mutate(cum_infections = i1 + i2 + d + c + r) %>%
        mutate(infections = i1 + i2 + c) %>%
        mutate(population) %>%
        mutate(infections = i1 + i2 + c) %>%
        group_by(Day = V1) %>%
        summarise(v_d = var(d),
                  v_cum_infections = var(cum_infections),
                  e = mean(e),
                  i1 = mean(i1),
                  i2 = mean(i2),
                  s = mean(s),
                  d = mean(d),
                  c = mean(c),
                  r = mean(r),
                  compliance = mean(compliance),
                  v_d = mean(v_d),
                  cum_infections = mean(cum_infections),
                  v_cum_infections = mean(v_cum_infections),
                  infections = mean(infections),
                  contacts = mean(contacts)) %>%
        mutate(private_signal = as.character(sub("^\\D+", "", sub("^\\D+", "", wps[j]))))
      
      sensitivity.df[[i]] <- rbind.data.frame(sensitivity.df[[i]], sensitivity_temp.df)
      
      
      
    }
    
    i <- i + 1
    
  }
  
}


if(city == "cape_town"){
  population <- populations[1]
  city_name <- "Cape Town"
  capacity <- cpt_hospital_capacity
} #else{
#  population <- populations[2]
#  city_name <- "Johannesburg"
#  capacity <- jhb_hospital_capacity
#}


for(i in seq(1,2)){
  
  results_sensitivity[[i]] <- data.frame(`Outcome` = character(),
                                         `0` = numeric(),
                                         `0.05`= numeric(),
                                         `0.1`= numeric(),
                                         `0.15` = numeric(),
                                         `0.25`= numeric(),
                                         `0.5`= numeric(),
                                         `0.75`= numeric(),
                                         `1.0`= numeric(),
                                         stringsAsFactors=FALSE) 
  
  for(learn in 2:9){
    
    wps_index <- c(1, 2, 3, 4, 6, 11, 16, 21)
    
    end_of_sim_results <- sensitivity.df[[i]] %>%
      filter(private_signal == as.character(wps[wps_index[learn-1]])) %>%
      filter(Day < 180) %>%
      filter(Day == max(Day)) %>%
      mutate(City = city_name)
    
    results_sensitivity[[i]][1,c(1,learn)] <- c("Total Infections", round(end_of_sim_results$cum_infections))
    results_sensitivity[[i]][2,c(1,learn)] <- c("Total Deaths", round(end_of_sim_results$d))
    results_sensitivity[[i]][3,c(1,learn)] <- c("Total Recoveries", round(end_of_sim_results$r))
    
    
    
    max_of_sim_results <- sensitivity.df[[i]] %>%
      filter(private_signal == as.character(wps[wps_index[learn-1]])) %>%
      filter(Day < 180)
    
    results_sensitivity[[i]][4,c(1,learn)] <- c("Peak Infections", 
                                                round(max_of_sim_results$infections[which(
                                                  max_of_sim_results$infections == max(max_of_sim_results$infections))]))
    
    results_sensitivity[[i]][5,c(1,learn)] <- c("Peak Critical", 
                                                round(max_of_sim_results$c[which(
                                                  max_of_sim_results$c == max(max_of_sim_results$c))]))
    
    
    
    
    peak <- sensitivity.df[[i]] %>%
      filter(private_signal == as.character(wps[wps_index[learn-1]])) %>%
      filter(infections == max(infections))
    
    results_sensitivity[[i]][6,c(1,learn)] <- c("Peak Infections (Day)", peak$Day)
    
    peak <- sensitivity.df[[i]] %>%
      filter(private_signal == as.character(wps[wps_index[learn-1]])) %>%
      filter(c == max(c))
    
    results_sensitivity[[i]][7,c(1,learn)] <- c("Peak Critical (Day)", peak$Day)
    
    results_sensitivity[[i]][8,c(1,learn)] <- c("End of simulation infections", round(end_of_sim_results$infections))
    
    over_capacity <- sensitivity.df[[i]] %>%
      filter(private_signal == as.character(wps[wps_index[learn-1]])) %>%
      filter(c > capacity)
    
    results_sensitivity[[i]][9,c(1,learn)] <- c("Critical > Capacity (Days)", nrow(over_capacity))
    
    compliance_contacts <- sensitivity.df[[i]] %>%
      filter(private_signal == as.character(wps[wps_index[learn-1]])) %>%
      summarise(contacts = mean(contacts),
                compliance = mean(compliance))
    
    results_sensitivity[[i]][10,c(1,learn)] <- c("Contacts", round(compliance_contacts$contacts,2))
    results_sensitivity[[i]][11,c(1,learn)] <- c("Compliance", round(compliance_contacts$compliance, 2))
    
  }
  
  
  
  results_sensitivity[[i]][,c(2:9)] <- map_df(results_sensitivity[[i]][,c(2:9)], prettyNum,  big.mark = ",")
  colnames(results_sensitivity[[i]])[2:9] <- gsub("X", "", colnames(results_sensitivity[[i]])[2:9])
  
  
  
  
}


# Write Table 4 to file

print(xtable(results_sensitivity[[1]], type = "latex", align = c("l", "l", rep("c",8))), include.rownames=FALSE, booktabs = T, file = "results/tables/wps_lockdown_sensitivity.tex")

# Write Table 5 to file

print(xtable(results_sensitivity[[2]], type = "latex", align = c("l", "l", rep("c",8))), include.rownames=FALSE, booktabs = T, file = "results/tables/wps_no_intervention_sensitivity.tex")


# TABLES 6 AND 7 ----------------------------------------------------------------------------

i <-  1
sensitivity.df <- list()
results_sensitivity <- list()

sensitivity_scenarios <- c("lockdown", "no_intervention")
city <- cities[1]
p <- "parameters_tp"
tp <- c(0.02689, 0.02789, 0.02889, 0.02989, 0.03089, 0.03189, 0.03289)


for (city in cities) {
  
  for(s in sensitivity_scenarios){
    
    sensitivity_directory <- paste(base_data_directory, city, learning[1], "sensitivity", s, p, sep = "/")
    simulation_runs <- list.files(sensitivity_directory)
    files_to_read <- list.files(paste(sensitivity_directory, simulation_runs[1], sep = "/"), pattern = "*.csv")
    sensitivity.list <- sapply(paste(sensitivity_directory, simulation_runs[1], files_to_read, sep = "/"), FUN = function(x) {
      out <- fread(x)
    }, simplify = FALSE)
    
    sensitivity.df[[i]] <- do.call(rbind, sensitivity.list)
    
    if(city == "cape_town"){
      population <- populations[1]
      city_name <- "Cape Town"
    } else{
      population <- populations[2]
      city_name <- "Johannesburg"
    }
    
    sensitivity.df[[i]]  <- sensitivity.df[[i]]  %>%
      mutate(i1 = i1 / 100000 * population,
             i2 = i2 / 100000 * population,
             c = c / 100000 * population,
             d = d / 100000 * population,
             r = r / 100000 * population,
             s = s / 100000 * population,
             e = e / 100000 * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
      mutate(infections = i1 + i2 + c) %>%
      group_by(Day = V1) %>%
      summarise(v_d = var(d),
                v_cum_infections = var(cum_infections),
                e = mean(e),
                i1 = mean(i1),
                i2 = mean(i2),
                s = mean(s),
                d = mean(d),
                c = mean(c),
                r = mean(r),
                compliance = mean(compliance),
                v_d = mean(v_d),
                cum_infections = mean(cum_infections),
                v_cum_infections = mean(v_cum_infections),
                infections = mean(infections),
                contacts = mean(contacts)) %>%
      mutate(transmission = as.character(sub("^\\D+", "", sub("^\\D+", "", tp[1]))))
    
    
    for(j in 2:length(simulation_runs)){
      
      files_to_read <- list.files(paste(sensitivity_directory, simulation_runs[j], sep = "/"), pattern = "*.csv")
      
      sensitivity.list <- sapply(paste(sensitivity_directory, simulation_runs[j], files_to_read, sep = "/"), FUN = function(x) {
        out <- fread(x)
      }, simplify = FALSE)
      
      sensitivity_temp.df <- do.call(rbind, sensitivity.list)
      
      sensitivity_temp.df <- sensitivity_temp.df  %>%
        mutate(i1 = i1 / 100000 * population,
               i2 = i2 / 100000 * population,
               c = c / 100000 * population,
               d = d / 100000 * population,
               r = r / 100000 * population,
               s = s / 100000 * population,
               e = e / 100000 * population) %>%
        mutate(cum_infections = i1 + i2 + d + c + r) %>%
        mutate(infections = i1 + i2 + c) %>%
        mutate(population) %>%
        mutate(infections = i1 + i2 + c) %>%
        group_by(Day = V1) %>%
        summarise(v_d = var(d),
                  v_cum_infections = var(cum_infections),
                  e = mean(e),
                  i1 = mean(i1),
                  i2 = mean(i2),
                  s = mean(s),
                  d = mean(d),
                  c = mean(c),
                  r = mean(r),
                  compliance = mean(compliance),
                  v_d = mean(v_d),
                  cum_infections = mean(cum_infections),
                  v_cum_infections = mean(v_cum_infections),
                  infections = mean(infections),
                  contacts = mean(contacts)) %>%
        mutate(transmission = as.character(sub("^\\D+", "", sub("^\\D+", "", tp[j]))))
      
      sensitivity.df[[i]] <- rbind.data.frame(sensitivity.df[[i]], sensitivity_temp.df)
      
      
      
    }
    
    i <- i + 1
    
  }
  
}


if(city == "cape_town"){
  population <- populations[1]
  city_name <- "Cape Town"
  capacity <- cpt_hospital_capacity
}# else{
#  population <- populations[2]
#  city_name <- "Johannesburg"
#  capacity <- jhb_hospital_capacity
#}


for(i in seq(1,2)){
  
  results_sensitivity[[i]] <- data.frame(`Outcome` = character(),
                                         `0.027` = numeric(),
                                         `0.028`= numeric(),
                                         `0.029`= numeric(),
                                         `0.030`= numeric(),
                                         `0.031`= numeric(),
                                         `0.032`= numeric(),
                                         `0.033`= numeric(),
                                         stringsAsFactors=FALSE) 
  
  for(learn in seq(2, 8)){
    
    end_of_sim_results <- sensitivity.df[[i]] %>%
      filter(transmission == tp[learn-1]) %>%
      filter(Day < 180) %>%
      filter(Day == max(Day)) %>%
      mutate(City = city_name)
    
    results_sensitivity[[i]][1,c(1,learn)] <- c("Total Infections", round(end_of_sim_results$cum_infections))
    results_sensitivity[[i]][2,c(1,learn)] <- c("Total Deaths", round(end_of_sim_results$d))
    results_sensitivity[[i]][3,c(1,learn)] <- c("Total Recoveries", round(end_of_sim_results$r))
    
    
    
    max_of_sim_results <- sensitivity.df[[i]] %>%
      filter(transmission == tp[learn-1]) %>%
      filter(Day < 180)
    
    results_sensitivity[[i]][4,c(1,learn)] <- c("Peak Infections", 
                                                round(max_of_sim_results$infections[which(
                                                  max_of_sim_results$infections == max(max_of_sim_results$infections))]))
    
    results_sensitivity[[i]][5,c(1,learn)] <- c("Peak Critical", 
                                                round(max_of_sim_results$c[which(
                                                  max_of_sim_results$c == max(max_of_sim_results$c))]))
    
    
    
    
    peak <- sensitivity.df[[i]] %>%
      filter(transmission == tp[learn-1]) %>%
      filter(infections == max(infections))
    
    results_sensitivity[[i]][6,c(1,learn)] <- c("Peak Infections (Day)", peak$Day)
    
    peak <- sensitivity.df[[i]] %>%
      filter(transmission == tp[learn-1]) %>%
      filter(c == max(c))
    
    results_sensitivity[[i]][7,c(1,learn)] <- c("Peak Critical (Day)", peak$Day)
    
    results_sensitivity[[i]][8,c(1,learn)] <- c("End of simulation infections", round(end_of_sim_results$infections))
    
    over_capacity <- sensitivity.df[[i]] %>%
      filter(transmission == tp[learn-1]) %>%
      filter(c > capacity)
    
    results_sensitivity[[i]][9,c(1,learn)] <- c("Critical > Capacity (Days)", nrow(over_capacity))
    
    compliance_contacts <- sensitivity.df[[i]] %>%
      filter(transmission == tp[learn-1]) %>%
      summarise(contacts = mean(contacts),
                compliance = mean(compliance))
    
    results_sensitivity[[i]][10,c(1,learn)] <- c("Contacts", round(compliance_contacts$contacts,2))
    results_sensitivity[[i]][11,c(1,learn)] <- c("Compliance", round(compliance_contacts$compliance, 2))
    
  }
  
  
  
  results_sensitivity[[i]][,c(2:8)] <- map_df(results_sensitivity[[i]][,c(2:8)], prettyNum,  big.mark = ",")
  colnames(results_sensitivity[[i]])[2:8] <- gsub("X", "", colnames(results_sensitivity[[i]])[2:8])
  
  
  
  
}


# Write Table 6 to file

print(xtable(results_sensitivity[[1]], type = "latex", align = c("l", "l", rep("c",7))), include.rownames=FALSE, file = "results/tables/tp_lockdown_sensitivity.tex")

# Write Table 7 to file

print(xtable(results_sensitivity[[2]], type = "latex", align = c("l", "l", rep("c",7))), include.rownames=FALSE, file = "results/tables/tp_no_intervention_sensitivity.tex")

# TABLE 8  ---------------------------------------------------------------------------------

i <-  1
sensitivity.df <- list()
results_sensitivity <- list()
table_data <- list()

vaccination_strategies <- c("connection_based", "random", "risk_based")
city <- cities[1]


for (city in cities) {
  
  for(vaccination in vaccination_strategies){
    
    simulation_results <- paste(base_data_directory, city, learning[1], "vaccination", scenarios[1], vaccination_strategies[i], sep = "/")
    files_to_read <- list.files(simulation_results, pattern = "*.csv")
    
    simulation_list <- sapply(paste(simulation_results, files_to_read, sep = "/"), FUN = function(x) {
      out <- fread(x)}, simplify = FALSE)
    
    simulation_output[[i]] <- do.call(rbind, simulation_list)
    
    if(city == "cape_town"){
      population <- populations[1]
      city_name <- "Cape Town"
    }
    else{
      population <- populations[2]
      city_name <- "Johannesburg"
    }
    
    table_data[[i]] <- simulation_output[[i]] %>%
      mutate(i1 = i1 / model_population * population,
             i2 = i2 / model_population * population,
             c = c / model_population * population,
             d = d / model_population * population,
             r = r / model_population * population,
             s = s / model_population * population,
             e = e / model_population * population) %>%
      mutate(cum_infections = e) %>%
      mutate(infections = i1 + i2 + c) %>%
      group_by(Day = V1) %>%
      summarise(high_d = ci(d)[3],
                low_d = ci(d)[2],
                high_infections = ci(infections)[3],
                low_infections = ci(infections)[2],
                high_c = ci(c)[3],
                low_c = ci(c)[2],
                high_cum_infections = ci(cum_infections)[3],
                low_cum_infections = ci(cum_infections)[2],
                high_contacts = ci(contacts)[3],
                low_contacts = ci(contacts)[2],
                high_compliance = ci(compliance)[3],
                low_compliance = ci(compliance)[2],
                d = mean(d),
                r = mean(r),
                c = mean(c),
                e = mean(e),
                cum_infections = mean(cum_infections),
                infections = mean(infections),
                compliance = mean(compliance),
                contacts = mean(contacts))
    
    
    i <- i + 1
    
    
  }
  
  
}



if(city == "cape_town"){
  population <- populations[1]
  city_name <- "Cape Town"
  capacity <- cpt_hospital_capacity
}# else{
#  population <- populations[2]
#  city_name <- "Johannesburg"
#  capacity <- jhb_hospital_capacity
#}


results <- data.frame(`Outcome` = character(),
                      `Connection-based` = numeric(),
                      `Random`= numeric(),
                      `Risk-based`= numeric(),
                      stringsAsFactors=FALSE) 

end_of_sim_results <- table_data[[1]] %>%
  filter(Day < 180) %>%
  filter(Day == max(Day)) %>%
  mutate(City = city_name) %>%
  mutate(scenario == "Connection-based")

results[1,c(1,2)] <- c("Total Infections", round(sum(table_data[[1]]$cum_infections)/4))
results[2,c(1,2)] <- c("Total Critical cases", round(mean(table_data[[1]]$c)))
results[3,c(1,2)] <- c("Total Deaths", round(end_of_sim_results$d))
results[4,c(1,2)] <- c("Total Recoveries", round(end_of_sim_results$r))
results[5,c(1,2)] <- c("Contacts", round(mean(table_data[[1]]$contacts),2))
results[6,c(1,2)] <- c("Compliance",  round(mean(table_data[[1]]$compliance),2))

end_of_sim_results <- table_data[[2]] %>%
  filter(Day < 180) %>%
  filter(Day == max(Day)) %>%
  mutate(City = city_name) %>%
  mutate(scenario == "Random")

results[1,3] <- round(sum(table_data[[2]]$cum_infections)/4)
results[2,3] <- round(mean(table_data[[2]]$c))
results[3,3] <- round(end_of_sim_results$d)
results[4,3] <- round(end_of_sim_results$r)
results[5,3] <- round(mean(table_data[[2]]$contacts),2)
results[6,3] <- round(mean(table_data[[2]]$compliance),2)

end_of_sim_results <- table_data[[3]] %>%
  filter(Day < 180) %>%
  filter(Day == max(Day)) %>%
  mutate(City = city_name) %>%
  mutate(scenario == "Risk-based")


results[1,4] <- round(sum(table_data[[3]]$cum_infections)/4)
results[2,4] <- round(mean(table_data[[3]]$c))
results[3,4] <- round(end_of_sim_results$d)
results[4,4] <- round(end_of_sim_results$r)
results[5,4] <- round(mean(table_data[[3]]$contacts),2)
results[6,4] <- round(mean(table_data[[3]]$compliance),2)

results[,c(2:4)] <- map_df(results[,c(2:4)], prettyNum,  big.mark = ",")


# Write Table 8 to file

print(xtable(results, type = "latex", align = c("l", "l", rep("c",3))), include.rownames=FALSE, file = "results/tables/vaccination.tex")





