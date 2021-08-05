# -------------------------------------------------------------------------------------------

# Author: Allan Davids
# Email: allan.davids@uct.ac.za
# Date created: 1 December 2020
# Date last revised: 23 March 2021
# Purpose: Writes figures in paper

# -------------------------------------------------------------------------------------------

# FIGURE 3a ---------------------------------------------------------------------------------

simulation_output <- list()
figure_data <- list()
i <- 1

for (city in cities) {
  
  for(scenario in scenarios){
    
    simulation_results <- paste(base_data_directory, city, learning[1], scenario, sep = "/")
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
    
    figure_data[[i]] <- simulation_output[[i]] %>%
      mutate(i1 = i1 / model_population * population,
             i2 = i2 / model_population * population,
             c = c / model_population * population,
             d = d / model_population * population,
             r = r / model_population * population,
             s = s / model_population * population,
             e = e / model_population * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
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
                cum_infections = mean(cum_infections),
                infections = mean(infections),
                compliance = mean(compliance),
                contacts = mean(contacts)) %>%
      mutate(City = city_name)
    
    
    i <- i + 1
    
    
  }
  
  
}

cpt_data <- figure_data[[1]] %>%
  mutate(scenario = "Lockdown") %>%
  mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B"))

date_indices <- c(cpt_data$Day[which(cpt_data$Date == "01 April")],
                  cpt_data$Day[which(cpt_data$Date == "01 May")],
                  cpt_data$Day[which(cpt_data$Date == "01 June")],
                  cpt_data$Day[which(cpt_data$Date == "01 July")],
                  cpt_data$Day[which(cpt_data$Date == "01 August")],
                  cpt_data$Day[which(cpt_data$Date == "01 September")],
                  cpt_data$Day[which(cpt_data$Date == "01 October")])

cpt_data <- rbind.data.frame(cpt_data, figure_data[[2]] %>%
                               mutate(scenario = "No intervention")  %>%
                               mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B")))

cpt_data$label <- paste0(cpt_data$scenario)

date_labels = c("1 May", "1 Jun", "1 Jul", "1 Aug", "1 Sep", "1 Oct")

# Write Figure 3a to file

pdf(paste0(output_folder, "/figures/validation", ".pdf"), width = 12, height = 8)
print(validation_figures(cpt_data, cpt_excess_deaths) +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels))
dev.off()


# FIGURE 6 ----------------------------------------------------------------------------------

cpt_peak_date <- cpt_data$Day[which(cpt_data$infections == max(cpt_data$infections))]
cpt_peak_date_text <- format(as.Date("2020-04-17") + cpt_peak_date, "%d %B")

pdf(paste0(output_folder, "/figures/infections", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "infections", "Infections")  +
        geom_vline(xintercept = cpt_peak_date, linetype = "dashed") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))

dev.off()

pdf(paste0(output_folder, "/figures/cum_infections", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "cum_infections", "Cumulative infections")  +
        geom_vline(xintercept = cpt_peak_date, linetype = "dashed") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .80),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))


dev.off()

cpt_critical_peak_date <- cpt_data$Day[which(cpt_data$c == max(cpt_data$c))]
cpt_critical_peak_date_text <- format(as.Date("2020-04-17") + cpt_critical_peak_date, "%d %B")


pdf(paste0(output_folder, "/figures/critical", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "c", "Critical cases") + 
        geom_hline(yintercept = cpt_hospital_capacity, linetype = "dashed") +
        geom_vline(xintercept = cpt_critical_peak_date, linetype = "dashed") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))

dev.off()

pdf(paste0(output_folder, "/figures/deaths", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "d", "Deaths") +
        geom_vline(xintercept = cpt_peak_date, linetype = "dashed") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .80),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))


dev.off()

pdf(paste0(output_folder, "/figures/contacts", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "contacts", "Contacts") +
        geom_vline(xintercept = cpt_peak_date, linetype = "dashed") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .80),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))

dev.off()

pdf(paste0(output_folder, "/figures/compliance", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "compliance", "Compliance") +
        geom_vline(xintercept = cpt_peak_date, linetype = "dashed") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .80),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))

dev.off()

# FIGURE 3b ---------------------------------------------------------------------------------

city <- cities[1]
p <- "parameters_wps"
wps <- seq(0, 1, 0.05)
i <-  1

sensitivity.df <- list()

for (city in cities) {
  
  for(scenario in scenarios){
    
    sensitivity_directory <- paste(base_data_directory, city, learning[1], "sensitivity", scenario, p, sep = "/")
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
             e = e / 100000 * population,
             contacts = contacts/100000 * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
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
                v_d = var(d),
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
               e = e / 100000 * population,
               contacts = contacts/100000 * population) %>%
        mutate(cum_infections = i1 + i2 + d + c + r) %>%
        mutate(infections = i1 + i2 + c) %>%
        mutate(population) %>%
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
                  v_d = var(d),
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

pdf(paste0(output_folder, "/figures/validation_no_learning", ".pdf"), width = 12, height = 8)
print(validation_figures(sensitivity.df[[1]] %>% filter(private_signal == 1), cpt_excess_deaths) +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels))
dev.off()

# FIGURE 4 TOP ROW and FIGURE 7--------------------------------------------------------------

# Write top row of Figure 4 to file

pdf(paste0(output_folder, "/figures/wps_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "infections", "Infections", "lockdown", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/wps_cum_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "cum_infections", "Cumulative infections", "lockdown", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .45),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/wps_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "infections", "Infections", "no_intervention", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/wps_cum_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "cum_infections", "Cumulative infections", "no_intervention", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .45),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/wps_compliance_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "compliance", "Compliance", "no_intervention", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels))
dev.off()

pdf(paste0(output_folder, "/figures/wps_contacts_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "contacts", "Contacts", "no_intervention", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels))
dev.off()
pdf(paste0(output_folder, "/figures/wps_critical_sensitivity_lockdown", ".pdf"), width = 12, height = 8)

print(sensitivity_figures("cape_town", "c", "Critical", "lockdown", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/wps_deaths_sensitivity_lockdown", ".pdf"), width = 12, height = 8)

print(sensitivity_figures("cape_town", "d", "Deaths", "lockdown", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .45),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/wps_critical_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)

print(sensitivity_figures("cape_town", "c", "Critical", "no_intervention", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/wps_deaths_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)

print(sensitivity_figures("cape_town", "d", "Deaths", "no_intervention", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .45),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

# FIGURE 5  ---------------------------------------------------------------------------------

pdf(paste0(output_folder, "/figures/wps_contacts_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "contacts", "Contacts", "lockdown", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels))
dev.off()

pdf(paste0(output_folder, "/figures/wps_compliance_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "compliance", "Compliance", "lockdown", "parameters_wps") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels))
dev.off()

# FIGURE 4 MIDDLE ROW -----------------------------------------------------------------------

i <- 1
city <- cities[1]
p <- "parameters_tp"
tp <- c(0.027, 0.028, 0.029, 0.030, 0.031, 0.032, 0.033)

sensitivity.df <- list()

for (city in cities) {
  
  for(scenario in scenarios){
    
    sensitivity_directory <- paste(base_data_directory, city, learning[1], "sensitivity", scenario, p, sep = "/")
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
             e = e / 100000 * population,
             contacts = contacts/100000 * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
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
                v_d = var(d),
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
               e = e / 100000 * population,
               contacts = contacts/100000 * population) %>%
        mutate(cum_infections = i1 + i2 + d + c + r) %>%
        mutate(infections = i1 + i2 + c) %>%
        mutate(population) %>%
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
                  v_d = var(d),
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

# Write middle row of Figure 4 to file

pdf(paste0(output_folder, "/figures/tp_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "infections", "Infections", "lockdown", "parameters_tp") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tp_cum_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "cum_infections", "Cumulative infections", "lockdown", "parameters_tp") +
        scale_x_continuous(breaks = date_indices,
                           labels =date_labels) +
        theme(legend.position = c(.95, .45),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tp_critical_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "c", "Critical", "lockdown", "parameters_tp") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tp_deaths_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "d", "Deaths", "lockdown", "parameters_tp") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .43),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tp_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "infections", "Infections", "no_intervention", "parameters_tp") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tp_cum_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "cum_infections", "Cumulative infections", "no_intervention", "parameters_tp") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels)+
        theme(legend.position = c(.95, .85),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

# FIGURE 4 BOTTOM ROW -----------------------------------------------------------------------

city <- cities[1]
p <- "parameters_tm"
tm <- seq(0.26, 0.56, 0.05)
i <-  1

sensitivity.df <- list()

for (city in cities) {
  
  for(scenario in scenarios[1]){
    
    sensitivity_directory <- paste(base_data_directory, city, learning[1], "sensitivity", scenario, p, sep = "/")
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
             e = e / 100000 * population,
             contacts = contacts/100000 * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
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
                v_d = var(d),
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
      mutate(omega = as.character(sub("^\\D+", "", sub("^\\D+", "", tm[1]))))
    
    
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
               e = e / 100000 * population,
               contacts = contacts/100000 * population) %>%
        mutate(cum_infections = i1 + i2 + d + c + r) %>%
        mutate(infections = i1 + i2 + c) %>%
        mutate(population) %>%
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
                  v_d = var(d),
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
        mutate(omega = as.character(sub("^\\D+", "", sub("^\\D+", "", tm[j]))))
      
      sensitivity.df[[i]] <- rbind.data.frame(sensitivity.df[[i]], sensitivity_temp.df)
      
      
    }
    
    i <- i + 1
    
  }
  
}

# Write bottom row of Figure 4 to file

pdf(paste0(output_folder, "/figures/tm_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "infections", "Infections", "lockdown", "parameters_tm") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tm_cum_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "cum_infections", "Cumulative infections", "lockdown", "parameters_tm") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(1, .15),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6),
              legend.direction = "horizontal"))
dev.off()



pdf(paste0(output_folder, "/figures/tm_compliance_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "compliance", "Compliance", "lockdown", "parameters_tm") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tm_contacts_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "contacts", "Contacts", "lockdown", "parameters_tm") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .15),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6),
              legend.direction = "horizontal"))
dev.off()

pdf(paste0(output_folder, "/figures/tm_critical_sensitivity_lockdown", ".pdf"), width = 12, height = 8)

print(sensitivity_figures("cape_town", "c", "Critical", "lockdown", "parameters_tm") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

pdf(paste0(output_folder, "/figures/tm_deaths_sensitivity_lockdown", ".pdf"), width = 12, height = 8)

print(sensitivity_figures("cape_town", "d", "Deaths", "lockdown", "parameters_tm") +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = c(.25, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)))
dev.off()

# FIGURE 8 ----------------------------------------------------------------------------------

vaccination_strategies <- c("connection_based", "random", "risk_based")

simulation_output <- list()
figure_data <- list()
i <- 1

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
    
    figure_data[[i]] <- simulation_output[[i]] %>%
      mutate(i1 = i1 / model_population * population,
             i2 = i2 / model_population * population,
             c = c / model_population * population,
             d = d / model_population * population,
             r = r / model_population * population,
             s = s / model_population * population,
             e = e / model_population * population) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
      mutate(infections = i1 + i2 + c,
             e_d = d - lag(d)) %>%
      group_by(Day = V1) %>%
      summarise(high_e_d = ci(e_d)[3],
                low_e_d = ci(e_d)[2],
                high_d = ci(d)[3],
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
                e_d = mean(e_d),
                cum_infections = mean(cum_infections),
                infections = mean(infections),
                compliance = mean(compliance),
                contacts = mean(contacts))
    
    
    i <- i + 1
    
    
  }
  
  
}

cpt_data <- figure_data[[1]] %>%
  mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B")) %>%
  mutate(scenario = "Connection-based") %>%
  rbind.data.frame(figure_data[[2]] %>%
                     mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B")) %>%
                     mutate(scenario = "Random")) %>%
  rbind.data.frame(figure_data[[3]] %>%
                     mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B")) %>%
                     mutate(scenario = "Risk-based"))

# Write Figure 8 to file

pdf(paste0(output_folder, "/figures/infections_vaccination", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "infections", "Infections", 0.2)  +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = "bottom"))  

dev.off()

pdf(paste0(output_folder, "/figures/critical_vaccination", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "c", "Critical cases", 0.2) + 
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = "bottom"))  

dev.off()

pdf(paste0(output_folder, "/figures/deaths_vaccination", ".pdf"), width = 12, height = 8)

print(scenario_figures(cpt_data, "d", "Deaths", 0.2) +
        scale_x_continuous(breaks = date_indices,
                           labels = date_labels) +
        theme(legend.position = "bottom")) 


dev.off()
