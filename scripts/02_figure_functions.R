# --------------------------------------------------------------------------------

# Author: Allan Davids
# Email: allan.davids@uct.ac.za
# Date created: 1 December 2020
# Date last revised: 23 March 2021
# Purpose: Creates functions to render figures in paper

# --------------------------------------------------------------------------------

scenario_figures <- function(city_data, outcome, yaxis_label, alpha_bands = 0.6){
  
  scenario_figure <- ggplot(city_data %>%
                              filter(Day < 180)) +
    geom_line(aes_string(x = "Day", y = outcome, color = "scenario"), size = 2) +
    geom_ribbon(aes_string(x = "Day", ymin = paste("low", outcome, sep = "_"), ymax = paste("high", outcome, sep = "_"), fill = "scenario",), alpha = alpha_bands, show.legend = F) +
    ylab("") + 
    xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                       axis.title = element_text(size = 26), legend.text = element_text(size = 26), 
                                       legend.title = element_blank(), 
                                       legend.key.width = unit(1.5, "cm"), 
                                       legend.key.height = unit(1,"cm"), 
                                       axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),
                                       legend.position = "bottom") +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_fill_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + scale_x_continuous(breaks = seq(0, 200, 30))
  
  return(scenario_figure)
}

scenario_figures_joint <- function(city_data, outcome, yaxis_label){
  
  scenario_figure <- ggplot(full_data %>%
                              filter(Day < 180)) +
    geom_line(aes_string(x = "Day", y = outcome, color = "City", linetype = "scenario", group = "label"), size = 2) +
    ylab(yaxis_label) + 
    xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                       axis.title = element_text(size = 26), legend.text = element_text(size = 18), 
                                       legend.title = element_blank(), 
                                       legend.key.width = unit(1.5, "cm"), 
                                       legend.key.height = unit(1,"cm"), axis.text.x = element_text(angle = 0, vjust = 0.5),
                                       legend.position = c(.95, .95),
                                       legend.justification = c("right", "top"),
                                       legend.box.just = "right",
                                       legend.margin = margin(6, 6, 6, 6)
    ) +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) 
  
  return(scenario_figure)
}

validation_figures <- function(city_data, excess_deaths){
  
  city_data <- city_data[1:180,]
  
  temp <- city_data %>%
    filter(Day < 180) %>%
    mutate(deaths = "Simulated deaths")
  
  temp$d <- temp$d - dplyr::lag(temp$d, 1)
  temp$d[1] <- 0
  
  temp$low_d <- temp$low_d - dplyr::lag(temp$low_d, 1)
  temp$low_d[1] <- 0
  
  temp$high_d <- temp$high_d - dplyr::lag(temp$high_d, 1)
  temp$high_d[1] <- 0
  
  temp <- rbind.data.frame(temp, temp)
  
  temp$deaths[181:360] <- "Excess deaths"
  
  temp$d[181:360] <- c(rep(NA,44), excess_deaths, rep(NA, 180-98-44))
  
  
  validation_figure <- ggplot(temp) +
    geom_line(aes(x = Day, y = d, color = deaths), size = 2) +
    geom_ribbon(aes_string(x = "Day", ymin = "low_d", ymax = "high_d"), alpha = 0.6, fill = brewer.pal(n = 6, name = "Dark2")[1], show.legend = F) +
    scale_color_manual(values = c("Black", brewer.pal(n = 6, name = "Dark2")[1])) +
    ylab("") + 
    xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                       axis.title = element_text(size = 26), legend.text = element_text(size = 26), 
                                       legend.position = "bottom", legend.title = element_blank(), 
                                       legend.key.width = unit(1.5, "cm"), 
                                       legend.key.height = unit(1,"cm"), axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
    scale_y_continuous(breaks = seq(0, 125, 25), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + scale_x_continuous(breaks = seq(0, 200, 30))
  
  
  error <- (temp$d[1:180] - temp$d[181:360])[!is.na((temp$d[1:180] - temp$d[181:360]))]
  n <- length(error)
  print(paste0("RMSE = ", round(sqrt(sum(error^2)/n), 2)))
  
  return(validation_figure)
  
}

sensitivity_figures <- function(city, outcome, yaxis_label, scenario, parameter){
  
  if(city == "joburg" & scenario == "lockdown"){
    
    list_element <- 3
    
  }
  else if(city == "joburg" & scenario == "no_intervention"){
    
    list_element <- 4
    
  }
  else if(city == "cape_town" & scenario == "lockdown"){
    
    list_element <- 1
    
  } 
  else {
    
    list_element <- 2
    
  }
  
  
  if(parameter == "parameters_wps"){
    
    sensitivity_figure <- ggplot(sensitivity.df[[list_element]] %>%
                                   filter(Day < 180) %>%
                                   filter(private_signal %in% wps[c(1:3, 6, 11, 16, 21)])) +
      geom_line(aes_string(x = "Day", y = outcome, color = "private_signal"), size = 1.5) +
      geom_ribbon(aes_string(x = "Day", ymin = paste("low", outcome, sep = "_"), ymax = paste("high", outcome, sep = "_"), fill = "private_signal",), alpha = 0.25, show.legend = F) +
      ylab("") + 
      xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                         axis.title = element_text(size = 26), legend.text = element_text(size = 26), 
                                         legend.title = element_blank(), 
                                         legend.key.width = unit(1.5, "cm"), 
                                         legend.key.height = unit(1,"cm"), 
                                         axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),
                                         legend.position = "bottom") +
      scale_color_manual(values = brewer.pal(n = 7, name = "Dark2")) + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + scale_x_continuous(breaks = seq(0, 200, 30)) + scale_fill_manual(values = brewer.pal(n = 7, name = "Dark2")) + guides()
    
  } else if (parameter == "parameters_tp") {
    
    sensitivity_figure <- ggplot(sensitivity.df[[list_element]] %>%
                                   filter(Day < 180)) +
      geom_line(aes_string(x = "Day", y = outcome, color = "transmission"), size = 1.5) +
      geom_ribbon(aes_string(x = "Day", ymin = paste("low", outcome, sep = "_"), ymax = paste("high", outcome, sep = "_"), fill = "transmission",), alpha = 0.25, show.legend = F) +
      ylab("") + 
      xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                         axis.title = element_text(size = 26), legend.text = element_text(size = 26), 
                                         legend.title = element_blank(), 
                                         legend.key.width = unit(1.5, "cm"), 
                                         legend.key.height = unit(1,"cm"), 
                                         axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),
                                         legend.position = c(.95, .95),
                                         legend.justification = c("right", "top"),
                                         legend.box.just = "right",
                                         legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(values = brewer.pal(n = 7, name = "Dark2")) + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + scale_x_continuous(breaks = seq(0, 200, 30)) + scale_fill_manual(values = brewer.pal(n = 7, name = "Dark2")) 
    
  } else{
    
    sensitivity_figure <- ggplot(sensitivity.df[[list_element]] %>%
                                   filter(Day < 180)) +
      geom_line(aes_string(x = "Day", y = outcome, color = "omega"), size = 1.5) +
      geom_ribbon(aes_string(x = "Day", ymin = paste("low", outcome, sep = "_"), ymax = paste("high", outcome, sep = "_"), fill = "omega",), alpha = 0.25, show.legend = F) +
      ylab("") + 
      xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                         axis.title = element_text(size = 26), legend.text = element_text(size = 26), 
                                         legend.title = element_blank(), 
                                         legend.key.width = unit(1.5, "cm"), 
                                         legend.key.height = unit(1,"cm"),
                                         axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),
                                         legend.position = c(.95, .95),
                                         legend.justification = c("right", "top"),
                                         legend.box.just = "right",
                                         legend.margin = margin(6, 6, 6, 6)) +
      scale_color_manual(values = brewer.pal(n = 7, name = "Dark2")) + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + scale_x_continuous(breaks = seq(0, 200, 30)) + scale_fill_manual(values = brewer.pal(n = 7, name = "Dark2")) 
    
    
  }
  
  
  
  
  return(sensitivity_figure)
}

learning_figures <- function(city_data, outcome, yaxis_label){
  
  learning_figure <- ggplot(city_data %>%
                              filter(Day < 180)) +
    geom_line(aes_string(x = "Day", y = outcome, color = "simulation"), size = 2) +
    ylab("") + 
    xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                       axis.title = element_text(size = 26), legend.text = element_text(size = 26), 
                                       legend.position = "bottom", legend.title = element_blank(), 
                                       legend.key.width = unit(1.5, "cm"), 
                                       legend.key.height = unit(1,"cm"), 
                                       axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) + 
    scale_color_manual(values =brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) + scale_x_continuous(breaks = seq(0, 200, 30))
  
  return(learning_figure)  
  
}

lexicographic_figures <- function(city_data, outcome, yaxis_label){
  
  scenario_figure <- ggplot(city_data %>%
                              filter(Day < 180)) +
    geom_line(aes_string(x = "Day", y = outcome, color = "learning", linetype = "scenario", group = "label"), size = 2) +
    ylab(yaxis_label) + 
    xlab("") + theme_classic() + theme(axis.text = element_text(size = 24), 
                                       axis.title = element_text(size = 26), legend.text = element_text(size = 18), 
                                       legend.position = "bottom", legend.title = element_blank(), 
                                       legend.key.width = unit(1.5, "cm"), 
                                       legend.key.height = unit(1,"cm"), axis.text.x = element_text(angle = 0, vjust = 0.5)) +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  
  
  return(scenario_figure)
}