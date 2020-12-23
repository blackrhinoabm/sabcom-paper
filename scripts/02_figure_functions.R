#------------------------------------------------------------------------------

# This script creates the functions to render figures in the paper

#------------------------------------------------------------------------------


scenario_figures <- function(city_data, outcome, yaxis_label) {
  scenario_figure <- ggplot(city_data %>%
    filter(Day < 180)) +
    geom_line(aes_string(
      x = "Day", y = outcome,
      color = "scenario"
    ), size = 2) +
    ylab(yaxis_label) +
    xlab("Days") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 26),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)
    ) +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels = function(x) {
      format(x,
        big.mark = ",",
        scientific = FALSE
      )
    }) +
    scale_x_continuous(breaks = seq(0, 200, 30))

  return(scenario_figure)
}

scenario_figures_joint <- function(city_data, outcome, yaxis_label) {
  scenario_figure <- ggplot(full_data %>%
    filter(Day < 180)) +
    geom_line(aes_string(
      x = "Day", y = outcome, color = "City",
      linetype = "scenario", group = "label"
    ), size = 2) +
    ylab(yaxis_label) +
    xlab("") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 18),
      legend.position = "bottom", legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    ) +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels = function(x) {
      format(x,
        big.mark = ",",
        scientific = FALSE
      )
    })

  return(scenario_figure)
}

validation_figures <- function(city_data, excess_deaths) {
  temp <- city_data %>%
    filter(Day < 180) %>%
    filter(scenario == "Lockdown") %>%
    mutate(deaths = "Simulated deaths")

  temp$d <- temp$d - dplyr::lag(temp$d, 1)
  temp$d[1] <- 0

  temp <- rbind.data.frame(temp, temp)

  temp$deaths[181:360] <- "Excess deaths"

  temp$d[181:360] <- c(excess_deaths, rep(NA, 180 - 117))

  validation_figure <- ggplot(temp) +
    geom_line(aes(x = Day, y = d, color = deaths), size = 2) +
    scale_color_manual(values = c("Black", brewer.pal(
      n = 6,
      name = "Dark2"
    )[1])) +
    ylab("Deaths") +
    xlab("Days") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 26),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      axis.text.x = element_text(
        angle = 0,
        hjust = 1, vjust = 0.5
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, 125, 25),
      labels = function(x) {
        format(x,
          big.mark = ",",
          scientific = FALSE
        )
      }
    ) +
    scale_x_continuous(breaks = seq(0, 200, 30))

  print(paste0(
    "The average squared difference for ", unique(temp$City),
    " is: ", round(mean((temp$d[1:117] - temp$d[181:297])^2))
  ))

  return(validation_figure)
}

sensitivity_figures <- function(city, outcome, yaxis_label, scenario) {
  if (city == "joburg" & scenario == "lockdown") {
    list_element <- 3
  }
  else if (city == "joburg" & scenario == "no_intervention") {
    list_element <- 4
  }
  else if (city == "cape_town" & scenario == "lockdown") {
    list_element <- 1
  }
  else {
    list_element <- 2
  }

  sensitivity_figure <- ggplot(sensitivity.df[[list_element]] %>%
    filter(learning %in% c(
      "0", "0.2", "0.4",
      "0.6", "0.8", "1"
    )) %>%
    filter(Day < 180)) +
    geom_line(aes_string(x = "Day", y = outcome, color = "learning"), size = 1.5) +
    ylab(yaxis_label) +
    xlab("Days") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 26),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)
    ) +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels = function(x) {
      format(x,
        big.mark = ",",
        scientific = FALSE
      )
    }) +
    scale_x_continuous(breaks = seq(0, 200, 30))

  return(sensitivity_figure)
}

learning_figures <- function(city_data, outcome, yaxis_label) {
  learning_figure <- ggplot(city_data %>%
    filter(Day < 180)) +
    geom_line(aes_string(x = "Day", y = outcome, color = "simulation"),
      size = 2
    ) +
    ylab(yaxis_label) +
    xlab("Days") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 26),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)
    ) +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels = function(x) {
      format(x,
        big.mark = ",",
        scientific = FALSE
      )
    }) +
    scale_x_continuous(breaks = seq(0, 200, 30))

  return(learning_figure)
}

lexicographic_figures <- function(city_data, outcome, yaxis_label) {
  scenario_figure <- ggplot(city_data %>%
    filter(Day < 180)) +
    geom_line(aes_string(
      x = "Day", y = outcome, color = "learning",
      linetype = "scenario", group = "label"
    ), size = 2) +
    ylab(yaxis_label) +
    xlab("") +
    theme_classic() +
    theme(
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 18),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    ) +
    scale_color_manual(values = brewer.pal(n = 6, name = "Dark2")) +
    scale_y_continuous(labels = function(x) {
      format(x,
        big.mark = ",",
        scientific = FALSE
      )
    })


  return(scenario_figure)
}
