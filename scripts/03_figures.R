#------------------------------------------------------------------------------

# This scripts renders all figures

#------------------------------------------------------------------------------

# FIGURE 4 --------------------------------------------------------------------

simulation_output <- list()
figure_data <- list()
i <- 1

for (city in cities) {
  for (scenario in scenarios) {
    simulation_results <- paste(base_data_directory, city, learning[1],
      scenario,
      sep = "/"
    )
    files_to_read <- list.files(simulation_results, pattern = "*.csv")

    simulation_list <- sapply(paste(simulation_results, files_to_read,
      sep = "/"
    ), FUN = function(x) {
      out <- fread(x)
    }, simplify = FALSE)

    simulation_output[[i]] <- do.call(rbind, simulation_list)

    if (city == "cape_town") {
      population <- populations[1]
      city_name <- "Cape Town"
    }
    else {
      population <- populations[2]
      city_name <- "Johannesburg"
    }

    figure_data[[i]] <- simulation_output[[i]] %>%
      mutate(
        i1 = i1 / model_population * population,
        i2 = i2 / model_population * population,
        c = c / model_population * population,
        d = d / model_population * population,
        r = r / model_population * population,
        s = s / model_population * population,
        e = e / model_population * population
      ) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
      mutate(infections = i1 + i2 + c) %>%
      group_by(Day = V1) %>%
      summarise(
        sd_d = sd(d),
        sd_r = sd(r),
        sd_cum_infections = sd(cum_infections),
        sd_infections = sd(infections),
        max_d = max(d),
        min_d = min(d),
        max_r = max(r),
        min_r = min(r),
        min_cum_infections = min(cum_infections),
        max_cum_infections = max(cum_infections),
        d = mean(d),
        r = mean(r),
        c = mean(c),
        cum_infections = mean(cum_infections),
        infections = mean(infections),
        compliance = mean(compliance),
        contacts = mean(contacts)
      ) %>%
      mutate(City = city_name)


    i <- i + 1
  }
}


cpt_data <- figure_data[[1]] %>%
  mutate(scenario = "Lockdown") %>%
  mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B"))


date_indices <- c(
  cpt_data$Day[which(cpt_data$Date == "01 May")],
  cpt_data$Day[which(cpt_data$Date == "01 June")],
  cpt_data$Day[which(cpt_data$Date == "01 July")],
  cpt_data$Day[which(cpt_data$Date == "01 August")],
  cpt_data$Day[which(cpt_data$Date == "01 September")],
  cpt_data$Day[which(cpt_data$Date == "01 October")]
)

cpt_data <- rbind.data.frame(cpt_data, figure_data[[2]] %>%
  mutate(scenario = "No intervention") %>%
  mutate(Date = format(as.Date("2020-04-17")
  + Day, "%d %B")))

cpt_data$label <- paste0("CPT: ", cpt_data$scenario)


jhb_data <- figure_data[[3]] %>%
  mutate(scenario = "Lockdown") %>%
  mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B"))

jhb_data <- rbind.data.frame(jhb_data, figure_data[[4]] %>%
  mutate(scenario = "No intervention") %>%
  mutate(Date = format(as.Date("2020-04-17") +
    Day, "%d %B")))
jhb_data$label <- paste0("JHB: ", jhb_data$scenario)

full_data <- rbind.data.frame(cpt_data, jhb_data)

# Write to file

jhb_peak_date <- jhb_data$Day[which(jhb_data$infections ==
  max(jhb_data$infections))]
jhb_peak_date_text <- format(as.Date("2020-04-17") + jhb_peak_date, "%d %B")

cpt_peak_date <- cpt_data$Day[which(cpt_data$infections ==
  max(cpt_data$infections))]
cpt_peak_date_text <- format(as.Date("2020-04-17") + cpt_peak_date, "%d %B")

pdf(paste0(output_folder, "/figures/infections", ".pdf"),
  width = 12,
  height = 8
)

print(scenario_figures_joint(full_data, "infections", "Infections") +
  geom_vline(
    xintercept = jhb_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[2]
  ) +
  geom_vline(
    xintercept = cpt_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[1]
  ) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/cum_infections", ".pdf"),
  width = 12,
  height = 8
)

print(scenario_figures_joint(full_data, "cum_infections", "Cumulative Infections") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))


dev.off()

jhb_critical_peak_date <- jhb_data$Day[which(jhb_data$c == max(jhb_data$c))]
jhb_critical_peak_date_text <- format(as.Date("2020-04-17") +
  jhb_critical_peak_date, "%d %B")

cpt_critical_peak_date <- cpt_data$Day[which(cpt_data$c == max(cpt_data$c))]
cpt_critical_peak_date_text <- format(as.Date("2020-04-17") +
  cpt_critical_peak_date, "%d %B")


pdf(paste0(output_folder, "/figures/critical", ".pdf"),
  width = 12,
  height = 8
)

print(scenario_figures_joint(full_data, "c", "Critical cases") +
  geom_hline(
    yintercept = cpt_hospital_capacity, linetype = "dashed",
    color = brewer.pal(6, "Dark2")[1]
  ) +
  geom_hline(
    yintercept = jhb_hospital_capacity, linetype = "dashed",
    color = brewer.pal(6, "Dark2")[2]
  ) +
  geom_vline(
    xintercept = jhb_critical_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[2]
  ) +
  geom_vline(
    xintercept = cpt_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[1]
  ) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/deaths", ".pdf"), width = 12, height = 8)

print(scenario_figures_joint(full_data, "d", "Deaths") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))


dev.off()

pdf(paste0(output_folder, "/figures/contacts", ".pdf"), width = 12, height = 8)

print(scenario_figures_joint(full_data, "contacts", "Contacts") +
  geom_vline(
    xintercept = jhb_critical_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[2]
  ) +
  geom_vline(
    xintercept = cpt_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[1]
  ) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/compliance", ".pdf"),
  width = 12,
  height = 8
)

print(scenario_figures_joint(full_data, "compliance", "Compliance") +
  geom_vline(
    xintercept = jhb_critical_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[2]
  ) +
  geom_vline(
    xintercept = cpt_peak_date, linetype = "solid",
    color = brewer.pal(n = 6, name = "Dark2")[1]
  ) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()



# FIGURE 5 --------------------------------------------------------------------

pdf(paste0(output_folder, "/figures/cpt_validation", ".pdf"),
  width = 12,
  height = 8
)
print(validation_figures(cpt_data, cpt_excess_deaths))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_validation", ".pdf"),
  width = 12,
  height = 8
)
print(validation_figures(jhb_data, jhb_excess_deaths))
dev.off()

# FIGURES 6 and 7--------------------------------------------------------------

scenarios <- c(
  "sensitivity_lockdown_private_weight",
  "sensitivity_no_intervention_private_weight"
)
i <- 1
sensitivity.df <- list()

for (city in cities) {
  for (scenario in scenarios) {
    sensitivity_directory <- paste(base_data_directory, city, learning[1],
      scenario,
      sep = "/"
    )
    simulation_runs <- list.files(sensitivity_directory)
    files_to_read <- list.files(paste(sensitivity_directory,
      simulation_runs[1],
      sep = "/"
    ),
    pattern = "*.csv"
    )
    sensitivity.list <- sapply(paste(sensitivity_directory, simulation_runs[1],
      files_to_read,
      sep = "/"
    ),
    FUN = function(x) {
      out <- fread(x)
    },
    simplify = FALSE
    )

    sensitivity.df[[i]] <- do.call(rbind, sensitivity.list)

    if (city == "cape_town") {
      population <- populations[1]
      city_name <- "Cape Town"
    } else {
      population <- populations[2]
      city_name <- "Johannesburg"
    }

    sensitivity.df[[i]] <- sensitivity.df[[i]] %>%
      mutate(
        i1 = i1 / 100000 * population,
        i2 = i2 / 100000 * population,
        c = c / 100000 * population,
        d = d / 100000 * population,
        r = r / 100000 * population,
        s = s / 100000 * population,
        e = e / 100000 * population,
        contacts = contacts / 100000 * population
      ) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
      mutate(infections = i1 + i2 + c) %>%
      group_by(Day = V1) %>%
      summarise(
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
        contacts = mean(contacts)
      ) %>%
      mutate(learning = as.character(sub(
        "^\\D+", "",
        sub(
          "^\\D+", "",
          simulation_runs[1]
        )
      )))


    for (j in 2:length(simulation_runs)) {
      files_to_read <- list.files(paste(sensitivity_directory,
        simulation_runs[j],
        sep = "/"
      ), pattern = "*.csv")

      sensitivity.list <- sapply(paste(sensitivity_directory,
        simulation_runs[j],
        files_to_read,
        sep = "/"
      ),
      FUN = function(x) {
        out <- fread(x)
      },
      simplify = FALSE
      )

      sensitivity_temp.df <- do.call(rbind, sensitivity.list)

      sensitivity_temp.df <- sensitivity_temp.df %>%
        mutate(
          i1 = i1 / 100000 * population,
          i2 = i2 / 100000 * population,
          c = c / 100000 * population,
          d = d / 100000 * population,
          r = r / 100000 * population,
          s = s / 100000 * population,
          e = e / 100000 * population,
          contacts = contacts / 100000 * population
        ) %>%
        mutate(cum_infections = i1 + i2 + d + c + r) %>%
        mutate(infections = i1 + i2 + c) %>%
        mutate(population) %>%
        mutate(infections = i1 + i2 + c) %>%
        group_by(Day = V1) %>%
        summarise(
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
          contacts = mean(contacts)
        ) %>%
        mutate(learning = as.character(sub(
          "^\\D+", "",
          sub(
            "^\\D+", "",
            simulation_runs[j]
          )
        )))

      sensitivity.df[[i]] <- rbind.data.frame(
        sensitivity.df[[i]],
        sensitivity_temp.df
      )
    }

    i <- i + 1
  }
}


# Write to file

pdf(paste0(
  output_folder, "/figures/cpt_infection_sensitivity_lockdown",
  ".pdf"
), width = 12, height = 8)
print(sensitivity_figures(
  "cape_town", "infections", "Infections",
  "lockdown"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "infections", "Infections", "lockdown") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/cpt_cum_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "cum_infections", "Cumulative Infections", "lockdown") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_cum_infection_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "cum_infections", "Cumulative Infections", "lockdown") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/cpt_critical_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "c", "Critical Cases", "lockdown") + geom_hline(
  yintercept = cpt_hospital_capacity,
  linetype = "dashed"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_critical_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "c", "Critical Cases", "lockdown") + geom_hline(
  yintercept = jhb_hospital_capacity,
  linetype = "dashed"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/cpt_death_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "d", "Deaths", "lockdown") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_death_sensitivity_lockdown", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "d", "Deaths", "lockdown") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()


pdf(paste0(output_folder, "/figures/cpt_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "infections", "Infections", "no_intervention") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "infections", "Infections", "no_intervention") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/cpt_cum_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "cum_infections", "Cumulative Infections", "no_intervention") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_cum_infection_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "cum_infections", "Cumulative Infections", "no_intervention") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/cpt_critical_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "c", "Critical Cases", "no_intervention") + geom_hline(
  yintercept = cpt_hospital_capacity,
  linetype = "dashed"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_critical_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "c", "Critical Cases", "no_intervention") + geom_hline(
  yintercept = jhb_hospital_capacity,
  linetype = "dashed"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/cpt_death_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("cape_town", "d", "Deaths", "no_intervention") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_death_sensitivity_no_intervention", ".pdf"), width = 12, height = 8)
print(sensitivity_figures("joburg", "d", "Deaths", "no_intervention") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()





# FIGURE 8 --------------------------------------------------------------------

simulation_output <- list()
figure_data <- list()
i <- 1
scenarios <- c("lockdown", "no_intervention")

for (city in cities) {
  for (learn in learning) {
    for (scenario in scenarios) {
      simulation_results <- paste(base_data_directory, city, learn, scenario, sep = "/")
      files_to_read <- list.files(simulation_results, pattern = "*.csv")

      simulation_list <- sapply(paste(simulation_results, files_to_read, sep = "/"), FUN = function(x) {
        out <- fread(x)
      }, simplify = FALSE)

      simulation_output[[i]] <- do.call(rbind, simulation_list)

      if (city == "cape_town") {
        population <- populations[1]
        city_name <- "Cape Town"
      }
      else {
        population <- populations[2]
        city_name <- "Johannesburg"
      }

      figure_data[[i]] <- simulation_output[[i]] %>%
        mutate(
          i1 = i1 / model_population * population,
          i2 = i2 / model_population * population,
          c = c / model_population * population,
          d = d / model_population * population,
          r = r / model_population * population,
          s = s / model_population * population,
          e = e / model_population * population
        ) %>%
        mutate(cum_infections = i1 + i2 + d + c + r) %>%
        mutate(infections = i1 + i2 + c) %>%
        group_by(Day = V1) %>%
        summarise(
          sd_d = sd(d),
          sd_r = sd(r),
          sd_cum_infections = sd(cum_infections),
          sd_infections = sd(infections),
          max_d = max(d),
          min_d = min(d),
          max_r = max(r),
          min_r = min(r),
          min_cum_infections = min(cum_infections),
          max_cum_infections = max(cum_infections),
          d = mean(d),
          r = mean(r),
          c = mean(c),
          cum_infections = mean(cum_infections),
          infections = mean(infections),
          compliance = mean(compliance),
          contacts = mean(contacts)
        ) %>%
        mutate(City = city_name)


      i <- i + 1
    }
  }
}

full_data <- figure_data[[1]] %>%
  mutate(scenario = "Lockdown") %>%
  mutate(learning = "De Groot") %>%
  mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B"))

date_indices <- c(
  full_data$Day[which(full_data$Date == "01 May")],
  full_data$Day[which(full_data$Date == "01 June")],
  full_data$Day[which(full_data$Date == "01 July")],
  full_data$Day[which(full_data$Date == "01 August")],
  full_data$Day[which(full_data$Date == "01 September")],
  full_data$Day[which(full_data$Date == "01 October")]
)

for (i in 2:8) {
  full_data <- rbind.data.frame(full_data, figure_data[[i]] %>%
    mutate(scenario = ifelse(i %in% c(3, 5, 7), "Lockdown", "No intervention")) %>%
    mutate(learning = ifelse(i %in% c(2, 5, 6), "De Groot", "Lexicographic")) %>%
    mutate(Date = format(as.Date("2020-04-17") + Day, "%d %B")))
}

full_data$label <- paste(full_data$learning, full_data$scenario, sep = "-")

# Write to file

pdf(paste0(output_folder, "/figures/cpt_lexicographic_infections", ".pdf"), width = 12, height = 8)

print(lexicographic_figures(
  full_data %>% filter(City == "Cape Town"),
  "infections", "Infections"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/cpt_lexicographic_compliance", ".pdf"), width = 12, height = 8)

print(lexicographic_figures(
  full_data %>% filter(City == "Cape Town"),
  "compliance", "Compliance"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/cpt_lexicographic_deaths", ".pdf"), width = 12, height = 8)

print(lexicographic_figures(
  full_data %>% filter(City == "Cape Town"),
  "d", "Deaths"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/jhb_lexicographic_infections", ".pdf"), width = 12, height = 8)

print(lexicographic_figures(
  full_data %>% filter(City == "Johannesburg"),
  "infections", "Infections"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/jhb_lexicographic_compliance", ".pdf"), width = 12, height = 8)

print(lexicographic_figures(
  full_data %>% filter(City == "Johannesburg"),
  "compliance", "Compliance"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

pdf(paste0(output_folder, "/figures/jhb_lexicographic_deaths", ".pdf"), width = 12, height = 8)

print(lexicographic_figures(
  full_data %>% filter(City == "Johannesburg"),
  "d", "Deaths"
) +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))

dev.off()

# FIGURES 9 and 10 ------------------------------------------------------------

learning.df <- list()

i <- 1

for (i in seq(1, 4)) {
  if (i == 1 | i == 2) {
    city <- "cape_town"
    population <- populations[1]
    city_name <- "Cape Town"
  } else {
    city <- "johannesburg"
    population <- populations[2]
    city_name <- "Johannesburg"
  }

  if (i == 1 | i == 3) {
    lockdown_scenario <- c("sensitivity_lockdown_private_weight", "sensitivity_no_intervention_private_weight")[1]
    scenario <- c("lockdown", "no_intervention")[1]
  } else {
    lockdown_scenario <- c("sensitivity_lockdown_private_weight", "sensitivity_no_intervention_private_weight")[2]
    scenario <- c("lockdown", "no_intervention")[2]
  }

  sensitivity_directory <- paste(base_data_directory, city, learning[1], lockdown_scenario, "1", sep = "/")
  files_to_read <- list.files(sensitivity_directory, pattern = "*.csv")
  sensitivity.list <- sapply(paste(sensitivity_directory, files_to_read, sep = "/"), FUN = function(x) {
    out <- fread(x)
  }, simplify = FALSE)

  learning.df[[i]] <- do.call(rbind, sensitivity.list)

  learning.df[[i]] <- learning.df[[i]] %>%
    mutate(
      i1 = i1 / 100000 * population,
      i2 = i2 / 100000 * population,
      c = c / 100000 * population,
      d = d / 100000 * population,
      r = r / 100000 * population,
      s = s / 100000 * population,
      e = e / 100000 * population,
      contacts = contacts / 100000 * population
    ) %>%
    mutate(cum_infections = i1 + i2 + d + c + r) %>%
    mutate(infections = i1 + i2 + c) %>%
    mutate(population) %>%
    mutate(infections = i1 + i2 + c) %>%
    group_by(Day = V1) %>%
    summarise(
      e = mean(e),
      i1 = mean(i1),
      i2 = mean(i2),
      s = mean(s),
      d = mean(d),
      c = mean(c),
      r = mean(r),
      compliance = mean(compliance),
      cum_infections = mean(cum_infections),
      infections = mean(infections),
      contacts = mean(contacts)
    ) %>%
    mutate(simulation = "No learning")


  simulation_results <- paste(base_data_directory, city, learning[1], scenario, sep = "/")
  files_to_read <- list.files(simulation_results, pattern = "*.csv")

  simulation_list <- sapply(paste(simulation_results, files_to_read, sep = "/"), FUN = function(x) {
    out <- fread(x)
  }, simplify = FALSE)

  temp <- do.call(rbind, simulation_list)

  if (city == "cape_town") {
    population <- populations[1]
    city_name <- "Cape Town"
  }
  else {
    population <- populations[2]
    city_name <- "Johannesburg"
  }

  temp <- temp %>%
    mutate(
      i1 = i1 / 100000 * population,
      i2 = i2 / 100000 * population,
      c = c / 100000 * population,
      d = d / 100000 * population,
      r = r / 100000 * population,
      s = s / 100000 * population,
      e = e / 100000 * population,
      contacts = contacts / 100000 * population
    ) %>%
    mutate(cum_infections = i1 + i2 + d + c + r) %>%
    mutate(infections = i1 + i2 + c) %>%
    mutate(population) %>%
    mutate(infections = i1 + i2 + c) %>%
    group_by(Day = V1) %>%
    summarise(
      e = mean(e),
      i1 = mean(i1),
      i2 = mean(i2),
      s = mean(s),
      d = mean(d),
      c = mean(c),
      r = mean(r),
      compliance = mean(compliance),
      cum_infections = mean(cum_infections),
      infections = mean(infections),
      contacts = mean(contacts)
    ) %>%
    mutate(simulation = "Learning")

  learning.df[[i]] <- rbind.data.frame(learning.df[[i]], temp)
}

# Write to file

pdf(paste0(output_folder, "/figures/cpt_learning_lockdown_infections", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[1]], "infections", "Infections") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/cpt_learning_lockdown_deaths", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[1]], "d", "Deaths") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/cpt_learning_lockdown_critical", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[1]], "c", "Critical cases") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/cpt_learning_no_intervention_infections", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[2]], "infections", "Infections") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/cpt_learning_no_intervention_deaths", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[2]], "d", "Deaths") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/cpt_learning_no_intervention_critical", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[2]], "c", "Critical cases"))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_learning_lockdown_infections", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[3]], "infections", "Infections") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/jhb_learning_lockdown_deaths", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[3]], "d", "Deaths") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/jhb_learning_lockdown_critical", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[3]], "c", "Critical cases") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()

pdf(paste0(output_folder, "/figures/jhb_learning_no_intervention_infections", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[4]], "infections", "Infections") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/jhb_learning_no_intervention_deaths", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[4]], "d", "Deaths") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
pdf(paste0(output_folder, "/figures/jhb_learning_no_intervention_critical", ".pdf"), width = 12, height = 8)
print(learning_figures(learning.df[[4]], "c", "Critical cases") +
  scale_x_continuous(
    breaks = date_indices,
    labels = c(
      "1 May",
      "1 Jun",
      "1 Jul",
      "1 Aug",
      "1 Sep",
      "1 Oct"
    )
  ))
dev.off()
