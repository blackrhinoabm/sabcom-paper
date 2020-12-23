#------------------------------------------------------------------------------

# This scripts renders all tables

#------------------------------------------------------------------------------

# TABLE 5 ---------------------------------------------------------------------

simulation_output <- list()
results <- list()
peak_results <- list()
compliance_list <- list()
contact_list <- list()

i <- 1

# Loop through cities and lockdown scenarios

for (city in cities) {
  for (scenario in scenarios) {
    simulation_results <- paste(base_data_directory, city, learning[1],
      scenario,
      sep = "/"
    )
    files_to_read <- list.files(simulation_results, pattern = "*.csv")

    simulation_list <- sapply(paste(simulation_results,
      files_to_read,
      sep = "/"
    ),
    FUN = function(x) {
      out <- fread(x)
    },
    simplify = FALSE
    )

    for (l in seq(simulation_list)) {
      simulation_list[[l]]$seed_id <- files_to_read[l]
    }

    simulation_output[[i]] <- do.call(rbind, simulation_list)

    if (city == "cape_town") {
      population <- populations[1]
      city_name <- "Cape Town"
      capacity <- cpt_hospital_capacity
    } else {
      population <- populations[2]
      city_name <- "Johannesburg"
      capacity <- jhb_hospital_capacity
    }

    # Store results which are recorded at the end of the simulation

    end_of_sim_results <- simulation_output[[i]] %>%
      mutate(
        i1 = i1 / model_population * population,
        i2 = i2 / model_population * population,
        c = c / model_population * population,
        d = d / model_population * population,
        r = r / model_population * population,
        s = s / model_population * population,
        e = e / model_population * population
      ) %>%
      filter(V1 < 180) %>%
      filter(V1 == max(V1)) %>%
      mutate(cum_infections = i1 + i2 + d + c + r) %>%
      group_by(Day = V1) %>%
      summarise(
        sd_d = sd(d),
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
        cum_infections = mean(cum_infections)
      ) %>%
      mutate(City = city_name)

    results[[i]] <- data.frame(
      `Outcome` = character(),
      `Min` = numeric(),
      `Mean` = numeric(),
      `Max` = numeric(),
      `SD` = numeric(),
      stringsAsFactors = FALSE
    )

    results[[i]][1, ] <- c(
      "Total Infections",
      round(end_of_sim_results$min_cum_infections),
      round(end_of_sim_results$cum_infections),
      round(end_of_sim_results$max_cum_infections),
      round(end_of_sim_results$sd_cum_infections)
    )


    results[[i]][2, ] <- c(
      "Total Deaths",
      round(end_of_sim_results$min_d),
      round(end_of_sim_results$d),
      round(end_of_sim_results$max_d),
      round(end_of_sim_results$sd_d)
    )

    results[[i]][3, ] <- c(
      "Total Recoveries",
      round(end_of_sim_results$min_r),
      round(end_of_sim_results$r),
      round(end_of_sim_results$max_r),
      round(end_of_sim_results$sd_r)
    )

    # Store results which are recorded at their maximum in the simulation

    max_of_sim_results <- simulation_output[[i]] %>%
      mutate(
        i1 = i1 / model_population * population,
        i2 = i2 / model_population * population,
        c = c / model_population * population,
        d = d / model_population * population,
        r = r / model_population * population,
        s = s / model_population * population,
        e = e / model_population * population
      ) %>%
      mutate(
        cum_infections = i1 + i2 + d + c + r,
        infections = i1 + i2 + c
      ) %>%
      filter(V1 < 180) %>%
      group_by(Day = V1) %>%
      summarise(
        sd_c = sd(c),
        sd_infections = sd(infections),
        max_c = max(c),
        min_c = min(c),
        min_infections = min(infections),
        max_infections = max(infections),
        c = mean(c),
        infections = mean(infections)
      ) %>%
      mutate(City = city_name)

    results[[i]][4, ] <- c(
      "Peak Infections",
      round(max_of_sim_results$min_infections[which(
        max_of_sim_results$infections ==
          max(max_of_sim_results$infections)
      )]),
      round(max_of_sim_results$infections[which(
        max_of_sim_results$infections ==
          max(max_of_sim_results$infections)
      )]),
      round(max_of_sim_results$max_infections[which(
        max_of_sim_results$infections ==
          max(max_of_sim_results$infections)
      )]),
      round(max_of_sim_results$sd_infections[which(
        max_of_sim_results$infections ==
          max(max_of_sim_results$infections)
      )])
    )


    results[[i]][5, ] <- c(
      "Peak Critical",
      round(max_of_sim_results$min_c[which(
        max_of_sim_results$c ==
          max(max_of_sim_results$c)
      )]),
      round(max_of_sim_results$c[which(
        max_of_sim_results$c ==
          max(max_of_sim_results$c)
      )]),
      round(max_of_sim_results$max_c[which(
        max_of_sim_results$c ==
          max(max_of_sim_results$c)
      )]),
      round(max_of_sim_results$sd_c[which(
        max_of_sim_results$c ==
          max(max_of_sim_results$c)
      )])
    )

    results[[i]][, c(2:5)] <- map_df(results[[i]][, c(2:5)], prettyNum,
      big.mark = ","
    )


    temp <- simulation_output[[i]] %>%
      group_by((row_number() - 1) %/% (n() / 49)) %>%
      nest() %>%
      pull(data)

    get_peak_infections <- function(df, outcome) {
      peak <- df %>%
        mutate(Day = V1) %>%
        mutate(infections = i1 + i2 + c) %>%
        filter(infections == max(infections)) %>%
        mutate(Day = V1)

      return(peak$Day[1])
    }

    infections_peak <- as.integer(lapply(temp, get_peak_infections))


    results[[i]][6, ] <- c(
      "Peak Infections (Day)",
      round(min(infections_peak)),
      round(mean(infections_peak)),
      round(max(infections_peak)),
      round(sd(infections_peak))
    )

    get_peak_critical <- function(df, outcome) {
      peak <- df %>%
        mutate(Day = V1) %>%
        filter(c == max(c)) %>%
        mutate(Day = V1)

      return(peak$Day[1])
    }

    critical_peak <- as.integer(lapply(temp, get_peak_critical))


    results[[i]][7, ] <- c(
      "Peak Critical (Day)",
      round(min(critical_peak)),
      round(mean(critical_peak)),
      round(max(critical_peak)),
      round(sd(critical_peak))
    )

    get_days_overcapcity <- function(df) {
      over_capacity <- df %>%
        mutate(c = c / model_population * population) %>%
        group_by(Day = V1) %>%
        summarise(c = mean(c)) %>%
        filter(c > capacity)


      return(nrow(over_capacity))
    }

    overcapacity <- as.integer(lapply(temp, get_days_overcapcity))

    results[[i]][8, ] <- c(
      "Critical > Capacity (Days)",
      round(min(overcapacity)),
      round(mean(overcapacity)),
      round(max(overcapacity)),
      round(sd(overcapacity))
    )



    compliance_list[[i]] <- simulation_output[[i]] %>%
      group_by(seed_id) %>%
      summarise(compliance = mean(compliance))

    contact_list[[i]] <- simulation_output[[i]] %>%
      group_by(seed_id) %>%
      summarise(contacts = mean(contacts))

    results[[i]][9, ] <- c(
      "Contacts",
      round(min(contact_list[[i]]$contacts), 2),
      round(mean(contact_list[[i]]$contacts), 2),
      round(max(contact_list[[i]]$contacts), 2),
      round(sd(contact_list[[i]]$contacts), 2)
    )

    results[[i]][10, ] <- c(
      "Compliance",
      round(min(compliance_list[[i]]$compliance), 2),
      round(mean(compliance_list[[i]]$compliance), 2),
      round(max(compliance_list[[i]]$compliance), 2),
      round(sd(compliance_list[[i]]$compliance), 2)
    )


    i <- i + 1
  }
}



# Write to file

print(xtable(results[[1]], type = "latex", align = c("l", "l", rep("c", 4))),
  include.rownames = FALSE, file = "results/tables/cpt_lockdown.tex"
)

print(xtable(results[[2]], type = "latex", align = c("l", "l", rep("c", 4))),
  include.rownames = FALSE, file = "results/tables/cpt_no_intervention.tex"
)

print(xtable(results[[3]], type = "latex", align = c("l", "l", rep("c", 4))),
  include.rownames = FALSE, file = "results/tables/jhb_lockdown.tex"
)

print(xtable(results[[4]], type = "latex", align = c("l", "l", rep("c", 4))),
  include.rownames = FALSE, file = "results/tables/jhb_no_intervention.tex"
)

# TABLE 4 ---------------------------------------------------------------------

i <- 1
sensitivity.df <- list()
results_sensitivity <- list()

sensitivity_scenarios <- c(
  "sensitivity_lockdown_private_weight",
  "sensitivity_no_intervention_private_weight"
)


for (city in cities) {
  for (s in sensitivity_scenarios) {
    sensitivity_directory <- paste(base_data_directory, city, learning[1],
      s,
      sep = "/"
    )
    simulation_runs <- list.files(sensitivity_directory)
    files_to_read <- list.files(paste(sensitivity_directory,
      simulation_runs[1],
      sep = "/"
    ),
    pattern = "*.csv"
    )
    sensitivity.list <- sapply(paste(sensitivity_directory,
      simulation_runs[1], files_to_read,
      sep = "/"
    ), FUN = function(x) {
      out <- fread(x)
    }, simplify = FALSE)

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
        e = e / 100000 * population
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
      mutate(learning = as.character(sub("^\\D+", "", sub(
        "^\\D+", "",
        simulation_runs[1]
      ))))


    for (j in 2:length(simulation_runs)) {
      files_to_read <- list.files(paste(sensitivity_directory,
        simulation_runs[j],
        sep = "/"
      ),
      pattern = "*.csv"
      )

      sensitivity.list <- sapply(paste(sensitivity_directory,
        simulation_runs[j], files_to_read,
        sep = "/"
      ), FUN = function(x) {
        out <- fread(x)
      }, simplify = FALSE)

      sensitivity_temp.df <- do.call(rbind, sensitivity.list)

      sensitivity_temp.df <- sensitivity_temp.df %>%
        mutate(
          i1 = i1 / 100000 * population,
          i2 = i2 / 100000 * population,
          c = c / 100000 * population,
          d = d / 100000 * population,
          r = r / 100000 * population,
          s = s / 100000 * population,
          e = e / 100000 * population
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
        mutate(learning = as.character(sub("^\\D+", "", sub(
          "^\\D+", "",
          simulation_runs[j]
        ))))

      sensitivity.df[[i]] <- rbind.data.frame(
        sensitivity.df[[i]],
        sensitivity_temp.df
      )
    }

    i <- i + 1
  }
}


if (city == "cape_town") {
  population <- populations[1]
  city_name <- "Cape Town"
  capacity <- cpt_hospital_capacity
} else {
  population <- populations[2]
  city_name <- "Johannesburg"
  capacity <- jhb_hospital_capacity
}


for (i in seq(1, 4)) {
  results_sensitivity[[i]] <- data.frame(
    `Outcome` = character(),
    `0.0` = numeric(),
    `0.2` = numeric(),
    `0.4` = numeric(),
    `0.6` = numeric(),
    `0.8` = numeric(),
    `1.0` = numeric(),
    stringsAsFactors = FALSE
  )

  for (learn in seq(2, 7)) {
    end_of_sim_results <- sensitivity.df[[i]] %>%
      filter(learning == seq(0, 1, 0.2)[learn - 1]) %>%
      filter(Day < 180) %>%
      filter(Day == max(Day)) %>%
      mutate(City = city_name)

    results_sensitivity[[i]][1, c(1, learn)] <- c(
      "Total Infections",
      round(end_of_sim_results$cum_infections)
    )
    results_sensitivity[[i]][2, c(1, learn)] <- c(
      "Total Deaths",
      round(end_of_sim_results$d)
    )
    results_sensitivity[[i]][3, c(1, learn)] <- c(
      "Total Recoveries",
      round(end_of_sim_results$r)
    )



    max_of_sim_results <- sensitivity.df[[i]] %>%
      filter(learning == seq(0, 1, 0.2)[learn - 1]) %>%
      filter(Day < 180)

    results_sensitivity[[i]][4, c(1, learn)] <- c(
      "Peak Infections",
      round(max_of_sim_results$infections[which(
        max_of_sim_results$infections ==
          max(max_of_sim_results$infections)
      )])
    )

    results_sensitivity[[i]][5, c(1, learn)] <- c(
      "Peak Critical",
      round(max_of_sim_results$c[which(
        max_of_sim_results$c ==
          max(max_of_sim_results$c)
      )])
    )




    peak <- sensitivity.df[[i]] %>%
      filter(learning == seq(0, 1, 0.2)[learn - 1]) %>%
      filter(infections == max(infections))

    results_sensitivity[[i]][6, c(1, learn)] <- c(
      "Peak Infections (Day)",
      peak$Day
    )

    peak <- sensitivity.df[[i]] %>%
      filter(learning == seq(0, 1, 0.2)[learn - 1]) %>%
      filter(c == max(c))

    results_sensitivity[[i]][7, c(1, learn)] <- c(
      "Peak Critical (Day)",
      peak$Day
    )


    over_capacity <- sensitivity.df[[i]] %>%
      filter(learning == seq(0, 1, 0.2)[learn - 1]) %>%
      filter(c > capacity)

    results_sensitivity[[i]][8, c(1, learn)] <- c(
      "Critical > Capacity (Days)",
      nrow(over_capacity)
    )

    compliance_contacts <- sensitivity.df[[i]] %>%
      filter(learning == seq(0, 1, 0.2)[learn - 1]) %>%
      summarise(
        contacts = mean(contacts),
        compliance = mean(compliance)
      )

    results_sensitivity[[i]][9, c(1, learn)] <- c(
      "Contacts",
      round(
        compliance_contacts$contacts,
        2
      )
    )
    results_sensitivity[[i]][10, c(1, learn)] <- c(
      "Compliance",
      round(
        compliance_contacts$compliance,
        2
      )
    )
  }



  results_sensitivity[[i]][, c(2:7)] <- map_df(results_sensitivity[[i]][, c(2:7)],
    prettyNum,
    big.mark = ","
  )
  colnames(results_sensitivity[[i]])[2:7] <- gsub(
    "X", "",
    colnames(results_sensitivity[[i]])[2:7]
  )
}




# Write to file
print(xtable(results_sensitivity[[1]],
  type = "latex",
  align = c("l", "l", rep("c", 6))
),
include.rownames = FALSE,
file = "results/tables/cpt_sensitivity.tex"
)
print(xtable(results_sensitivity[[3]],
  type = "latex",
  align = c("l", "l", rep("c", 6))
),
include.rownames = FALSE,
file = "results/tables/jhb_sensitivity.tex"
)

# TABLE 3 --------------------------------------------------------------------

# Lockdown

learning_result_cpt <- results[[1]][, c(1, 3)]

no_learning_result_cpt <- results_sensitivity[[1]][, c(1, 7)]

colnames(learning_result_cpt)[2] <- "Learning"
colnames(no_learning_result_cpt)[2] <- "No Learning"

full_learning_results <- left_join(
  learning_result_cpt,
  no_learning_result_cpt
)

learning_result_jhb <- results[[3]][, c(1, 3)]

no_learning_result_jhb <- results_sensitivity[[3]][, c(1, 7)]

colnames(learning_result_jhb)[2] <- "Learning_JHB"
colnames(no_learning_result_jhb)[2] <- "No Learning_JHB"

full_learning_results <- left_join(
  full_learning_results,
  learning_result_jhb
)
full_learning_results <- left_join(
  full_learning_results,
  no_learning_result_jhb
)

print(xtable(full_learning_results,
  type = "latex",
  align = c("l", "l", rep("c", 4))
),
include.rownames = FALSE, file = "results/tables/lockdown_learning.tex"
)

# No Intervention

learning_result_cpt <- results[[2]][, c(1, 3)]

no_learning_result_cpt <- results_sensitivity[[2]][, c(1, 7)]

colnames(learning_result_cpt)[2] <- "Learning"
colnames(no_learning_result_cpt)[2] <- "No Learning"

full_learning_results <- left_join(
  learning_result_cpt,
  no_learning_result_cpt
)

learning_result_jhb <- results[[4]][, c(1, 3)]

no_learning_result_jhb <- results_sensitivity[[4]][, c(1, 7)]

colnames(learning_result_jhb)[2] <- "Learning_JHB"
colnames(no_learning_result_jhb)[2] <- "No Learning_JHB"

full_learning_results <- left_join(
  full_learning_results,
  learning_result_jhb
)
full_learning_results <- left_join(
  full_learning_results,
  no_learning_result_jhb
)

print(xtable(full_learning_results,
  type = "latex",
  align = c("l", "l", rep("c", 4))
),
include.rownames = FALSE,
file = "results/tables/no_intervention_learning.tex"
)
