# Design by Study


# CT Chest ----------------------------------------------------------------

design_chest <- function(age_group,
                         weight_kg
) {
  
  age_group <- match.arg(age_group, 
                         choices = c("younger_child", "older_child"))
  
  cat("-- Design CT --", "\n\n")
  cat("CT chest, venous only","\n")
  # Weight
  cat("Body weight:", weight_kg, "kg", "\n")
  
  # kV
  kV <- get_kV(weight_kg)
  cat("kV:", kV, "\n")
  # mA
  cat("mA: auto", "\n")
  # Noise
  noise_index <- switch (age_group,
                         "younger_child" = 17,
                         "older_child" = 20
  )
  cat("Noise index:", noise_index, "\n")
  # Delay
  cat("Delay: 45 sec", "\n")
  # Contrast
  ml_kg <-  1 # Chest
  print_contrast(ml_kg = ml_kg, weight_kg = weight_kg); cat("\n")
  # Rate
  print_rate_ml_sec(
    contrast_ml = get_contrast_ml(ml_kg = ml_kg, weight_kg = weight_kg), 
    rate_formula = "no_delay"
  )
  cat("\n\n---")
  
}


# CT Whole Abdomen --------------------------------------------------------

design_whole_abd <- function(age_group,
                             weight_kg, 
                             rate_formula = c("no_delay", "delay"),
                             delay_sec = NULL
){
  
  age_group <- match.arg(age_group,
                         choices = c("younger_child", "older_child"))
  rate_formula <- match.arg(rate_formula)
  
  cat("-- Design CT --", "\n\n")
  cat("CT whole abdomen, venous only","\n")
  # Weight
  cat("Body weight:", weight_kg, "kg", "\n")
  
  # kV
  kV <- get_kV(weight_kg)
  cat("kV:", kV, "\n")
  # mA
  cat("mA: auto", "\n")
  # Noise
  noise_index <- switch (age_group,
                         "younger_child" = 15,
                         "older_child" = 17
  )
  cat("Noise index:", noise_index, "\n")
  # Delay
  if (rate_formula == "no_delay") {
    cat("Delay: 60 or 65 or 70 sec", "\n")
  } else {
    cat("Delay:", delay_sec, "\n")
  }
  
  # Contrast
  ml_kg <- 2 # WA
  print_contrast(ml_kg = ml_kg, weight_kg = weight_kg); cat("\n")
  # Rate
  print_rate_ml_sec(
    contrast_ml = get_contrast_ml(ml_kg = ml_kg, weight_kg = weight_kg),
    rate_formula = rate_formula,
    delay_sec = delay_sec
  )
  cat("\n\n---")
  
}


# CT Chest + WA -----------------------------------------------------------

design_chest_whole_abd <- function(age_group,
                                   weight_kg, 
                                   rate_formula = c("no_delay", "delay"),
                                   delay_sec = NULL
){
  
  age_group <- match.arg(age_group,
                         choices = c("younger_child", "older_child"))
  rate_formula <- match.arg(rate_formula)
  
  cat("-- Design CT --", "\n\n")
  cat("Venous chest + whole abdomen ","\n")
  # Weight
  cat("Body weight:", weight_kg, "kg", "\n")
  
  # kV
  kV <- get_kV(weight_kg)
  cat("kV:", kV, "\n")
  # mA
  cat("mA: auto", "\n")
  # Noise
  noise_index <- switch (age_group,
                         "younger_child" = 15,
                         "older_child" = 17
  )
  cat("Noise index:", noise_index, "\n")
  # Delay
  if (rate_formula == "no_delay") {
    cat("Delay: 60 or 65 or 70 sec", "\n")
  } else {
    cat("Delay:", delay_sec, "\n")
  }
  
  # Contrast
  ml_kg <- 2 # WA
  print_contrast(ml_kg = ml_kg, weight_kg = weight_kg); cat("\n")
  # Rate
  print_rate_ml_sec(
    contrast_ml = get_contrast_ml(ml_kg = ml_kg, weight_kg = weight_kg),
    rate_formula = rate_formula,
    delay_sec = delay_sec
  )
  cat("\n\n---")
  
}


# CTA liver ---------------------------------------------------------------

design_cta_liver <- function(age_group,
                             weight_kg
){
  
  age_group <- match.arg(age_group,
                         choices = c("younger_child", "older_child"))
  
  cat("-- Design CT --", "\n\n")
  cat("CTA liver","\n")
  # Weight
  cat("Body weight:", weight_kg, "kg", "\n")
  
  # kV
  kV <- get_kV(weight_kg)
  cat("kV:", kV, "\n")
  # mA
  cat("mA: auto", "\n")
  # Noise
  noise_index <- switch (age_group,
                         "younger_child" = 15,
                         "older_child" = 17
  )
  cat("Noise index:", noise_index, "\n")
  # Delay
  cat("Delay: 20 sec (CTA); 70 sec (Venous)", "\n")
  
  # Contrast
  ml_kg <- 2.5 # WA
  print_contrast(ml_kg = ml_kg, weight_kg = weight_kg); cat("\n")
  ml <- get_contrast_ml(ml_kg = ml_kg, weight_kg = weight_kg)
  
  # Rate
  rate <- (ml + 15) / 20
  rate_adj <- round_up_decimal(rate, 1) # Round up for 1 decimal
  cat(glue::glue("Rate: {rate_adj} ml/sec ({ml} + 15) / 20 = {round(rate, 3)}"))
  
  cat("\n\n---")
  
}

