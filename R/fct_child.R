

# kV ----------------------------------------------------------------------

get_kV <- function(weight_kg) {
  
  stopifnot(is.numeric(weight_kg) & weight_kg > 0)
  
  kV <- if(weight_kg < 20) {
    "80"
  } else if (weight_kg == 20) {
    "80 or 100"
  } else if (weight_kg < 45) {
    "100"
  } else if (weight_kg == 45) {
    "100 or 120"
  } else if (weight_kg <= 60) {
    "120"
  } else {
    # weight_kg ≥ 60
    "120 ?"
  }
  
  kV
  
}


# Contrast (ml) -----------------------------------------------------------

get_contrast_ml <- function(ml_kg, weight_kg){
  
  stopifnot(is.numeric(weight_kg) & weight_kg > 0)
  ml <- ml_kg * weight_kg
  
  # Maximum contrast = 80 ml
  if (ml >= 80) return(80)
  round(ml, 1)
  
}

print_contrast <- function(ml_kg, weight_kg) {
  
  ml <- ml_kg * weight_kg
  ml_adj_txt <- if (ml >= 80) {
    "80 ml [maximum]"
  } else {
    paste0(round(ml, 1), " ml")
  }
  
  cat(glue::glue("Contrast: {ml_adj_txt} ({ml_kg} ml/kg * {weight_kg} kg = {ml}) "))
  
}


# Rate (ml/sec) -----------------------------------------------------------


print_rate_ml_sec <- function(contrast_ml, 
                              rate_formula = c("no_delay", "delay"),
                              delay_sec = NULL
) {
  
  rate_formula <- match.arg(rate_formula)
  stopifnot(is.numeric(contrast_ml) & contrast_ml > 0)
  
  switch (rate_formula,
          "no_delay" = {
            
            rate <- (contrast_ml + 15) / 45 
            rate_adj <- round_up_decimal(rate, 1) # Round up for 1 decimal
            
            show_calc <- glue::glue("({contrast_ml} + 15) / 45 = {round(rate, 3)}")
            
            
          },
          "delay" = { 
            
            stopifnot(is.numeric(delay_sec))
            
            rate <- (contrast_ml + 15) / (delay_sec - 15) 
            rate_adj <- round_up_decimal(rate, 1) # Round up for 1 decimal
            
            show_calc <- glue::glue("({contrast_ml} + 15) / ({delay_sec} - 15) = {round(rate, 3)}")
            
          }
  )
  
  cat(glue::glue("Rate: {rate_adj} ml/sec {show_calc}"))
}


# Helper: Round Up --------------------------------------------------------


round_up_decimal <- function(x, level = 1) {
  z <- 10^level
  ceiling(x * z) / z
}
