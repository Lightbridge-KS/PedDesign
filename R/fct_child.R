

# Study Type Description --------------------------------------------------

study_type_desc <- list(
  "chest" = "CT chest, venous only", 
  "whole_abd" = "CT whole abdomen, venous only",
  "chest_whole_abd" = "Venous chest + whole abdomen", 
  "cta_liver" = "CTA liver" 
)

# Get kV ------------------------------------------------------------------

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



# Get Noise Index ---------------------------------------------------------

get_noise_index <- function(study_type, age_group){
  
  study_type <- match.arg(study_type, 
                          choices = c("chest", "whole_abd",
                                      "chest_whole_abd", "cta_liver"))
  age_group <- match.arg(age_group, 
                         choices = c("younger_child", "older_child"))
  
  ls <- list(
    chest = c(younger_child = "17", older_child = "20"),
    whole_abd = c(younger_child = "15", older_child = "17"),
    chest_whole_abd = c(younger_child = "15", older_child = "17"),
    cta_liver = c(younger_child = "15", older_child = "17")
  )
  
  ls[[study_type]][[age_group]]
  
}



# Print Delay (sec) ---------------------------------------------------------

print_delay_sec <- function(study_type, 
                            iv_location =  c("arm", "leg")
){
  
  study_type <- match.arg(study_type, 
                          choices = c("chest", "whole_abd",
                                      "chest_whole_abd", "cta_liver"))
  iv_location <- match.arg(iv_location)
  
  
  delay_sec <- list(
    chest = "Delay: 45 sec", 
    whole_abd = "Delay: 60 or 65 or 70 sec", 
    chest_whole_abd = "Delay: 60 or 65 or 70 sec",  
    cta_liver = "Delay: 20 sec (CTA); 70 sec (Venous)"
  )
  
  switch (iv_location,
          "arm" = { cat(delay_sec[[study_type]]) },
          "leg" = { 
            cat(delay_sec[[study_type]], 
                "(If IV access via leg, consider +10 sec)")
          }
  )
  
  
}




# Get Contrast (ml/kg) -------------------------------------------------------

get_contrast_ml_kg <- function(study_type) {
  
  study_type <- match.arg(study_type, 
                          choices = c("chest", "whole_abd",
                                      "chest_whole_abd", "cta_liver"))
  
  ml_per_kg <- list(chest = 1, 
                    whole_abd = 2, chest_whole_abd = 2, 
                    cta_liver = 2.5)
  
  ml_per_kg[[study_type]]
  
}


# Get Contrast (ml) -------------------------------------------------------

get_contrast_ml <- function(study_type, weight_kg){
  
  stopifnot(is.numeric(weight_kg) & weight_kg > 0)
  study_type <- match.arg(study_type, 
                          choices = c("chest", "whole_abd",
                                      "chest_whole_abd", "cta_liver"))
  
  contrast_ml_kg <- get_contrast_ml_kg(study_type)
  
  contrast_ml <- contrast_ml_kg * weight_kg
  
  # Maximum contrast = 80 ml
  if (contrast_ml >= 80) return(80)
  
  contrast_ml
  
}


# Print Contrast ----------------------------------------------------------

print_contrast <- function(study_type, weight_kg){
  
  study_type <- match.arg(study_type, 
                          choices = c("chest", "whole_abd",
                                      "chest_whole_abd", "cta_liver"))
  
  contrast_ml_kg <- get_contrast_ml_kg(study_type)
  contrast_ml_precise <- contrast_ml_kg * weight_kg
  
  contrast_ml <- round(get_contrast_ml(study_type, weight_kg), 1)
  
  show_calc <- glue::glue("({contrast_ml_kg} ml/kg * {weight_kg} kg = {contrast_ml_precise})")
  
  # Max Dose
  if(contrast_ml == 80) {
    glue::glue("Contrast: 80 ml [Maximum] {show_calc}")
  } else {
    glue::glue("Contrast: {contrast_ml} ml {show_calc}")
  }
  
}



# Print Rate (ml/sec) -------------------------------------------------------

print_rate_ml_sec <- function(contrast_ml, 
                              rate_formula = c("no_delay", "delay"),
                              delay_sec = NULL
) {
  
  rate_formula <- match.arg(rate_formula)
  stopifnot(is.numeric(contrast_ml) & contrast_ml > 0)
  
  # Ceiling 1 decimal
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  
  
  switch (rate_formula,
          "no_delay" = {
            
            rate <- (contrast_ml + 15) / 45 
            rate_adj <- ceiling_dec(rate, 1) # Round up for 1 decimal
            
            show_calc <- glue::glue("({contrast_ml} + 15) / 45 = {round(rate, 2)}")
            
            
          },
          "delay" = { 
            
            stopifnot(is.numeric(delay_sec))
            
            rate <- (contrast_ml + 15) / (delay_sec - 15) 
            rate_adj <- ceiling_dec(rate, 1) # Round up for 1 decimal
            
            show_calc <- glue::glue("({contrast_ml} + 15) / ({delay_sec} - 15) = {round(rate, 2)}")
            
          }
  )
  
  glue::glue("Rate: {rate_adj} ml/sec {show_calc}")
}

