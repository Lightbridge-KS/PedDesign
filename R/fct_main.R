print_design_ct <- function(study_type, 
                            age_group,
                            weight_kg,
                            iv_location =  c("arm", "leg"),
                            rate_formula = c("no_delay", "delay"),
                            delay_sec = NULL
) {
  
  study_type <- match.arg(study_type, 
                          choices = c("chest", "whole_abd",
                                      "chest_whole_abd", "cta_liver"))
  age_group <- match.arg(age_group, 
                         choices = c("younger_child", "older_child"))
  iv_location <- match.arg(iv_location)
  
  cat("-- Design CT --", "\n\n")
  
  # Study Type
  cat(study_type_desc[[study_type]], "\n")
  
  # kV
  kV <- get_kV(weight_kg)
  cat(glue::glue("kV: {kV}"), "\n")
  # mA
  cat("mA: auto", "\n")
  
  # Noise index
  noise_index <- get_noise_index(study_type, age_group)
  cat(glue::glue("Noise index: {noise_index}"), "\n")
  
  # Delay
  cat(print_delay_sec(study_type, iv_location), "\n")
  
  # Contrast (ml/kg)
  contrast_ml <- get_contrast_ml(study_type, weight_kg)
  cat(print_contrast(study_type, weight_kg), "\n")
  
  # Rate (ml/sec)
  cat(print_rate_ml_sec(contrast_ml, 
                        rate_formula = rate_formula, 
                        delay_sec = delay_sec), "\n\n")
  
  cat("---")
  
}