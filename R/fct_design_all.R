# Main Wrapper for App

design_ct <- function(study_type,
                      age_group,
                      weight_kg,
                      rate_formula = c("no_delay", "delay"),
                      delay_sec = NULL) {
  
  study_type <- match.arg(study_type, 
                          choices = c("chest", "whole_abd",
                                      "chest_whole_abd", "cta_liver"))
  age_group <- match.arg(age_group, 
                         choices = c("younger_child", "older_child"))
  rate_formula <- match.arg(rate_formula)
  
  # CT Chest
  if(study_type == "chest") {
    design_chest(age_group = age_group, weight_kg = weight_kg)
  }
  # CT Whole Abdomen
  if(study_type == "whole_abd") {
    design_whole_abd(age_group = age_group, weight_kg = weight_kg,
                     rate_formula = rate_formula, delay_sec = delay_sec)
  }
  # CT Chest + WA
  if(study_type == "chest_whole_abd") {
    design_chest_whole_abd(age_group = age_group, weight_kg = weight_kg,
                           rate_formula = rate_formula, delay_sec = delay_sec)
  }
  # CTA Liver
  if(study_type == "cta_liver") {
    design_cta_liver(age_group = age_group, weight_kg = weight_kg)
  }
}