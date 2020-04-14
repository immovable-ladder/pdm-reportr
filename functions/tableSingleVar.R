##TODO - document functions

tableSingleVar <- function(var, geo_name = "District"){
  filtered <- select(ona_df, geo, !!sym(var)) %>%
    filter(!is.na(!!sym(var))) %>% 
    count(!!sym(geo), !!sym(var)) %>% 
    merge(., respondents_pergeo, by = geo) %>% 
    mutate(`%` = round(n/totalgeo*100, digits = 2)) %>% 
    select(-totalgeo)
  
  names(filtered) <- c(geo_name, "Response", "# Reporting", paste0("% of ", geo_name))
  
  pandoc.table(filtered, split.table = Inf)
}