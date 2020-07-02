# function to generate a filtered df of Yes/No variables for charting
## Inputs: df, grouping variable, variables to convert from coded to uncoded
## uses a lookup_yn table which must exist for this to work
# as.data.frame call required because there's a known issue with dplyr outputs which gives it attributes that are not just df-like, so it needs to be converted to a df before melt will work

generateYesNoDF <- function(df = df, grouping = NA, uncode = varsToChart, lookup = lookup_yn, 
                            match_labels = NA, match_x = NA, match_y = NA) {

    temp_df <-  select(df, one_of(grouping, uncode)) %>% 
      mutate_at(vars(one_of(uncode)), funs(lookup$value[match(., lookup$factor)])) %>% 
      group_by(!!sym(grouping)) %>% 
      summarise(across(any_of({{uncode}}), function(x) (sum(x, na.rm = T)/n()*100))) %>%
      as.data.frame(.) %>%  
      #melt(., id.vars = grouping)
      pivot_longer(-!!sym(grouping), names_to = "variable", values_to = "value")

    if(!is.na(match_labels)) {
      temp_df <- merge(temp_df, match_labels, by.x = match_x, by.y = match_y)
    }
    
    return(temp_df)
}