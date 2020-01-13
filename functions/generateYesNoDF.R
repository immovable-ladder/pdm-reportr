# function to generate a filtered df of Yes/No variables for charting
## Inputs: df, grouping variable, variables to convert from coded to uncoded
## uses a lookup_yn table which must exist for this to work
##TODO add parameters to specify the Yes and No codes
# as.data.frame call required because there's a known issue with dplyr outputs which gives it attributes that are not just df-like, so it needs to be converted to a df before melt will work

generateYesNoDF <- function(df = ona_df, grouping = "geo", uncode = varsToChart, lookup = lookup_yn) {
  select(df, one_of(vars[[grouping]], uncode)) %>% 
    mutate_at(vars(one_of(uncode)), funs(lookup$value[match(., lookup$factor)])) %>% 
    group_by(!!sym(vars[[grouping]])) %>% 
    summarise_at(uncode, function(x) (sum(x, na.rm = T)/n()*100)) %>%
    as.data.frame(.) %>%  
    melt(., id.vars = vars[[grouping]])
}