##TODO - document functions

plotSingleVar <- function(df = ona_df, var, factor_levels = "", nrow = 2){
    filtered <- select(df, geo, !!sym(var)) %>% 
    filter(!is.na(!!sym(var))) %>% 
    group_by(!!sym(geo), !!sym(var)) %>% 
    summarise(n = n()) %>% 
    merge(., respondents_pergeo, by = geo) %>% 
    mutate(`% of respondents` = round(n/totalgeo*100, digits = 2)) %>% 
    rename(Response = !!sym(var))
    
    if(length(factor_levels) > 1) {
      filtered$Response <- factor(filtered$Response,
                                  levels = factor_levels)
    } else {
      filtered$Response  <- with(filtered, reorder(Response, -`% of respondents`)) 
    }
  
    ggplot(filtered, aes(x = Response, 
                       y = `% of respondents`, fill = Response)) +
    geom_bar(position = position_dodge2(preserve = "single"), alpha = .9, stat = "identity") +
    facet_grid(. ~ !!sym(geo), space = "free_x") +
    format_chart() +
    scale_fill_pander() +
    theme(axis.text.x=element_blank()) +
    xlab("") +
    geom_text(aes(label = paste0(as.character(round(`% of respondents`, digits = 1)), "%")), 
              position = position_dodge(width=0.9), size = 2.5) +
    guides(fill=guide_legend(nrow = nrow, byrow = TRUE))
}