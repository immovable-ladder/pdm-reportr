##TODO - document functions

plotMultiVar <- function(var_marker, factor_levels = "", nrow = 2){
  filtered <- select(ona_df, geo, contains(var_marker)) %>%
    reshape2::melt(., id.vars = geo) %>% 
    group_by(!!sym(geo), variable) %>% 
    summarise(n = sum(value, na.rm = T)) %>% 
    merge(., respondents_pergeo, by = geo) %>% 
    mutate(`% of respondents` = round(n/totalgeo*100, digits = 2))
  
  filtered$variable <- gsub(paste0(var_marker, "\\/"), "", filtered$variable)
  
  if(length(factor_levels) > 1) {
    filtered$variable <- factor(filtered$variable,
                              levels = factor_levels)
  } else {
    filtered$variable  <- with(filtered, reorder(variable, -`% of respondents`))
  }
  
  filtered <- rename(filtered, Response = variable)
  
  ggplot(filtered, aes(x = Response, 
                       y = `% of respondents`, fill = Response)) +
    geom_bar(position = position_dodge2(preserve = "single"), alpha = .9, stat = "identity") +
    facet_grid(. ~ !!sym(vars$geo), space = "free_x") +
    format_chart() +
    scale_fill_pander() +
    theme(axis.text.x=element_blank()) +
    xlab("") +
    geom_text(aes(label = paste0(as.character(round(`% of respondents`, digits = 1)), "%")), 
              position = position_dodge(width=0.9), size = 2.5) +
    guides(fill=guide_legend(nrow = nrow, byrow = TRUE))
}