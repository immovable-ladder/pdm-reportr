##TODO - document functions

plotSingleVarBy <- function(df = ona_df, var, var_by, factor_levels = "", name_y = "Response"){
  
  # select the vars, set groupings by geo and specified var_by, and calculate % per geo
  filtered <- select(df, one_of(geo, var, var_by)) %>%
    group_by(!!sym(geo), !!sym(var_by)) %>% 
    summarise(n = n()) %>% 
    merge(., respondents_pergeo, by = geo) %>% 
    mutate(`% of respondents` = round(n/totalgeo*100, digits = 2)) %>% 
    rename(`name_y` = !!sym(var))
  
  
  if(length(factor_levels) > 1) {
    
    filtered[name_y] <- factor(filtered$Response,
                                levels = factor_levels)
    
  } else {
    
    filtered[name_y]  <- with(filtered, reorder(!!sym(name_y), -`% of respondents`)) 
  }
  
  ggplot(filtered, aes(x = !!sym(var_by), y = !!sym(name_y), fill = !!sym(var_by))) +
    geom_col() +
    xlab("") +
    facet_wrap(eval(expr(. ~ !!sym(geo))), labeller = labeller(groupwrap = label_wrap_gen(10))) +
    format_chart(textang = 45, legend_pos = "none") +
    scale_fill_pander() +
    geom_text(aes(label = paste0(as.character(round(`% of respondents`, digits = 1)), "%")), 
              position = position_dodge(width=0.9), size = 2.5)
}