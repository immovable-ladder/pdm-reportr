##TODO - document functions
#TODO - fix the circularity here - one function references another and lookups are an argument in both

plotMultiVarSummary <- function(varsToChart, lookup_table = lookup_yn){
  labels <- as.vector(as.matrix(select(arrange(filter(data_dict, pdm_var %in% varsToChart), desc(pdm_var)), question)))
  
  ggplot(generateYesNoDF(lookup = lookup_table), aes(x = variable, y = value, fill = !!sym(geo))) +
    geom_col(position = "dodge", alpha = .9) +
    labs(x = "", 
         y = "% of respondents") +
    scale_x_discrete(labels = str_wrap(labels, width = 30)) + 
    coord_flip() +
    format_chart(legend_name = geo) +
    geom_text(aes(label = paste0(as.character(round(value, digits = 1)), "%")), 
              position = position_dodge(width=0.9), size = 2.5)
}