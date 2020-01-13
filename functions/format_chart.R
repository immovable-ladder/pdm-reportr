# formatting charts function
format_chart <- function(textang = 0, legend_name = "Legend", legend_pos = "top", line_height = 1) {
  list(
    theme_bw(),
    theme(axis.text.x = element_text(face = "bold", angle = textang, hjust = 1, lineheight = line_height),
          legend.position = legend_pos, legend.title = element_text(size = 9),
          legend.text = element_text(size = 7)),
    scale_fill_manual(values = mcColors, name = legend_name)
  )
}