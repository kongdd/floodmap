
# ggplot2::theme_set(theme_grey(base_size = 16, base_family = "Times"))

#' @title plot YMSS
#' @description plot ymss
#' @import data.table ggplot2
#' @export
YMSS.plot <- function(X){
  ggplot(X, aes(x = TIME, y = value, colour = ymss, fill = ymss, shape = ymss)) +
    geom_point() + geom_line() + ylab("YMSS") +
    theme(#legend.position = c(1, 0), legend.justification = c(1, 0),
      legend.position="bottom",
      text = element_text(colour = "black"),
      axis.title = element_text(size = 16),
      axis.text = element_text(colour = "black", size = 14),
      axis.text.x = element_text(size = 14),
      legend.margin = unit(0, "cm"),
      legend.text = element_text(size = 14),
      legend.title = element_blank())
}
