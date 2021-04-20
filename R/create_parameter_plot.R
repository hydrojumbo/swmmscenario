#' Create a plot comparing a single parameter from multiple sources, such as multiple scenarios.
#' @param scenariosFrame A "wide" data frame with a 'Time' column and a series of each source/scenario's values.
#' @param yAxisTitle The label to provide the y-axis (describe the parameter visualized across sources)
#' @import reshape2
#' @import ggplot2
#' @return ggplot plot with each source/scenario as its own series and a legend mapping series to color.
#' @export
create_parameter_plot <- function(scenariosFrame, yAxisTitle) {
  dataMelted <- reshape2::melt(scenariosFrame, id.var="Time")

  #aes_string: https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  plot <- ggplot(dataMelted, aes_string(x="Time", y="value", col="variable")) +
    geom_line(size=1.8) +
    labs(x = "Time",
         y = yAxisTitle,
         color = "Legend")

  return(plot)
}
