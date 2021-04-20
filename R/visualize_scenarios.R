#' Draws a multi-panel graphic of the key parameters of a SWMM model across multiple design scenarios.
#' @param outputFiles List of paths to SWMM '.out' output files with data to visualize. Each scenario being compared should have 1 output file.
#' @param outputScenarioNames Labels for the time-series of each scenario. This list must be in the same order as outputFiles.
#' @param storageNodes List of node (Storage Units, Junctions, Outfalls, and Dividers) names to include in visualization. Storage nodes are placed at the top of the graphic.
#' @param controlledLinks List of links (Conduits, Pumps, Orifices, Weirs, and Outlets) that were actively controlled in at least one of the scenarios.
#' The graphic presents both flow rate and capacity for all Controlled links.
#' @param passiveLinks List of links (Conduits, Pumps, Orifices, Weirs, and Outlets) that were not controlled in any scenario but should be visualized nonetheless
#' (i.e. to show impact of upstream controls). Passive links are displayed at the bottom of the graphic.
#' @param title The graphic title to use.
#' @param flowUnits The flow units used by the SWMM model underlying these scenario results, either CFS or CMS.
#' @import grid
#' @import gridExtra
#' @import dplyr
#' @export
#' @examples
#' visualize_scenarios(
#'   outputFiles = c("/dev/swmmscenario/inst/theta.out", "/dev/swmmscenario/inst/theta_optimizedsmartretention.out"),
#'   outputScenarioNames = c("Passive Baseline", "Smart Retention"),
#'   storageNodes = c("P1"),
#'   controlledLinks = c("1"),
#'   passiveLinks = c("8"),
#'   title = "Theta Watershed Design Storm",
#'   flowUnits = "CMS")
visualize_scenarios <- function (outputFiles, outputScenarioNames, storageNodes, controlledLinks, passiveLinks, title, flowUnits = "CFS") {

  plots <- list()
  depthUnits <- "ft"
  if (flowUnits == "CMS" || flowUnits == "LPS" || flowUnits == "MLD") {
    depthUnits <- "m"
  }

  # build storage nodes plots (depth)
  for (storageIndex in 1:length(storageNodes)) {
    storageDepths <- data.frame()
    hasDepth = FALSE
    for (fileIndex in 1:length(outputFiles)) {
      ts <- get_swmm_parameter(
        outFile = outputFiles[fileIndex],
        iType = 1,
        objectName = storageNodes[[storageIndex]],
        vIndex = 0,
        resultName = outputScenarioNames[fileIndex])
      if (hasDepth) {
        storageDepths <- inner_join(storageDepths, ts, by="Time")
      }
      else {
        storageDepths <- ts
        hasDepth <- TRUE
      }
    }
    nodePlot <- create_parameter_plot(
      storageDepths,
      paste(storageNodes[[storageIndex]], " Depth [", depthUnits, "]", sep = ""))
    plots[[length(plots)+1]] <- nodePlot
  }

  # build controlled links plots (capacity and flow rate)
  for (clinkIndex in 1:length(controlledLinks)) {
    capacities <- data.frame()
    flows <- data.frame()
    hasData = FALSE
    for (fileIndex in 1:length(outputFiles)) {
      capacity <- get_swmm_parameter(
        outFile = outputFiles[fileIndex],
        iType = 2,
        objectName = controlledLinks[[clinkIndex]],
        vIndex = 4,
        resultName = outputScenarioNames[fileIndex])
      flow <- get_swmm_parameter(
        outFile = outputFiles[fileIndex],
        iType = 2,
        objectName = controlledLinks[[clinkIndex]],
        vIndex = 0,
        resultName = outputScenarioNames[fileIndex])
      if (hasData) {
        capacities <- inner_join(capacities, capacity, by="Time")
        flows <- inner_join(flows, flow, by="Time")
      }
      else {
        capacities <- capacity
        flows <- flow
        hasData <- TRUE
      }
    }

    plots[[length(plots)+1]] <- create_parameter_plot(
      capacities,
      paste(controlledLinks[[clinkIndex]], " Capacity", sep = ""))
    plots[[length(plots)+1]] <- create_parameter_plot(
      flows,
      paste(controlledLinks[[clinkIndex]], " Flow Rate [", flowUnits, "]", sep = ""))
  }

  # build passive links plots (flow rate)
  for (passiveIndex in 1:length(passiveLinks)) {
    flows <- data.frame()
    hasData = FALSE
    for (fileIndex in 1:length(outputFiles)) {
      flow <- get_swmm_parameter(
        outFile = outputFiles[fileIndex],
        iType = 2,
        objectName = passiveLinks[[passiveIndex]],
        vIndex = 0,
        resultName = outputScenarioNames[fileIndex])
      if (hasData) {
        flows <- inner_join(flows, flow, by="Time")
      }
      else {
        flows <- flow
        hasData <- TRUE
      }
    }

    plots[[length(plots)+1]] <- create_parameter_plot(
      flows,
      paste(passiveLinks[[clinkIndex]], " Flow Rate [", flowUnits, "]", sep = ""))
  }

  # display graphic
  grid.arrange(
    grobs = plots,
    top=textGrob(
      label = title,
      gp=gpar(fontsize=20, font=1)),
    ncol = 1)
}
