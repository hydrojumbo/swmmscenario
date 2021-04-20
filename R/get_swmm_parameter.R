#' Get a data frame with a 'Time' column and a specific parameter from a SWMM output file
#' @param outFile Path to the '.out' output file.
#' @param iType Numeric id for the SWMM element type to read. See the swmmr::read_out function.
#' @param objectName Name of the element in the SWMM output file to get data from. See the swmmr::read_out function.
#' @param vIndex Numeric id of the SWMM variable type to read. See the swmmr::read_out function.
#' @param resultName The name to use instead of the SWMM-provided parameter in the returned data frame.
#' This can be helpful if you intend to compare this parameter with those from other sites or scenarios.
#' @import swmmr
#' @import dplyr
#' @import lubridate
#' @import magrittr
#' @import tibble
#' @import rlang
#' @return Data frame of the specified time-series parameter, with 'Time' and resultName-specified columns.
#' @export
get_swmm_parameter <- function (outFile, iType, objectName, vIndex, resultName) {
  data <- data.frame(swmmr::read_out(
    file = outFile,
    iType = iType,
    object_name = objectName,
    vIndex = vIndex
  ))
  data %<>% tibble::rownames_to_column("Time")
  data$Time %<>% lubridate::ymd_hms()
  colnames(data)[2] <- resultName
  # data %<>% dplyr::rename(!!quo_name(resultName) := variableName)

  return(data)
}


