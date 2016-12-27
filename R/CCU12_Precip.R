#' Precipitation for June-September 2012 recorded in Kolkata
#'
#'
#' @docType data
#'
#' @usage data(CCU12_Precip)
#'
#' @format A data frame with columns
#' \describe{
#'
#' \item{Date}{The data in Year-Month-Day format}
#' \item{Precip}{Precipitation in millimeters}
#' \item{TMax}{Maximum temperature, in Celcius}
#' \item{TMin}{Minimum temperature, in Celcius}
#'}
#' @keywords datasets
#'
#' @examples
#' Precip <-CCU12_Precip$Precip
#' TMax <-CCU12_Precip$TMax
#' plot(TMax, Precip)
"CCU12_Precip"
