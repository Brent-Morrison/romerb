#' Time series for testing ts_segmentation function.
#'
#' A dataset containing the CFNAI index and returns to a long momentum strategy
#'
#' @format A data frame with 612 rows and 3 variables:
#' \describe{
#'   \item{date_stamp}{month end dates commencing January 1970}
#'   \item{cndtn_series}{Chicago Fed National Activity Index (CFNAI) time series}
#'   \item{invest_series}{cumulative returns to along only momentum strategy}
#'   ...
#' }
#' @source \url{https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}
"ts_segmentation_test_data"