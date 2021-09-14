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

#' Stock fundamental data.
#'
#' A dataset containing various stock level characteristics for approximately 700 companies for each month in 2017
#'
#' @format A data frame with 8702 rows and 10 variables:
#' \describe{
#'   \item{date_stamp}{month end dates}
#'   \item{sector}{the industry sector to whcih the stock belongs}
#'   \item{ticker}{the ticker symbol identifying the company}
#'   \item{log_pb}{the log of the book to price ratio}
#'   \item{mkt_cap}{the stocks market capitalisation}
#'   \item{roe}{the stocks return on equity for the trailing 4 quarters}
#'   \item{total_assets}{the stocks total assets at the most recent available reporting date}
#'   \item{total_equity}{the stocks total equity at the most recent available reporting date}
#'   \item{total_equity_cln}{should the stocks have negative total equity, 10% of total assets}
#'   \item{leverage}{total assets divided by total liabilities}
#'   ...
#' }
#' @source \url{https://www.sec.gov/edgar/searchedgar/companysearch.html}
"stock_fundamentals"