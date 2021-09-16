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
#'   \item{date_stamp}{date_stamp}
#'   \item{sector}{the industry sector to which the stock belongs}
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

#' Stock price data.
#'
#' A dataset containing various stock level characteristics for approximately 700 companies for each month in 2017
#'
#' @format A data frame with 30705 rows and 24 variables:
#' \describe{
#'   \item{symbol}{the ticker symbol identifying the company}
#'   \item{date_stamp}{date_stamp}
#'   \item{close}{the closing price on the last day of the month}
#'   \item{adjusted_close}{the adjusted closing price on the last day of the month}
#'   \item{volume}{trading volume}
#'   \item{rtn_log_1m}{1 month logarithic return}
#'   \item{amihud_1m}{"amihud" illiquidity measure - 1 month average}
#'   \item{amihud_60d}{"amihud" illiquidity measure - 3 month average}
#'   \item{amihud_vol_60d}{volatility of the daily amihud illiquidity measure}
#'   \item{vol_ari_20d}{annualised 1 month volatility of 1 day arithmetic returns}
#'   \item{vol_ari_60d}{annualised 3 month volatility of 1 day arithmetic returns}
#'   \item{vol_ari_120d}{annualised 3 month volatility of 1 day arithmetic returns}
#'   \item{skew_ari_120d}{skewness of 1 day arithmetic returns calculated over 6 months}
#'   \item{kurt_ari_120d}{kurtosis of 1 day arithmetic returns calculated over 6 months}
#'   \item{smax_20d}{average of the five highest daily returns over the trailing month divided by the trailing 20 day daily return volatility}
#'   \item{cor_rtn_1d_mkt_120d}{the correlation of the daily returns between the stock and the S&P500 index over trailing 6 months}
#'   \item{beta_rtn_1d_mkt_120d}{the slope of the regression line between the stocks daily returns and the S&P500 index daily returns over trailing 6 months}
#'   \item{rtn_ari_1m}{1 month arithmetic returns}
#'   \item{rtn_ari_3m}{3 month arithmetic returns}
#'   \item{rtn_ari_6m}{6 month arithmetic returns}
#'   \item{rtn_ari_12m}{12 month arithmetic returns}
#'   \item{sector}{the industry sector to which the stock belongs}
#'   \item{suv}{standardised unexpected volume}
#'   \item{ipc}{intra-portfolio correlation}
#'   ...
#' }
#' @source \url{Alpha Vantage forraw data and "return_attributes.R" for further calculations}
"stock_prices"