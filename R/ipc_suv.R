#' Standardised unexpected volume
#' 
#' Implementation as per:  
#' Garfinkel, Jon A. and Sokobin, Jonathan, Volume, Opinion Divergence and Returns: A Study of Post-Earnings Announcement Drift (November 2003). AFA 2004 San Diego Meetings, Available at SSRN: https://ssrn.com/abstract=280913 or http://dx.doi.org/10.2139/ssrn.280913
#' 
#' @param df A data frame containing columns 'date_stamp', 'pos', 'neg'
#'
#' @return A tibble containing a date stamp and standardised unexpected volume
#' @export
#'
#' @examples
suv <- function(df) { 
  max_date = max(df$date_stamp)
  mdl <- summary(stats::lm(volume ~ pos + neg, data = df))
  res <- mdl$residuals / mdl$sigma
  suv <- mean(res[(max(length(res),21)-21):length(res)])
  return(tibble::tibble(date_stamp = max_date, suv = suv))
}