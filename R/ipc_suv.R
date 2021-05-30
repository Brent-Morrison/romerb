#' Standardised unexpected volume
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