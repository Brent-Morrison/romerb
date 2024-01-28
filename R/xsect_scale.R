#' Cross sectional scale specific columns in a data frame
#' 
#' @param df A data frame containing columns 'date_stamp', 'date_char', 'symbol' and 'fwd_rtn'.
#' @param preds A list or character vector of columns names included in df.
#' @export
#' @return A tibble containing specified input columns and the columns specified with 'preds' on a standardised basis.
#' 
xsect_scale <- function(df, preds) { 
  
  stopifnot("Required columns not present" = min(c("date_stamp", "date_char", "symbol", "fwd_rtn") %in% names(df)) == 1)
  stopifnot("One of selected predictor columns not present" = min(unlist(preds) %in% names(df)) == 1)
  
  df <- df %>% 
    dplyr::group_by(date_stamp) %>% 
    dplyr::mutate(across(.cols = unlist(preds), .fns = ~ as.vector(scale(.x)))) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(date_stamp, date_char, symbol, fwd_rtn, unlist(preds))
  
  return(df)
}