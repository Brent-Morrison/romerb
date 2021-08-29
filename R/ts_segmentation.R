#' Time series histogram and shading
#' 
#' Make density plot of subsequent returns conditioned on multiple binary indicators derived from a reference time series 
#' 
#' @param df A dataframe containing the following columns:
#' - date
#' - a times series for assessment (to be referenced by the argument "invest_series")
#' - an indicator time series (to be referenced by the argument "cndtn_series") for plotting and categorisation into bins representing specific level and change values
#' 
#' @param date_idx The column in df representing the date index
#' 
#' @param invest_series A column in df representing the time series for which returns are to be assessed
#' 
#' @param invest_name A string representing the name of the time series for which returns are to be assessed.  
#' If populated, this this will display in the plot title as opposed to the column name. 
#' 
#' @param cndtn_series A column in df representing the conditioning time series to derive the multiple binary indicators
#' 
#' @param cndtn_name A string representing the name of the conditioning time series to derive the multiple binary indicators.  
#' If populated, this this will display in the plot title as opposed to the column name.
#' 
#' @param bin_method either, "level" - split time series into terciles only, or "both"  - split time series into terciles and a 6 month change indicator ("increase" or "decrease")
#' 
#' @param lb The look back period for draw-down assessment
#' 
#' @param pc The percent draw-down for binary market in/out indicator cutoff
#' 
#' @param fr The minimum forward return for binary market in/out indicator cutoff
#' 
#' @export
#' @return A ggplot object.
#' 
ts_segmentation <- function(
  df, 
  date_idx, 
  invest_series, 
  invest_name = NULL,
  cndtn_series, 
  cndtn_name = NULL,
  bin_method, 
  lb = 6, 
  pc = 0.2, 
  fr = -0.05
  ) {
  
  # Programming with ggplot2
  # https://fishandwhistle.net/slides/rstudioconf2020/#1
  
  # use old tidyr::nest
  nest <- tidyr::nest_legacy
  
  # Bind variables locally - https://nathaneastwood.github.io/2019/08/18/no-visible-binding-for-global-variable/
  . <- fwd_rtn_m <- x1.lag6 <- x1.lag12 <- x1_lag00 <- x1.qntl	<- x1.delta <- x1_lag06	<- x1_lag12 <- 
  Indicator <- rowname <- rowIndex	<- Value <- Value_fact <- data <-ks_fit <- Mean <- In <- 
  Out <- mean_diff <- p_val <- start <- end <- min_for_dd <- rtn_for_ind <- drawdown <- flag <-
  y1 <- diff_flag <- colnum <- NULL
  
  di <- rlang::enquo(date_idx)
  is <- rlang::enquo(invest_series)
  x1 <- rlang::enquo(cndtn_series)
  #x1a<- paste0(rlang::quo_name(x1), " : ")
  x1a<- paste0(rlang::as_name(x1), " : ")
  x1b<- rlang::enquo(bin_method)
  x2 <- df %>% 
    dplyr::select(!!di, !!is, !!x1) %>% 
    dplyr::mutate(
      fwd_rtn_m   = dplyr::lead(log(!!is)) - log(!!is),
      rtn_for_ind = log(!!is) - dplyr::lag(log(!!is), lb),
      min_for_dd  = slider::slide_dbl(.x = !!is, .f =  min, .before = lb - 1),
      drawdown    = -dplyr::lag(log(!!is), lb) + log(min_for_dd),
      flag        = ifelse(rtn_for_ind < fr | drawdown < -pc , 1, 0), 
      y1          = dplyr::lead(flag, lb),
      diff_flag   = c(NA, diff(y1))
      ) %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(
      x1.lag6  = dplyr::lag(!!x1, 6),
      x1.lag12 = dplyr::lag(!!x1, 12), 
      
      # tercile level factor
      x1.qntlx = dplyr::ntile(!!x1, 3),
      x1.qntl  = dplyr::case_when(x1.qntlx == 1 ~ "_low", 
                          x1.qntlx == 2 ~ "_mid", 
                          x1.qntlx == 3 ~ "_high"),
           
       # change in level indicator
       x1.rtn6  = !!x1 - x1.lag6,
       x1.rtn12 = !!x1 - x1.lag12,
       
       # binary change in level factor
       x1.delta = dplyr::case_when(
         rlang::as_name(x1b) == "level" ~ "", 
         rlang::as_name(x1b) == "both"  ~ dplyr::if_else(!!x1 > dplyr::lag(!!x1, n = 6), "incr", "decr")
         )
      ) %>%
    
    # factor combining tercile level and binary change in level factors 
    tidyr::unite(x1_lag00, c(x1.qntl, x1.delta),sep = "_", remove = FALSE) %>%
    
    # lagged combined factor and filter out NA's
    dplyr::mutate(
      x1_lag06 = dplyr::lag(x1_lag00, 6),                           
      x1_lag12 = dplyr::lag(x1_lag00, 12)) %>%                           
    dplyr::filter(!is.na(x1.lag12))
  
  # current values of factor values for plot text
  x2.1 <- dplyr::slice(x2, dplyr::n()) %>% dplyr::select(x1_lag00, x1_lag06, x1_lag12) %>% t() %>% 
    data.frame() %>% tibble::rownames_to_column() %>% 
    tidyr::unite(Indicator, c(rowname, .), sep = "", remove = TRUE) %>% 
    dplyr::mutate(Indicator =  gsub("x1_", "", Indicator))
  
  #x2.2 <- quantile(!!x1, probs = 0.33)
  # dummy variables for each (current & lagged) combined level / change factor
  x3 <- stats::predict(caret::dummyVars(" ~ x1_lag00", data = x2), newdata = x2)
  x4 <- stats::predict(caret::dummyVars(" ~ x1_lag06", data = x2), newdata = x2)
  x5 <- stats::predict(caret::dummyVars(" ~ x1_lag12", data = x2), newdata = x2)
  
  # combine dummy variable sets (current and lagged) to single data frame 
  x6 <- tibble::as_tibble(cbind(x3, x4, x5)) %>% dplyr::select(-tidyselect::contains("NA")) %>% 
    tibble::rownames_to_column(var = 'rowIndex') %>% 
    
    # transform combined dummy variable data from wide to long format
    tidyr::gather(key = 'Indicator', value = 'Value', -rowIndex) %>% 
    
    # convert dummy variable to factor
    dplyr::mutate(Value_fact = ifelse(Value == 1, "In", "Out"))
  
  # assign rownames to columns in order to join return data to dummy variable data
  x7 <- x2 %>% dplyr::select(!!di, fwd_rtn_m) %>% tibble::rownames_to_column(var = 'rowIndex')
  
  # data for histogram plot - join return data to dummy variable data 
  x8 <- dplyr::full_join(x6, x7, by  = 'rowIndex') %>% 
    dplyr::mutate(Indicator = stringr::str_replace(Indicator, "x1_", ""))#!!x1a)) ADD CASE WHEN HERE FOR TRAILING UNDERSCORE
  
  # data for kolmorogov smirnov test - list of data frames for
  # each value of each (current & lagged) combined level / change factor
  x8.1<-x8 %>% dplyr::select(Indicator, !!di, Value_fact, fwd_rtn_m) %>% 
    tidyr::spread(Value_fact, fwd_rtn_m) %>% tidyr::nest(-Indicator)
  
  # perform ks test, map to each element of nested dataframe
  x8.2 <- x8.1 %>% dplyr::mutate(
    ks_fit = purrr::map(data, ~ stats::ks.test(.$In, .$Out)),
    p_val  = purrr::map_dbl(ks_fit, "p.value"))
  
  # mean return data & difference in mean for histogram text
  x9 <- x8 %>% dplyr::group_by(Value_fact, Indicator) %>% dplyr::summarise(Mean = mean(fwd_rtn_m))
  x9.1 <- x9 %>% tidyr::spread(Value_fact, Mean) %>% dplyr::mutate(mean_diff = In - Out)
  
  strt <- x2 %>% dplyr::filter(diff_flag == 1) %>% dplyr::select(!!di) %>% dplyr::rename(start = !!di)
  ends <- x2 %>% dplyr::filter(diff_flag == -1) %>% dplyr::select(!!di) %>% dplyr::rename(end = !!di)
  len <- min(dplyr::count(strt), dplyr::count(ends))
  shade <- data.frame(utils::head(strt, len), utils::head(ends, len))
  
  # HISTOGRAM PLOT
  
  colnum <- ifelse(rlang::as_name(x1b) == "both", 6, 3)
  
  x10 <- ggplot2::ggplot(data = x8, ggplot2::aes(x = fwd_rtn_m, colour = Value_fact, fill = Value_fact)) + 
    ggplot2::geom_density(alpha = 0.3) + 
    ggplot2::geom_text(
      data = x9.1, size = 2.5, 
      ggplot2::aes(x = -0.25, 
      y = 12, 
      label = paste0("Difference in\nmean ", 
                     scales::percent(round(dplyr::if_else(is.na(mean_diff), 0, mean_diff), 2)), 
                     sep =" "), 
      colour = NULL, 
      fill = NULL), 
      hjust = 0
      ) +
    ggplot2::geom_text(
      data = x8.2,  size = 2.5,  ggplot2::aes(x = -0.25, y = 8, 
      label = paste0("KS pvalue ", 
                      scales::percent(round(dplyr::if_else(is.na(p_val), 0, p_val), 2)), 
                      sep =" "), 
      colour = NULL, 
      fill = NULL), 
      hjust = 0
      ) +
    ggplot2::geom_vline(
      data = x9, ggplot2::aes(xintercept = Mean, colour = Value_fact),
      linetype = "dashed", size = 0.5) +
    ggplot2::labs(
      title    = paste(ifelse(is.null(invest_name), rlang::as_name(is), invest_name), "subsequent month returns", sep = " "), 
      subtitle = paste("Conditioned on level of ", ifelse(is.null(cndtn_name), rlang::as_name(x1), cndtn_name),  " at various lags\nCurrent values: ", x2.1[1, 1], ", ", x2.1[2, 1], " and ", x2.1[3, 1], sep = ""),
      caption  = " The orange distribution represents subsequent monthly returns during\nperiods when the indicator is in the lag / level / direction specified\nby the facet title.  The blue distribution represent subsequent\nreturns during all other periods.", 
      x        = "", 
      y        = ""
      ) +
    ggplot2::facet_wrap(~ Indicator, ncol = colnum) +  
    ggplot2::theme_grey() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(face = "italic", size = 10),
      plot.caption  = ggplot2::element_text(face = "italic", size = 8),
      axis.title.y  = ggplot2::element_text(face = "italic", size = 9),
      axis.title.x  = ggplot2::element_text(face = "italic", size = 7),
      strip.text    = ggplot2::element_text(size = 8),
      legend.position = "none"
    )
  
  
  # PLOT OF IN/OUT SHADING
  
  x11 <- ggplot2::ggplot(data = df, ggplot2::aes(x = !!di, y = !!is, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_rect(
      data        = shade, 
      inherit.aes = FALSE,
      ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), 
      fill        ='lightblue', alpha=0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      #title    = paste(rlang::as_name(is), "conditioned on", rlang::as_name(x1), sep = " "),
      title    = paste(ifelse(is.null(invest_name), rlang::as_name(is), invest_name), "conditioned on", ifelse(is.null(cndtn_name), rlang::as_name(x1), cndtn_name), sep = " "),
      subtitle = "log scale",
      caption  = "",
      x        = "",
      y        = ifelse(is.null(invest_name), rlang::as_name(is), invest_name)
      ) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle   = ggplot2::element_text(face = "italic", size = 9),
      plot.caption    = ggplot2::element_text(hjust = 0),
      axis.title.y    = ggplot2::element_text(face = "italic", size = 9),
      axis.title.x    = ggplot2::element_text(face = "italic", size = 9))
  
  
  # PLOT OF SELECTED SERIES & IN/OUT SHADING
  
  x12 <- ggplot2::ggplot(data = df, ggplot2::aes(x = !!di, y = !!x1, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_rect(
      data        = shade, 
      inherit.aes = FALSE,
      ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), 
      fill        = 'lightblue', 
      alpha       = 0.5) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +  
    #geom_hline(yintercept = quantile(!!x1, probs = 0.66), color = "black", linetype = "dotted") + 
    #geom_hline(yintercept = quantile(!!x1, probs = 0.66), color = "black", linetype = "dotted") + 
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title            = "",
      subtitle         = "",
      #caption          = "Dashed lines represent upper and lower terciles", 
      x                = "",
      y                = ifelse(is.null(cndtn_name), rlang::as_name(x1), cndtn_name)
      ) + 
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle   = ggplot2::element_text(face = "italic", size = 9),
      plot.caption    = ggplot2::element_text(face = "italic", size = 8),
      axis.title.y    = ggplot2::element_text(face = "italic", size = 9),
      axis.title.x    = ggplot2::element_text(face = "italic", size = 9))
  
  # COMBINE PLOTS
  return(list(cowplot::plot_grid(x11, x12, ncol = 1, align = 'v'), x10))   
}
