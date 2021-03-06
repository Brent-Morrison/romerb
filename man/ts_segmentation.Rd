% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_segmentation.R
\name{ts_segmentation}
\alias{ts_segmentation}
\title{Time series histogram and shading}
\usage{
ts_segmentation(
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
)
}
\arguments{
\item{df}{A dataframe containing the following columns:
\itemize{
\item date
\item a times series for assessment (to be referenced by the argument "invest_series")
\item an indicator time series (to be referenced by the argument "cndtn_series") for plotting and categorisation into bins representing specific level and change values
}}

\item{date_idx}{The column in df representing the date index}

\item{invest_series}{A column in df representing the time series for which returns are to be assessed}

\item{invest_name}{A string representing the name of the time series for which returns are to be assessed.
If populated, this this will display in the plot title as opposed to the column name.}

\item{cndtn_series}{A column in df representing the conditioning time series to derive the multiple binary indicators}

\item{cndtn_name}{A string representing the name of the conditioning time series to derive the multiple binary indicators.
If populated, this this will display in the plot title as opposed to the column name.}

\item{bin_method}{either, "level" - split time series into terciles only, or "both"  - split time series into terciles and a 6 month change indicator ("increase" or "decrease")}

\item{lb}{The look back period for draw-down assessment}

\item{pc}{The percent draw-down for binary market in/out indicator cutoff}

\item{fr}{The minimum forward return for binary market in/out indicator cutoff}
}
\value{
A ggplot object.
}
\description{
Make density plot of subsequent returns conditioned on multiple binary indicators derived from a reference time series
}
