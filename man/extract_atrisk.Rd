% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event_time_desp.R
\name{extract_atrisk}
\alias{extract_atrisk}
\title{extract_atrisk}
\usage{
extract_atrisk(fit, time.list, time.scale = 1)
}
\arguments{
\item{fit}{a survfit object}

\item{time.list}{a numeric vector specifying the time points at which the number of at-risk subjects is calculated.}
}
\value{
A dataframe containing the number of at risk patients at time-, overall or by strata
}
\description{
extract_atrisk
}
\details{
The function creates a dataframe containing the number of at risk patients at time-, overall or by strata
}
