% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spe.R
\name{summary.spe}
\alias{summary.spe}
\title{Tabulate the output of \code{\link{spe}} function.}
\usage{
\method{summary}{spe}(object, result = c("sorted", "average"), ...)
}
\arguments{
\item{object}{The output of \code{\link{spe}} function.}

\item{result}{Whether the user wants to see the sorted or the average
estimates. Default is \code{sorted}, which shows the
sorted estimates.}

\item{...}{additional arguments affecting the summary produced.}
}
\description{
The option \code{result} allows user to tabulate either sorted estimates or
average estimates. For sorted estimates, the table shows user-specified
quantile indices, sorted estimates, standard errors, point-wise confidence
intervals, and uniform confidence intervals. For average estimates, the
table shows average estiamtes, standard errors, and confidence intervals.
}
\examples{
data("mortgage")
fm <- deny ~ black + p_irat + hse_inc + ccred + mcred + pubrec + ltv_med +
ltv_high + denpmi + selfemp + single + hischl
test <- spe(fm = fm, data = mortgage, var = "black", method = "logit",
us = c(2:98)/100, b = 50)
summary(test)
}
