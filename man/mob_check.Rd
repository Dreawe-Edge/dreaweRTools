% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mob_check_function.R
\name{mob_check}
\alias{mob_check}
\title{Mobile Check}
\usage{
mob_check(
  data,
  var = "CUSTOMER_MOBILE",
  replace = FALSE,
  stopNumbers = c(9999999999, 9999999990, 9999999993)
)
}
\arguments{
\item{data}{dataframe to be operated on}

\item{var}{variable number which contains mobile numbers inside the dataframe}

\item{replace}{whether to delete or replace rows without mobile numbers, default: FALSE}

\item{stopNumbers}{any user defined numbers want to replace or remove from the data}
}
\value{
dataframe with valid mobile numbers
}
\description{
Description: Takes dataframe and a column as input and check for
mobile number validation, following properties are currently being
verified:
1) Length of mobile number must be 10
2) It should start from any number from 6 to 9
3) It should not cantain any alpha characters
}
\examples{
mob_check()
}
