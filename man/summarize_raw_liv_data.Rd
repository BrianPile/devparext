% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_raw_liv_data.R
\name{summarize_raw_liv_data}
\alias{summarize_raw_liv_data}
\title{Create parametric data summary from LIV sweeps}
\usage{
summarize_raw_liv_data(
  df_liv,
  If_vec,
  Ix_vec,
  Pop_vec,
  Ik1 = 30,
  Ik2 = 140,
  n1_smooth = 1,
  n2_smooth = 1,
  n3_smooth = 1
)
}
\arguments{
\item{df_liv}{A data frame containing LIV sweep data}

\item{If_vec}{A numeric vector of bias currents}

\item{Ix_vec}{A numeric vector of bias currents relative to threshold}

\item{Pop_vec}{A numeric vector of operating power values}

\item{Ik1}{A numeric value for the start of the kink analysis span (mA)}

\item{Ik2}{A numeric value for the end of the kink analysis span (mA)}

\item{n1_smooth}{P-I smooth span}

\item{n2_smooth}{P-I first-derivative smooth span for Ith calculation}

\item{n3_smooth}{P-I second_derivative smooth span for Ith calculation}
}
\value{
A data frame containing LIV-extracted parametric data
}
\description{
Create parametric data summary from LIV sweeps
}
