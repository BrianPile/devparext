% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_peak_wav.R
\name{extract_peak_wav}
\alias{extract_peak_wav}
\title{Extract peak wavelength from optical spectrum data}
\usage{
extract_peak_wav(wav, power)
}
\arguments{
\item{wav}{A numeric vector of spectrum wavelengths}

\item{power}{A numeric vector of spectrum powers}
}
\value{
A numeric value
}
\description{
Extract peak wavelength from optical spectrum data
}
\examples{
wav = seq(1300, 1320, by = 1)
pow = rep(-70, length(wav))
pow[10] = -10
peak_wav = extract_peak_wav(wav, pow)
}
