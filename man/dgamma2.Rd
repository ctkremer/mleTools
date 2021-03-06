% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_distributions_and_links.R
\name{dgamma2}
\alias{dgamma2}
\alias{rgamma2}
\alias{gamma_shape}
\alias{gamma_scale}
\title{Mean-parameterized gamma distribution}
\usage{
dgamma2(x, mu, s, log = F)

rgamma2(n, mu, s)

gamma_shape(mu, s)

gamma_scale(mu, s)
}
\arguments{
\item{x}{Calculate the probability density at these values}

\item{mu}{The mean of the gamma distribution}

\item{s}{Dispersion parameter}

\item{log}{logical; return the ln of the probability density?}

\item{n}{number of random values to generate (for rgamma2)}
}
\description{
Note: parmeter s is not the variance of the distribution. 
Variance = mu + mu^2/s
}
\section{Functions}{
\itemize{
\item \code{rgamma2}: Generate random values from re-parameterized gamma

\item \code{gamma_shape}: Calculate gamma shape parameter from mu, s

\item \code{gamma_scale}: Calculate gamma scale parameter from mu, s
}}

