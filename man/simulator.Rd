% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.r
\name{simulator}
\alias{simulator}
\title{simulator}
\usage{
simulator(power_init, lossfunc, n_seconds, max_power, ...)
}
\arguments{
\item{power_init}{dbl Initial power values}

\item{lossfunc}{dbl Loss function}

\item{n_seconds}{int Number of seconds to simulate}

\item{max_power}{dbl Maximum power}

\item{...}{Other arguments to pass to optim, e.g. target_np, target_ap}
}
\value{
optim output
}
\description{
simulator
}
