% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pwt.R
\name{get_ggdc_uncached}
\alias{get_ggdc_uncached}
\alias{get_ggdc}
\title{Get data from Grönigen Growth and Development Centre}
\usage{
get_ggdc_uncached(dataset = "pwt", version = "10.01", gwcode = TRUE)

get_ggdc(dataset = "pwt", version = "10.01", gwcode = TRUE)
}
\arguments{
\item{dataset}{A string "pwt" for Penn World Tables and "maddison" for Maddison Project data.}

\item{version}{A string with the version of PWT or Maddison you want to downlaod.}
}
\value{
A data frame with PWT or Maddison data
}
\description{
Get data from Grönigen Growth and Development Centre
}
\section{Functions}{
\itemize{
\item \code{get_ggdc()}: Get data from Grönigen Growth and Development Centre, cached version

}}
\examples{
pwt <- get_penn(dataset = "pwt", version = "10.01")
}
\references{
Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at www.ggdc.net/pwt

Maddison Project Database, version 2020. Bolt, Jutta and Jan Luiten van Zanden (2020), “Maddison style estimates of the evolution of the world economy. A new 2020 update”.
}
