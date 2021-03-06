% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_one_pro.R
\name{add_one_pro}
\alias{add_one_pro}
\title{Input the data one by one, record the input process and each graph}
\usage{
add_one_pro(data, name, x, y)
}
\arguments{
\item{data}{list}

\item{name}{character, name of the chess}

\item{x}{numeric, x-axis}

\item{y}{numeric, y-axis}
}
\value{
Add one chess to the data and show the graphs
}
\description{
Input the data one by one, record the input process and each graph
}
\examples{
puzzle() \%>\%
  add_one_pro("MA", 5, 1)
}
