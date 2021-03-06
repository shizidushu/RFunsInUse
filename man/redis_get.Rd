% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redis.R
\name{redis_get}
\alias{redis_get}
\title{Generate a function to redis get with specified connection}
\usage{
redis_get(redis_con, serialize = TRUE)
}
\arguments{
\item{redis_con}{an interface to Redis. See \code{\link[redux]{hiredis}}}

\item{serialize}{Whether to serialise R object or not. Defaults to TRUE}
}
\description{
Warp GET see \url{https://redis.io/commands/get}
}
\details{
Use \code{\link[pryr]{unenclose}} to check the generated function
}
\section{Closure Arguments}{

\itemize{
\item key. Redis key. A string.
}
}

