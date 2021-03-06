% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redis.R
\name{redis_del}
\alias{redis_del}
\title{Generate a function to delete a key with specified connection}
\usage{
redis_del(redis_con)
}
\arguments{
\item{redis_con}{an interface to Redis. See \code{\link[redux]{hiredis}}}
}
\description{
Warp DEL see \url{https://redis.io/commands/del}
}
\details{
Use \code{\link[pryr]{unenclose}} to check the generated function
}
