#' Generate a function to redis set command with specified connection
#' 
#' Wrap SET and EXPIRE. see \url{https://redis.io/commands/set} and \url{https://redis.io/commands/expire}
#' 
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' 
#' @param redis_con an interface to Redis. See \code{\link[redux]{hiredis}}
#' @param seconds timeout in seconds. \code{\link[lubridate]{dhours}} can be used.
#' @param serialize Whether to serialise R object or not. Defaults to TRUE
#' @export
redis_set <- function(redis_con, seconds, serialize = TRUE){
  function(key, value) {
    r <- redis_con
    if (serialize) {
      value <- redux::object_to_bin(value)
    }
    r$SET(key, value)
    r$EXPIRE(key, seconds)
  }
}

#' Generate a function to redis exists with specified connection
#' 
#' Warp EXISTS. see \url{https://redis.io/commands/exists}
#' 
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' @export
redis_exists <- function(redis_con) {
  r <- redis_con
  function(key) {
    as.logical(r$EXISTS(key))
  }
}


#' Generate a function to redis get with specified connection
#' 
#' Warp GET. see \url{https://redis.io/commands/get}
#' 
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' 
#' @inheritParams redis_set
#' @export
redis_get <- function(redis_con, serialize = TRUE){
  function(key) {
    r <- redis_con
    value <- r$GET(key)
    if (serialize) {
      value <- redux::bin_to_object(value)
    }
    value
  }
}


#' Generate a function to fetch data from cache or get new data with a query if there is no cache
#' 
#' @param exists_f function used to check if cache exists.
#' @param get_f function used to get data from cache
#' @param set_f function used to set new data into cache if data not exists.
#' @export
fetch_data <- function (exists_f, get_f, set_f) {
  function(key, query_f) {
    if (exists_f(key)) {
      value <- get_f(key)
    } else {
      value <- query_f()
      set_f(key, value)
    }
    value
  }
}
