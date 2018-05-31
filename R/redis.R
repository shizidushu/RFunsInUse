#' Generate a function to redis set command with specified connection
#' 
#' Wrap SET and EXPIRE. see \url{https://redis.io/commands/set} and \url{https://redis.io/commands/expire}
#' 
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' 
#' @param redis_con an interface to Redis. See \code{\link[redux]{hiredis}}
#' @param time_to_live time to live in seconds. \code{\link[lubridate]{dhours}} can be used.
#' @param serialize Whether to serialise R object or not. Defaults to TRUE
#' @section Closure Arguments:
#' \itemize{
#'   \item key. Redis key. A string.
#'   \item value. A string or a R object if serialize is set to TRUE
#' }
#' @export
redis_set <- function(redis_con, time_to_live, serialize = TRUE){
  function(key, value) {
    r <- redis_con
    if (serialize) {
      value <- redux::object_to_bin(value)
    }
    r$SET(key, value)
    r$EXPIRE(key, time_to_live)
  }
}

#' Generate a function to redis exists with specified connection
#' 
#' Warp EXISTS. see \url{https://redis.io/commands/exists}
#' 
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' @inheritParams redis_set
#' @section Closure Arguments:
#' \itemize{
#'   \item key. Redis key. A string.
#' }
#' @export
redis_exists <- function(redis_con) {
  function(key) {
    r <- redis_con
    as.logical(r$EXISTS(key))
  }
}


#' Generate a function to redis get with specified connection
#' 
#' Warp GET see \url{https://redis.io/commands/get}
#' 
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' 
#' @inheritParams redis_set
#' @inheritSection redis_exists Closure Arguments
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
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' 
#' @param exists_f function used to check if cache exists.
#' @param get_f function used to get data from cache
#' @param set_f function used to set new data into cache if data not exists.
#' @section Closure Arguments:
#' \itemize{
#'   \item key. Redis key. A string.
#'   \item query_f. function used to get new data. Usually a sql query.
#' }
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



#' Generate a function to return the cache time of a key with specified connection
#' 
#' Warp TTL see \url{https://redis.io/commands/ttl}
#' 
#' Use \code{\link[pryr]{unenclose}} to check the generated function
#' 
#' @inheritParams  redis_set
#' @inheritSection redis_exists Closure Arguments
#' @export
redis_cache_time <- function(redis_con, time_to_live) {
  function(key) {
    r <- redis_con
    remaining_time <- r$TTL(key)
    if (remaining_time == -2) {
      "the key does not exist"
    } else if (remaining_time == -1) {
      "the key exists but has no associated expire"
    } else {
      lubridate::now(tzone = "Asia/Chongqing") - (time_to_live - lubridate::dseconds(remaining_time))
    }
  }
}
