#' Connect to biee using simon's username and password
#'
#' @param host host name; Usually the ip address of the host
#' @param port the port number;
#' @param user the user name
#' @param password password
#' @export
connect_biee <- function(host, port, user, password) {
  DBI::dbConnect(
    drv = RJDBC::JDBC(driverClass = "oracle.bi.jdbc.AnaJdbcDriver",
                classPath = system.file("bijdbc.jar", package="fun-in-use")
                ),
    paste0('jdbc:oraclebi://', host, ':', port, '/NQ_SESSION.WEBLANGUAGE=en;'),
    user,
    password
  )
}


#' connect to postgres database
#'
#' @param user the user name
#' @param password password
#' @param dbname the name of the database on the host, or the database file name
#' @param host  host name; defaults to 127.0.0.1
#' @param port the port number; defaults to 5432
#' @export
connect_postgres <- function(user = "postgres",
                             password = "mysecretpassword",
                             dbname = "postgres",
                             host = '127.0.0.1',
                             port = 5432) {
  pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    user = user,
    password = password,
    dbname = dbname,
    host = host,
    port = port
  )
}



#' kill all open connections at once
#' 
#' @description refer to \link{https://stackoverflow.com/questions/32139596/cannot-allocate-a-new-connection-16-connections-already-opened-rmysql}
#'
#' @param drv A object inheriting from DBIDriver. Defaults to RPostgreSQL::PostgreSQL()
#' @export
kill_db_connections <- function (drv = RPostgreSQL::PostgreSQL()) {
  all_cons <- DBI::dbListConnections(drv = drv)

  for(con in all_cons) DBI::dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))
}


#' Connect SQL Server
#'
#' @param server server address; defaults to 127.0.0.1
#' @param port port number
#' @param database the database name. Defaults to Master
#' @param useNTLMv2 a authentication method. Defaults to false
#' @param user user name
#' @param password the password
#' @export
connect_sql_server <- function(server = "127.0.0.1",
                               port=1433,
                               database = "Master",
                               useNTLMv2="false",
                               user,
                               password) {
  DBI::dbConnect(drv = RSQLServer::SQLServer(),
            server=server,
            port=port,
            database = database,
            properties=list(useNTLMv2=useNTLMv2,
                            user=user,
                            password=password)
  )
}
