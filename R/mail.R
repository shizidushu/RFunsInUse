#' Send emails
#'
#' @inheritParams mailR::send.mail
#' @param host.name the smtp server
#' @param user.name usually same as the email address
#' @param passwd password of the email
#' @param ssl Whether use ssl or not; defaults to true
#' @param port smtp port; defaults to 465
#' @export
send_mail <- function(from,
                      to,
                      subject = "",
                      body = "",
                      encoding = "utf-8",
                      host.name,
                      port = 465,
                      user.name,
                      passwd,
                      ssl = TRUE,
                      authenticate = TRUE,
                      attach.files = NULL) {
  mailR::send.mail(
    from = from,
    to = to,
    subject = subject,
    body = body,
    encoding = encoding,
    smtp = list(
      host.name = host.name,
      port = port,
      user.name = user.name,
      passwd = passwd,
      ssl = ssl
    ),
    authenticate = authenticate,
    send = TRUE,
    attach.files = attach.files
  )
}
