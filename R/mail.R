#' Send emails
#'
#' @inheritParams mailR::send.mail
#' @param host.name the smtp server
#' @param user.name usually same as the email address
#' @param passwd password of the email
#' @param ssl Whether use ssl or not; defaults to true
#' @param port smtp port; defaults to 465
send_mail <- function(from = "simon@inc.tvc-tech.com",
                      to,
                      subject = "",
                      body = "",
                      host.name = "smtp.exmail.qq.com",
                      port = 465,
                      user.name,
                      passwd,
                      ssl = TRUE,
                      authenticate = TRUE) {
  mailR::send.mail(
    from = from,
    to = to,
    subject = subject,
    body = body,
    smtp = list(
      host.name = host.name,
      port = port,
      user.name = user.name,
      passwd = passwd,
      ssl = ssl
    ),
    authenticate = authenticate,
    send = TRUE
  )
}
