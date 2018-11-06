#' runIntegrho
#' @description executes integrho app in default browser
#' @return none
#' @export
#'
#' @examples
#' runIntegrho(host="127.0.0.1", port=3277)
#'
runIntegrho <- function(host="127.0.0.1", port=3277)
{
    library("shiny")
    options(shiny.maxRequestSize=1073741824*8) ##100Gb
    options(shiny.trace=TRUE)

    # setwd("/Users/inzirio/Desktop/gDrive/Dropbox/Lavori/coding/integrho")
    integrho <- shinyApp(ui=IntegrhoUI, server=IntegrhoServer)
    shiny::runApp(integrho, host=host, port=port, launch.browser=TRUE)
}
