RenderTableToolUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Table Tool",
                 tabPanel("Combine Tables"),
                 tabPanel("Subset Table"),
                 widths = c(2,10)
    )
    
  )
}