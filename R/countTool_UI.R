RenderCountToolUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Count Tool",
                 tabPanel("Read Count"
                 ),
                 tabPanel("Normalize Counts"
                 ),
                 tabPanel("Bind Columns"
                 ),
                 tabPanel("Keep Columns"
                 ),
                 tabPanel("Round Values"
                 ),
                 widths = c(2,10)
    )
    
  )
}