RenderCountsFileExploreUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Counts File Exploration",
                 tabPanel("Bi-Histogram"),
                 tabPanel("Box Plot",
                          uiOutput("boxplot_ui")
                          ),
                 tabPanel("Density Plot"),
                 tabPanel("HeatMap"),
                 tabPanel("MA Plot"),
                 tabPanel("PCA"),
                 tabPanel("Scatter Plot"),
                 widths = c(2,10)
    )
    
  )
}