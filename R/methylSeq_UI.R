RenderMethylSeqUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Methyl-Seq Analysis",
                 tabPanel("Base Methylation Calling"),
                 tabPanel("Region Methylation Calling"),
                 tabPanel("Differential Enriched Regions"),
                 widths = c(2,10)
    )
    
  )
}