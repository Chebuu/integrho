RenderClusteringUI <- function(input, output, session) {
  fluidPage(
    tabsetPanel(
                 tabPanel("Hierarchical",
                          uiOutput("hierarchical_ui")
                  ),
                 tabPanel("K-Means")
                 # tabPanel("Permutation Test")#,
                 #widths = c(2,10)
    )
    
  )
}


RenderHierarchicalUI <- function(input, output, session) {
  fluidPage(
  )
}