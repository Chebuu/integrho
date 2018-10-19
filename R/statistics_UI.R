RenderTestsUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Tests",
                 tabPanel("Hypergeometric Test"),
                 tabPanel("Fisher Test"),
                 tabPanel("Permutation Test"),
                 widths = c(2,10)
    )
    
  )
}

RenderRegressionUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Regression",
                 tabPanel("Linear Model"),
                 tabPanel("Generalized Linear Model"),
                 widths = c(2,10)
    )
    
  )
}

RenderExploreSingleDSUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Explore Single Dataset",
                 tabPanel("Clustering",
                          uiOutput("clustering_ui")
                  ),
                 tabPanel("Correspondence Analysis"),
                 tabPanel("Discriminant Analysis"),
                 tabPanel("Partial Least Squares"),
                 tabPanel("PCA"),
                 widths = c(2,10)
    )
  )
}

RenderExplorePairsDSUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Explore Pairs of Dataset",
                 tabPanel("Sparse Canonical Correlation"),
                 tabPanel("Partial Least Squares"),
                 tabPanel("Co-Inertia"),
                 tabPanel("Epigenomix"),
                 tabPanel("Mancie"),
                 tabPanel("Integromics"),
                 tabPanel("iCluster"),
                 widths = c(2,10)
    )
  )
}

RenderExploreMultipleDSUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Explore Multiple Dataset",
                 tabPanel("MCIA"),
                 tabPanel("Generalized Canonical Correlation"),
                 tabPanel("NSF"),
                 widths = c(2,10)
    )
  )
}



