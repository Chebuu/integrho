RenderTestsUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Tests",
                            shiny::tabPanel("Hypergeometric Test"),
                            shiny::tabPanel("Fisher Test"),
                            shiny::tabPanel("Permutation Test"),
                            widths=c(2,10)
        )
    )
}

RenderRegressionUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Regression",
                            shiny::tabPanel("Linear Model"),
                            shiny::tabPanel("Generalized Linear Model"),
                            widths=c(2,10)
        )
    )
}

RenderExploreSingleDSUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Explore Single Dataset",
                            shiny::tabPanel("Clustering",
                                            shiny::uiOutput("clustering_ui")
                            ),
                            shiny::tabPanel("Correspondence Analysis"),
                            shiny::tabPanel("Discriminant Analysis"),
                            shiny::tabPanel("Partial Least Squares"),
                            shiny::tabPanel("PCA"),
                            widths=c(2,10)
        )
    )
}

RenderExplorePairsDSUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Explore Pairs of Dataset",
                            shiny::tabPanel("Sparse Canonical Correlation"),
                            shiny::tabPanel("Partial Least Squares"),
                            shiny::tabPanel("Co-Inertia"),
                            shiny::tabPanel("Epigenomix"),
                            shiny::tabPanel("Mancie"),
                            shiny::tabPanel("Integromics"),
                            shiny::tabPanel("iCluster"),
                            widths=c(2,10)
        )
    )
}

RenderExploreMultipleDSUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Explore Multiple Dataset",
                            shiny::tabPanel("MCIA"),
                            shiny::tabPanel("Generalized Canonical Correlation"),
                            shiny::tabPanel("NSF"),
                            widths=c(2,10)
        )
    )
}



