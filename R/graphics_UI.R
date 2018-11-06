RenderGraphicsUI <- function(input, output, session) {
  fluidPage(
    navlistPanel("Graphics",
                 tabPanel("Barplot"),
                 tabPanel("Bi-Histogram"),
                 tabPanel("Bi-Plot"),
                 tabPanel("Boxplot",
                          uiOutput("boxplot_ui")
                 ),
                 tabPanel("Correlation Plot"),
                 tabPanel("Density Plot"),
                 tabPanel("HeatMap"),
                 tabPanel("Histogram"),
                 tabPanel("MA Plot"),
                 tabPanel("PCA"),
                 tabPanel("Scatterplot",
                          uiOutput("scatterplot_ui")
                          ),
                 tabPanel("VENN Diagram"),
                 tabPanel("Volcano Plot"),
                 widths = c(2,10)
    )

  )
}

RenderIntegrationDataUI <- function(input, output, session) {
  fluidPage(
    navlistPanel("Integration Data",
                 tabPanel("Global Enrichment Profiles"),
                 tabPanel("Local Enrichment Profiles"),
                 tabPanel("Heatmap Table"),
                 widths = c(2,10)
    )
  )
}

RenderGenomicPlotsUI <- function(input, output, session) {
  fluidPage(
    navlistPanel("Genomic Plots",
                 tabPanel("Genome Density"),
                 tabPanel("Circles"),
                 widths = c(2,10)
    )

  )
}