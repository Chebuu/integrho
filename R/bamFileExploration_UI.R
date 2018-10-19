RenderBamFileExploreUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Bam Files Exploration",
                 tabPanel("Reads per Sample Histogram"),
                 tabPanel("Reads Mean Quality"),
                 tabPanel("Reads Quality per base"),
                 tabPanel("Nucleotide Frequencies"),
                 tabPanel("Reads per Chromosome"),
                 widths = c(2,10)
    )
    
  )
}


# HTML('<hr style="color: purple;">'),
# 
# tabPanel("BoxPlot"),
# tabPanel("Principal Component Analysis"),
# tabPanel("Scatter Plot"
# ),
# HTML('<hr style="color: purple;">'),
# tabPanel("BiHistogram Plot"
# ),
# tabPanel("Density Plot"
# ),