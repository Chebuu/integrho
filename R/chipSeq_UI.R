RenderChipSeqUI <- function(input, output, session) {
  fluidPage(
    navlistPanel("ChIP-Seq Analysis",
                 # tabPanel("Read Counting"
                 # ),
                 # tabPanel("Count Normalization"
                 # ),
                 tabPanel("Peak Calling"
                 ),
                 tabPanel("Differential Enriched Regions"
                 ),
                 # tabPanel("Gene Annotation"
                 # ),
                 tabPanel("Motif Discovery"
                 ),
                 # tabPanel("Functional Analysis"
                 # ),
                 widths = c(2,10)
    )
    
  )
}