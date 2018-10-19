RenderGeneOntologyUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Gene Ontology",
                 tabPanel("DAVID"),
                 tabPanel("Graphite"),
                 tabPanel("GAGE"),
                 widths = c(2,10)
    )
    
  )
}

RenderPathwayUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Pathway Analysis",
                 tabPanel("DAVID"),
                 tabPanel("Graphite"),
                 tabPanel("GAGE"),
                 tabPanel("GSEA"),
                 widths = c(2,10)
    )
    
  )
}

RenderGeneAnnotationUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Gene Annotation",
                 tabPanel("ChIPpeakAnno",
                          uiOutput("chippeakanno_ui")),
                 tabPanel("ChIPseeker"),
                 widths = c(2,10)
    )
    
  )
}
