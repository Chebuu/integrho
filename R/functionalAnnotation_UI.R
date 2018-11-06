RenderGeneOntologyUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Gene Ontology",
                            shiny::tabPanel("DAVID"),
                            shiny::tabPanel("Graphite"),
                            shiny::tabPanel("GAGE"),
                            widths=c(2,10)
        )
    )
}

RenderPathwayUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Pathway Analysis",
                            shiny::tabPanel("DAVID"),
                            shiny::tabPanel("Graphite"),
                            shiny::tabPanel("GAGE"),
                            shiny::tabPanel("GSEA"),
                            widths=c(2,10)
        )
    )
}

RenderGeneAnnotationUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::navlistPanel("Gene Annotation",
                            shiny::tabPanel("ChIPpeakAnno",
                                            shiny::uiOutput("chippeakanno_ui")),
                            shiny::tabPanel("ChIPseeker"),
                            widths=c(2,10)
        )
    )
}
