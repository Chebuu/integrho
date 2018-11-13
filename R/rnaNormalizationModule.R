
renderRnaSeqNormalizationUI <- function(id, label="normalize rna") {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(3,
                          shiny::wellPanel(
                              # sidebarPanel(
                              shiny::tags$div(
                                  title="",
                                  shiny::selectInput(inputId=ns("count_file"),
                                                     label="Select a file count:",
                                                     choices=BrowsePath(
                                                         file.path(ProjectPath,"count_files")))
                              ),
                              shiny::tags$div(
                                  title="",
                                  shiny::selectInput(inputId=ns("group_file"), label="Select a file group:",
                                              choices=BrowsePath(file.path(ProjectPath,"group_files")))
                              ),

                              shiny::tags$div(
                                  title="",
                                  shiny::selectInput(inputId=ns("normalization_type"), label="Select a normalization:",
                                              choices=c("Full Quantile", "Upper Quantile", "TMM"),
                                              selected="Upper Quantile" )
                              ),


                              shiny::tags$br(),
                              shiny::actionButton(inputId=ns("rnaseqnormalize_button"),
                                           label="Normalize", width="100%", icon=shiny::icon("fire"))

                          )
            ),
            shiny::column(9,
                    plotly::plotlyOutput(ns("boxplotrnaseq_plotly1")),
                    shiny::tags$br(),
                    plotly::plotlyOutput(ns("boxplotrnaseqnormalized_plotly1"))

            )
        )
    )
}

renderRnaSeqNormalization <- function(input, output, session)
{

    shiny::observeEvent(input$rnaseqnormalize_button, {
        output$boxplotrnaseq_plotly1 <- plotly::renderPlotly({
            renderRnaSeqUnNormalizedBoxplotPlot(input$count_file,
                                            input$group_file)
        })

        output$boxplotrnaseqnormalized_plotly1 <- plotly::renderPlotly({
            renderRnaSeqNormalizationBoxplotPlot(input$count_file,
                                                input$group_file,
                                                input$normalization_type)
        })
    })

}

renderRnaSeqUnNormalizedBoxplotPlot <- function(count_file, group_file)
{
    # stop(input$count_file)

    file.path.complete <- file.path(ProjectPath, "count_files", count_file)
    col.separator <- "\t"
    coverage.file <- read.table(file=file.path.complete, header=TRUE, sep=col.separator, row.names=1)

    ## group list on columns
    group.path.complete <- file.path(ProjectPath, "group_files", group_file)
    groups.file <- read.table(file=group.path.complete, header=FALSE, sep=col.separator)

    processed.df <- PrepareDataFrameForBoxplot(data.frame.to.proc=coverage.file, col.groups=as.character(t(groups.file)), to.stack=TRUE, to.log=TRUE)

    ggbxp <- BoxPlot(processed.df, is.stacked=TRUE, title="Un-Normalized Counts", fill.lbl="fill")
    require(plotly)
    ggbxp=ggplotly(ggbxp)
    return(ggbxp)

}


renderRnaSeqNormalizationBoxplotPlot <- function(count_file, group_file, norm_type)
{

    file.path.complete <- file.path(ProjectPath, "count_files", count_file)
    col.separator <- "\t"
    coverage.file <- read.table(file=file.path.complete, header=TRUE, sep=col.separator, row.names=1)

    ## group list on columns
    group.path.complete <- file.path(ProjectPath, "group_files", group_file)
    groups.file <- read.table(file=group.path.complete, header=FALSE, sep=col.separator)

    normaliz.type <- norm_type

    if(normaliz.type == "Upper Quantile") {
        n.type="uqua"
    } else if(normaliz.type == "Full Quantile") {
        n.type="fqua"
    } else if(normaliz.type == "TMM") {
        n.type="tmm"
    }

    normalized.data <- NormalizeData(data.to.normalize=coverage.file, norm.type= n.type)

    processed.df <- PrepareDataFrameForBoxplot(data.frame.to.proc=normalized.data, col.groups=as.character(t(groups.file)), to.stack=TRUE, to.log=TRUE)

    ggbxp <- BoxPlot(processed.df, is.stacked=TRUE, title="Normalized Counts", fill.lbl="fill")
    require(plotly)
    ggbxp=ggplotly(ggbxp)
    return(ggbxp)

}