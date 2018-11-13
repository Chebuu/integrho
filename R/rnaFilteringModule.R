renderRnaSeqFilteringUI <- function(id, label="filter rna") {
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
                                shiny::selectInput(inputId=ns("filtering_type"),
                                                    label="Select a filtering:",
                                                    choices=c("Proportion Test",
                                                            "Counts Per Million",
                                                            "Wilcoxon Test"),
                                                    selected="Proportion Test" )
                            ),
                              shiny::selectInput(inputId=ns("group_file"),
                                                 label="Select a file group:",
                                                 choices=BrowsePath(file.path(ProjectPath,"group_files"))),

                            # shiny::conditionalPanel(condition="input.filtering_type == 'CPM'",
                            shiny::tags$div(
                                title="cpm is only for Proportion and CPM tests",
                                shiny::numericInput(inputId=ns("cpm_num"),
                                                label="cpm",
                                                value=1)
                            ),
                            shiny::tags$div(
                                title="cv cutoff is only for cpm test",
                                shiny::numericInput(inputId=ns("cv_num"),
                                                    label="cv cutoff",
                                                    value=100)
                            ),
                            shiny::tags$div(
                                title="are counts normalized?",
                                shiny::checkboxInput(inputId=ns("is_norm"),
                                                    label="is normalized?",
                                                    value=FALSE)
                            ),
                            # ),
                            shiny::tags$br(),
                            shiny::actionButton(inputId=ns("rnaseqfilter_button"),
                                                label="Filter",
                                                width="100%",
                                                icon=shiny::icon("fire"))
                          )
            ),
            shiny::column(9,
                        shiny::htmlOutput(ns("filter_results")),
                        shiny::tags$br(),
                        shiny::dataTableOutput(outputId = ns("filtered_dataset_table"))
                        # plotly::plotlyOutput(ns("boxplotrnaseq_plotly1")),
                        # shiny::tags$br(),
                        # plotly::plotlyOutput(ns("boxplotrnaseqnormalized_plotly1"))

            )
        )
    )
}

renderRnaSeqFiltering <- function(input, output, session)
{




    # return(filtered.df)
    shiny::observeEvent(input$rnaseqfilter_button, {
        file.path.complete <- file.path(ProjectPath, "count_files",
                                        input$count_file)
        # print(file.path.complete)
        col.separator <- "\t"
        coverage.file <- read.table(file=file.path.complete,
                                    header = TRUE,
                                    sep = col.separator,
                                    row.names = 1)
        # print(head(coverage.file))

        ## group list on columns
        # group.path.complete <- file.path(ProjectPath, "group_files", input$group_file)
        # groups.file <- read.table(file=group.path.complete, header = FALSE, sep = col.separator)

        groups.l <- colnames(coverage.file)
        num.genes.before <- dim(coverage.file)[1]
        filtered.df <- FilterLowCountsNoiSeq(data.frame.ns=coverage.file,
                                             groups=groups.l,
                                             normalized.flag=input$is_norm,
                                             method=input$filtering_type,
                                             cov.cutoff=input$cv_num,
                                             cpm.in=input$cpm_num)
        num.genes.after <- dim(filtered.df)[1]

        output$filter_results <- shiny::renderText({
            shiny::HTML(paste(paste0("Number of genes before filtering was: ", num.genes.before),
                paste0("Number of genes after filtering is: ", num.genes.after), sep="<br/>"))
        })
        output$filtered_dataset_table <- shiny::renderDataTable({
            filtered.df
        })

    })


}
