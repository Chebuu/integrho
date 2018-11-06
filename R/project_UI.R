RenderProjectUI <- function(input, output, session)
{
fluidPage(
    titlePanel("Project Selection"),

    fluidRow(
        column(3,
            wellPanel(
                tags$div(
                    title="The project name will be used during all the analysis you will perform with IntegrHO",
                    textInput(inputId="project_name", label="Enter a project name:")
                ),
                tags$hr(),

                fileInput(inputId="design_file", label="Browse For Design File", multiple=FALSE, width="100%"),
                tags$div(
                    title="Check the box below if you want to bind design matrix columns to their data types",
                    checkboxInput(inputId="rhot_types", label="useTypes", value=FALSE)
                ),

                tags$hr(),
                tags$div(
                    title="Click Next for loading and processing project input information",
                    actionButton(inputId="proj_next_but", label="Start", width="100%")
                )


               # tags$hr(),
               # tags$p(tags$b("Manually Create Design File")),
               # tags$div(
               #   title="The number of samples explain how many file you have to process",
               #   numericInput(inputId="samples_number", label="Number of Samples:", value=3)
               # ),
               # tags$div(
               #   title="The number of groups explain the manner to group your samples",
               #   numericInput(inputId="samples_groups", label="Number of Groups:", value=1)
               # ),
               # # tags$div(
               # #   title=paste0("Choose annotation file"),
               # #   selectInput(inputId="annotation_type", label=NULL, choices=c("Human", "Mouse", "Drosophila"))
               # # ),
               # # tags$br(),
               # tags$hr(),
               # actionButton(inputId="create_project_button", label="create", width="100%", icon=icon("rocket"))



             )
      ),
      column(9,
             #uiOutput(outputId="project_definition_ui")
             # dataTableOutput(outputId="project_definition_ui")
             rhandsontable::rHandsontableOutput(outputId="project_definition_ui")
             # textOutput(outputId="log_project_creation")
             # uiOutput(outputId="log_project_creation")
      )
    )
  )
}


RenderProjectDefinitionUI <- function(input, output, session)
{
    filename <- input$design_file

    design.table <- read.table(file=filename$datapath, header=TRUE, sep="\t")
    if(colnames(design.table)[1] == "X")
    {
        colnames(design.table)[1] <- "Samples"
    }
    return(rhandsontable::rhandsontable(design.table, useTypes=input$rhot_types))
}


RenderExperimentDesignRow <- function(input, output, session, row.number)
{

  fluidRow(
    column(3,
      tags$div(
        title=paste0("Insert the path of file"),
        textInput(inputId=paste0("file_path_", row.number),
                    label=NULL, width="100%")
      )
    ),

    column(1,
      # tags$div(
        # title=paste0("Choose the file type"),
        selectInput(inputId=paste0("file_type_", row.number),
                    label=NULL, choices=c("bam", "bed"))
       ),
    column(1,
      # tags$div(
        # title=paste0("Choose the experiment type"),
        selectInput(inputId=paste0("experiment_type_", row.number),
                    label=NULL, choices=c("RNA-Seq", "ChIP-Seq"))
       ),

    column(1,
      # tags$div(
        # title=paste0("Choose the experiment group"),
        selectInput(inputId=paste0("experiment_group_", row.number),
                    label=NULL, choices=c(1:input$samples_groups))
       ),
    column(1,
           # tags$div(
           # title=paste0("Check if the experiment is paired end"),
           checkboxInput(inputId=paste0("paired_", row.number),
                        label=NULL, value=TRUE)
    ),
    column(1,
      # tags$div(
        # title=paste0("Check if the sample is Treatment or Control"),
        checkboxInput(inputId=paste0("treated_", row.number),
                    label=NULL, value=FALSE)
      #)
    )
  )

}


ProjectDefinitionUI <- function(input, output, session)
{
    table <- NULL
    # table <- c("File Path")
    # if on loaded experimental design file
    for(i in 1:input$samples_number)
    {
        table <- c(table,
                RenderExperimentDesignRow(input, output, session, row.number=i))
        # wellPanel(
        # # CreateProjectTree(input, output, session)
        # # RenderExperimentDesignRow(input, output, session, row.number=1)
        # )
    }
    tab <- do.call(tagList, table)
    return(tab)
}

# CreateProjectTree <- function(input, output, session) {
#   project.name="myproject"
#   path <- file.path("~/IntegrHO", project.name, "count_files")
#   dir.create(path, recursive=TRUE)
# }
