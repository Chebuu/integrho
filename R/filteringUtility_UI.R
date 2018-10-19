RenderFilteringUtilityUI <- function(input, output, session) {
  fluidPage(
    title = "Filtering Utility",
    fluidRow(
      column(2,
             wellPanel(
               tags$div(
                 title="",
                 selectInput(inputId = "count_file", label = "Select a file count:", choices = BrowsePath(file.path(ProjectPath,"count_files"))) 
               ),
               tags$div(
                 title="",
                 checkboxInput(inputId = "normalized_file", label = "Normalized counts?", value = FALSE)
               ),
               
               # tags$div(
               #   title="",
               #   selectInput(inputId = "group_file", label = "Select a file group:", choices = BrowsePath(file.path(ProjectPath,"group_files")))
               # ),
               
               tags$div(
                 title="",
                 selectInput(inputId = "filtering_method", label = "Select a method", choices = c("Counts Per Million", "Wilcoxon Test", "Proportion Test"), selected = "Proportion Test")
               ),
               
               tags$div(
                 title="",
                 numericInput(inputId = "cpm_cutoff", label = "Choose a Cutoff for CPM", value = 0.5, step = 0.25, min = 0, max=1)
               ),
               
               # uiOutput("cpmfiltering_ui"),
               tags$div(
                 title="",
                 numericInput(inputId = "cov_cutoff", label = "Choose a Threshold Cutoff", value = 100, step = 25, min = 0)
               ),
               
               
               tags$br(), 
               actionButton(inputId = "filtering_button", label = "Filter", width = "100%", icon = icon("magic"))
               
             )
      ),
      # mainPanel(
      column(10,
             dataTableOutput(outputId = "filtered_dataset_table")
             # plotlyOutput(outputId = "boxplot_plotly")
             # textOutput(outputId = "log_project_creation")
             # uiOutput(outputId = "log_project_creation")
      )
    )
  )
}

RenderFilteredDataSet <- function(input, output, session) {
  file.path.complete <- file.path(ProjectPath, "count_files", input$count_file)
  # print(file.path.complete)
  col.separator <- "\t"
  coverage.file <- read.table(file=file.path.complete, header = TRUE, sep = col.separator, row.names = 1)
  # print(head(coverage.file))
  
  ## group list on columns
  # group.path.complete <- file.path(ProjectPath, "group_files", input$group_file)
  # groups.file <- read.table(file=group.path.complete, header = FALSE, sep = col.separator)
  
  groups.l <- colnames(coverage.file)
  
  filtered.df <- FilterLowCountsNoiSeq(data.frame= coverage.file, groups=groups.l, normalized.flag=input$normalized_file, method=input$filtering_method, cov.cutoff=input$cov_cutoff, cpm=input$cpm_cutoff)
  
  return(filtered.df)
}

# RenderCpmFilteringComponent <- renderUI({
#   
#   
# })