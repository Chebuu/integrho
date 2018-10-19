RenderBoxplotUI <- function(input, output, session) {
  fluidPage(
    titlePanel("Boxplot"),
    
    fluidRow(
      column(2,
            wellPanel(
    # sidebarPanel(
               tags$div(
                 title="",
                 selectInput(inputId = "count_file", label = "Select a file count:", choices = BrowsePath(file.path(ProjectPath,"count_files"))) 
               ),
               tags$div(
                 title="",
                 selectInput(inputId = "group_file", label = "Select a file group:", choices = BrowsePath(file.path(ProjectPath,"group_files"))) 
               ),
               tags$br(), 
               actionButton(inputId = "boxplot_button", label = "boxplot", width = "100%", icon = icon("rocket"))
               
             )
      ),
      # mainPanel(
    column(10,
             plotlyOutput(outputId = "boxplot_plotly")
             # textOutput(outputId = "log_project_creation")
             # uiOutput(outputId = "log_project_creation")
      )
    )
  )
}

# BoxplotServerCallBacks <- function(input, output, session) {
#   
# }

RenderBoxplotPlot <- function(input,output,session){
  
  
  file.path.complete <- file.path(ProjectPath, "count_files", input$count_file)
  # print(file.path.complete)
  col.separator <- "\t"
  coverage.file <- read.table(file=file.path.complete, header = TRUE, sep = col.separator, row.names = 1)
   # print(head(coverage.file))
  
  ## group list on columns
  group.path.complete <- file.path(ProjectPath, "group_files", input$group_file)
  groups.file <- read.table(file=group.path.complete, header = FALSE, sep = col.separator)
  
  # print(groups.file)
  
  processed.df <- PrepareDataFrameForBoxplot(data.frame.to.proc = coverage.file, col.groups = as.character(t(groups.file)), to.stack = TRUE, to.log = TRUE)
  
  # print(head(processed.df))
  # print(tail(processed.df))
  
  ggbxp <- BoxPlot(processed.df, is.stacked = TRUE, title="Unnormalized Counts", fill.lbl = "fill")
  require(plotly)
  # ggbxp <- ggplotly(ggbxp)
  ggbxp=ggplotly(ggbxp)
  return(ggbxp)

}

# BrowsePath <- function(path.to.browse) {
#   ##da spostare in file per gestione file e stringhe
#   file.list <- list.files(path.to.browse)
#   return(file.list)
# }