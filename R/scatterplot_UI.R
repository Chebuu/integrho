RenderScatterplotUI <- function(input, output, session) {
  
  fluidPage(
    tabsetPanel(
      tabPanel("Scatterplot",
               uiOutput("singlescatterplot_ui")
      ),
      tabPanel("Scatterplot Matrix",
                uiOutput("scatterplotmatrix_ui")
               )
    )
    
  )
}

RenderSingleScatterplotUI <- function(input, output, session) {
  fluidPage(
    # titlePanel("Boxplot"),

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
                 selectInput(inputId = "col1_file", label = "Select a column to plot:", choices = c(1:10), selected = 1)
               ),
               
               tags$div(
                 title="",
                 selectInput(inputId = "col2_file", label = "Select a column to plot:", choices = c(1:10), selected = 2)
               ),
               
               tags$div(
                 title="",
                 checkboxInput(inputId = "log_flag", label = "Log transform")
               ),
               
               tags$div(
                 title="",
                 checkboxInput(inputId = "plotly_flag", label = "Use plotly")
                ),
      
               tags$br(),
               actionButton(inputId = "scatterplot_button", label = "scatterplot", width = "100%", icon = icon("binoculars"))

             )
      ),
      # mainPanel(
      column(10,
             conditionalPanel(
             condition = "input.plotly_flag",
               plotlyOutput(outputId = "singlescatter_plotly")
             ),
             conditionalPanel(
               condition = "!input.plotly_flag",
               plotOutput(outputId = "singlescatter_plot")
             )
      )
    )
  )
}


RenderSingleScatterplotPlot <- function(input,output,session) {
  
  
  
  file.path.complete <- file.path(ProjectPath, "count_files", input$count_file)
  # print(file.path.complete)
  col.separator <- "\t"
  coverage.file <- read.table(file=file.path.complete, header = TRUE, sep = col.separator, row.names = 1)
  
  # print(head(coverage.file))
  
  col1 <- as.integer(input$col1_file) 
  
  col2 <- as.integer(input$col2_file) 
  
  if ( ( col1> dim(coverage.file)[2]) || (col2 > dim(coverage.file)[2]) ) {
    warning("You selected a too big column number!")
    return()
  }
  
  sub.df <- coverage.file[,c(col1, col2)]
  
  # print(head(sub.df))
  
  self.title = paste0("Scatteplot ", colnames(sub.df)[col1], " vs ", colnames(sub.df)[col2])
  
  ggscp <- ScatterPlot(data.frame.to.plot = sub.df, title=self.title, log.transform = input$log_flag, plotly = input$plotly_flag)
  # if(input$plotly_flag) {
  #   require(plotly)
  #   # ggbxp <- ggplotly(ggbxp)
  #   ggbxp=ggplotly(ggbxp)
  # }
    
  
  return(ggscp)
  
}


RenderScatterplotMatrixUI <- function(input, output, session) {
  fluidPage(
    # titlePanel("Boxplot"),
    
    fluidRow(
      column(2,
             wellPanel(
               # sidebarPanel(
               tags$div(
                 title="",
                 selectInput(inputId = "countm_file", label = "Select a file count:", choices = BrowsePath(file.path(ProjectPath,"count_files")))
               ),
               
               tags$div(
                 title="",
                 selectInput(inputId = "col1m_file", label = "Select starting column to plot:", choices = c(1:10), selected = 1)
               ),
               
               tags$div(
                 title="",
                 selectInput(inputId = "col2m_file", label = "Select final column to plot:", choices = c(1:10), selected = 2)
               ),
               
               tags$div(
                 title="",
                 checkboxInput(inputId = "logm_flag", label = "Log transform")
               ),
               
               # tags$div(
               #   title="",
               #   checkboxInput(inputId = "plotly_flag", label = "Use plotly")
               # ),
               
               tags$br(),
               actionButton(inputId = "scatterplotmatrix_button", label = "scatterplot", width = "100%", icon = icon("bomb"))
               
             )
      ),
      # mainPanel(
      column(10,
               plotOutput(outputId = "matrixscatter_plot")
      )
    )
  )
}

RenderScatterplotMatrixPlot <- function(input,output,session) {
  
  file.path.complete <- file.path(ProjectPath, "count_files", input$countm_file)
  col.separator <- "\t"
  coverage.file <- read.table(file=file.path.complete, header = TRUE, sep = col.separator, row.names = 1)
  
  col1 <- as.integer(input$col1m_file) 
  col2 <- as.integer(input$col2m_file) 
  
  if ( ( col1> dim(coverage.file)[2]) || (col2 > dim(coverage.file)[2]) ) {
    warning("You selected a too big column number!")
    return()
  }
  
  # sub.df <- coverage.file[,c(col1:col2)]
  
  # print(head(sub.df))
  # self.title = paste0("Scatteplot Matrix from ", colnames(sub.df)[col1], " to ", colnames(sub.df)[col2])
  #ggscp <- ScatterPlot(data.frame.to.plot = sub.df, title=self.title, log.transform = input$log_flag, plotly = input$plotly_flag)

  require("GGally")
  scatmat <- ggscatmat(data = coverage.file, columns = c(col1:col2), alpha = 0.5)
  
  return(scatmat)
  
}

