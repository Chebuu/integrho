RenderRnaSeqGeneExpressionUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Gene Expression Analysis",
               tabPanel("Gene Counting"),
               tabPanel("Expression Normalization",
                        uiOutput("rnaseqnormalization_ui")),
               tabPanel("Differential Expression Simple Design"),
               tabPanel("Differential Expression Complex Design"),
               tabPanel("Filtering Utility",
                        uiOutput("filteringutility_ui")),
               tabPanel("Bach Effect"),
               # tabPanel("Functional Analysis"
               # ),
               widths = c(2,10)
    )
    
  )
}

RenderRnaSeqIsoformAnalysisUI <- function(input, output, session) {
  fluidPage(
    navlistPanel(title = "Isoform Analysis",
                 tabPanel("Isoform Estimation"),
                 tabPanel("Differential Isoform Expression"),
                 widths = c(2,10)
    )
    
  )
}

RenderRnaSeqNormalizationUI <- function(input, output, session) {
  fluidPage(
    # tabsetPanel(
                 # tabPanel("Upper Quartile", 
                          uiOutput("rnaseqnormalization_ui")#),
                 # tabPanel("TMM")
    # )
    
  )
}

RenderRnaSeqNormalizationUI <- function(input, output, session) {
  fluidPage(
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
               
               tags$div(
                 title="",
                 selectInput(inputId = "normalization_type", label = "Select a normalization:", choices = c("Full Quantile", "Upper Quantile", "TMM"), selected = "Upper Quantile" ) 
               ),
               
               
               tags$br(), 
               actionButton(inputId = "rnaseqnormalize_button", label = "Normalize", width = "100%", icon = icon("fire"))
               
             )
      ),
      column(9,
                plotlyOutput(outputId = "boxplotrnaseq_plotly"),
                tags$br(),
                plotlyOutput(outputId = "boxplotrnaseqnormalized_plotly")
             
      )
    )
  )
}


RenderRnaSeqUnNormalizedBoxplotPlot <- function(input,output,session){
  
  file.path.complete <- file.path(ProjectPath, "count_files", input$count_file)
  col.separator <- "\t"
  coverage.file <- read.table(file=file.path.complete, header = TRUE, sep = col.separator, row.names = 1)
  
  ## group list on columns
  group.path.complete <- file.path(ProjectPath, "group_files", input$group_file)
  groups.file <- read.table(file=group.path.complete, header = FALSE, sep = col.separator)
  
  processed.df <- PrepareDataFrameForBoxplot(data.frame.to.proc = coverage.file, col.groups = as.character(t(groups.file)), to.stack = TRUE, to.log = TRUE)
  
  ggbxp <- BoxPlot(processed.df, is.stacked = TRUE, title="Not Normalized Counts", fill.lbl = "fill")
  require(plotly)
  ggbxp=ggplotly(ggbxp)
  return(ggbxp)
  
}


RenderRnaSeqNormalizationBoxplotPlot <- function(input,output,session){
  
  file.path.complete <- file.path(ProjectPath, "count_files", input$count_file)
  col.separator <- "\t"
  coverage.file <- read.table(file=file.path.complete, header = TRUE, sep = col.separator, row.names = 1)

  ## group list on columns
  group.path.complete <- file.path(ProjectPath, "group_files", input$group_file)
  groups.file <- read.table(file=group.path.complete, header = FALSE, sep = col.separator)
  
  normaliz.type <- input$normalization_type
  
  if(normaliz.type == "Upper Quantile") {
    n.type="uqua"
  } else if(normaliz.type == "Full Quantile") {
    n.type="fqua"
  } else if(normaliz.type == "TMM") {
    n.type="tmm"
  }
  
  normalized.data <- NormalizeData(data.to.normalize = coverage.file, norm.type= n.type)
  
  processed.df <- PrepareDataFrameForBoxplot(data.frame.to.proc = normalized.data, col.groups = as.character(t(groups.file)), to.stack = TRUE, to.log = TRUE)
  
  ggbxp <- BoxPlot(processed.df, is.stacked = TRUE, title="Normalized Counts", fill.lbl = "fill")
  require(plotly)
  ggbxp=ggplotly(ggbxp)
  return(ggbxp)
  
}
