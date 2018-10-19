RenderRMarkdownUI <- function(input, output, server) {
  #require()
  fluidPage(
    titlePanel("R-Markdown"),
    fluidRow(
      tagList(
        with(tags,
              table(
               td(HTML("&nbsp;&nbsp;&nbsp;&nbsp;")),
               td(actionButton("evalRmd", "Knit report"), style= "padding-top:5px; padding-right:2px;"),
               td(downloadButton("saveHTML", "Save HTML"), style= "padding-top:5px;padding-right:2px;"),
               td(downloadButton("saveRmd", "Save Rmd"), style= "padding-top:5px;padding-right:2px;"),
               style="padding-bottom:2px"
                )
        )
      )
    ),
    #div(style="height: '100%'",
    fluidRow(
        column(12, 
         RenderShinyAceEditor(NULL)
        ),
      column(6,
             htmlOutput("rmd_knitted")
      )
    )
    #)
    
  )
}

RenderShinyAceEditor <- function(rmd.file) {
  
    rmd.file="./IntegrHO/myproject1/logs/report/MyProject_report.rmd"
  
  
  markdownfile <- readChar(rmd.file, file.info(rmd.file)$size)
                  
  
  shinyAce::aceEditor("rmd_report", 
                      value = markdownfile,
                      mode = "markdown",
                      vimKeyBinding = TRUE,
                      wordWrap = TRUE,
                      height="750px",
                      fontSize = 14,
                      #height = "100%",
                      # selectionId = "rmd_selection",
                      # value = state_init("rmd_report", init) %>% esc_slash,
                      hotkeys = list(runKeyRmd = list(win = "CTRL-ENTER", mac = "CMD-ENTER"))
                      )
}

# valsRmd <- reactiveValues(knit = 0)
# 
# observe({
#   input$runKeyRmd
#   if (!is.null(input$evalRmd)) isolate(valsRmd$knit %<>% add(1))
# })

scrub <-
  . %>%
  gsub("&lt;!--/html_preserve--&gt;","",.) %>%
  gsub("&lt;!--html_preserve--&gt;","",.) %>%
  gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",.) %>%
  gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",.)  ## knitr adds this

## Knit to save html
knitIt <- function(text) {
  knitr::knit2html(text = text, quiet = TRUE, envir = globalenv(),#r_knitr,
                   options = c("mathjax", "base64_images"),
                   stylesheet = file.path(r_path,"base/www/bootstrap.min.css")) %>%
    scrub %>% HTML
}

knitIt2 <- function(text) {
  ## fragment now also available with rmarkdown
  ## http://rmarkdown.rstudio.com/html_fragment_format.html
  
  md <- knit(text = text, parent.frame())
  paste(markdown::markdownToHTML(text = md, fragment.only = TRUE, stylesheet = ""),
        "<script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
        "<script>if (window.MathJax) MathJax.Hub.Typeset();</script>", sep = '\n') %>% scrub %>% HTML
  
  
}

knitRMD <- function (input, output, session) {
  if (input$evalRmd!=0){
    # req(input$evalRmd || (input$evalRmd >= 0 && !is.null(input$runKeyRmd$randNum)))
    isolate({
      if (input$rmd_report != "") {
        withProgress(message = "Knitting report", value = 0, {
        # knitIt2(input$rmd_report)
          
          input.file="./IntegrHO/myproject1/logs/report/MyProject_report.rmd"
          
          rmarkdown::render(input=input.file)
          
          html.file="./IntegrHO/myproject1/logs/report/MyProject_report.html"
          
           browseURL(html.file)
        })
      }
    })
  }
  
}



