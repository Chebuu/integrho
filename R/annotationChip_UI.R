RenderAnnotationChIPUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::titlePanel("ChIPpeakAnno Annotation"),

        shiny::fluidRow(
            shiny::column(3,
                    shiny::wellPanel(
                        shiny::tags$div(
                        title="",
                        shiny::selectInput(inputId="peak_file",
                                        label="Select peaks:",
                                        choices=BrowsePath(peaks.path)
                        )
                    ),
                    tags$div(
                        title="",
                        shiny::selectInput(inputId="annotation_file",
                                    label="Select annotation:",
                                    choices=BrowsePath(annotations.path)
                        )
                    ),
                    tags$div(
                        title="",
                        shiny::selectInput(inputId="featuretype_param",
                                    label="Feature type:",
                                    choices=c("TSS", "miRNA", "Exon")
                        )
                    ),

                    tags$div(
                        title="",
                        shiny::selectInput(inputId="output_param",
                                    label="Output distance:",
                                    choices=c("nearestLocation",
                                                "overlapping",
                                                "both",
                                                "shortestDistance",
                                                "inside",
                                                "upstream&inside",
                                                "inside&downstream",
                                                "upstream",
                                                "downstream",
                                                "upstreamORdownstream")
                                    )
                        ),
                    tags$div(
                        title="",
                        shiny::numericInput(inputId="maxgap_param",
                                            label="Maxgap for overlaps:",
                                            value=0,
                                            min=0,
                                            step=1
                        )
                    ),

                    tags$div(
                        title="",
                        shiny::selectInput(inputId="peaklocfordist_param",
                                        label="Peak location for distance:",
                                        choices=c("start", "middle", "end")
                        )
                    ),
                    tags$div(
                        title="",
                        shiny::selectInput(inputId="featurelocfordist_param",
                                    label="Feature location for distance:",
                                    choices=c("TSS",
                                              "middle",
                                              "start",
                                              "end",
                                              "geneEnd"
                                    )
                        )
                    ),
                    tags$div(
                        title="",
                        shiny::selectInput(inputId="select_param",
                                        label="Select:",
                                        choices=c("all",
                                                "first",
                                                "last",
                                                "arbitrary"
                                        )
                        )
                    ),
                    tags$div(
                        title="",
                        shiny::checkboxInput(inputId="ignorestrand_param",
                                            label="Ignore Strand",
                                            value=TRUE
                        )
                    ),

                    tags$div(
                        title="",
                        shiny::checkboxInput(inputId="rr_param",
                                        label="Enable Reproducible Research",
                                        value=TRUE)
                    ),

                    tags$br(),
                    shiny::actionButton(inputId="annotationchipPEA_button",
                                    label="Annotate",
                                    width="100%",
                                    icon=icon("map-signs")
                    )

                    )
            ),
            column(9,
                plotly::plotlyOutput(outputId="chippeakannotation_pie"),
                tags$br(),
                plotly::plotlyOutput(outputId="chippeakannotation_hist")
            )
        )
    )
}

RenderAnnoPeakGeneChIPUI <- function(input, output, session)
{
    shiny::fluidPage(
        shiny::titlePanel("ChIPpeakAnno Annotation"),

        shiny::fluidRow(
            shiny::column(3,
                        shiny::wellPanel(
                            tags$div(
                                title="",
                                shiny::selectInput(inputId="peak_file",
                                                label="Select peaks:",
                                                choices=BrowsePath(peaks.path)
                                )
                            ),
                            tags$div(
                                title="",
                                shiny::selectInput(inputId="annotation_file",
                                            label="Select annotation:",
                                            choices=BrowsePath(annotations.path)
                                )
                              ),
                              tags$div(
                                title="",
                                shiny::selectInput(inputId="featuretype_param",
                                            label="Feature type:",
                                            choices=c("TSS", "miRNA", "Exon")
                                )
                            ),

                            tags$div(
                                title="",
                                shiny::selectInput(inputId="output_param",
                                            label="Output distance:",
                                            choices=c("nearestLocation",
                                                        "overlapping",
                                                        "both",
                                                        "shortestDistance",
                                                        "inside",
                                                        "upstream&inside",
                                                        "inside&downstream",
                                                        "upstream",
                                                        "downstream",
                                                        "upstreamORdownstream")
                                )
                            ),
                            tags$div(
                                title="",
                                shiny::numericInput(inputId="maxgap_param",
                                                label="Maxgap for overlaps:",
                                                value=0,
                                                min=0,
                                                step=1
                                )
                            ),

                            tags$div(
                                title="",
                                shiny::selectInput(inputId="peaklocfordist_param",
                                            label="Peak location for distance:",
                                                choices=c("start", "middle", "end")
                                )
                            ),
                            tags$div(
                                title="",
                                shiny::selectInput(
                                        inputId="featurelocfordist_param",
                                        label="Feature location for distance:",
                                        choices=c("TSS",
                                        "middle",
                                        "start",
                                        "end",
                                        "geneEnd"
                                        )
                                )
                            ),
                            tags$div(
                                title="",
                                shiny::selectInput(inputId="select_param",
                                                label="Select:",
                                                choices=c("all",
                                                            "first",
                                                            "last",
                                                            "arbitrary"
                                                )
                                )
                            ),
                            tags$div(
                                title="",
                                shiny::checkboxInput(inputId="ignorestrand_param",
                                                    label="Ignore Strand",
                                                    value=TRUE
                                )
                            ),

                            tags$div(
                                title="",
                                shiny::checkboxInput(inputId="rr_param",
                                           label="Enable Reproducible Research",
                                           value=TRUE)
                            ),

                            tags$br(),
                            shiny::actionButton(inputId="annotationchipPEA_button",
                                                label="Annotate",
                                                width="100%",
                                                icon=icon("map-signs")
                            )

                        )
            ),
            shiny::column(9,
                shiny::tabsetPanel(
                        shiny::tabPanel("pie"#,
                            # plotly::plotlyOutput(
                            #     outputId="chippeakannotation_pie",
                            #     height="600px"
                            # )
                        ),
                        shiny::tabPanel("hist"#,
                            # plotly::plotlyOutput(
                            #     outputId="chippeakannotation_hist",
                            #     height="600px"
                            # )
                        ),
                        shiny::tabPanel("volcano",
                                        plotly::plotlyOutput(
                                            outputId="chippeakannotation_volc",
                                            height="600px"
                                        )
                        ),
                        id="peakgenepanels", selected=NULL, type="tabs")
                # plotly::plotlyOutput(outputId="chippeakannotation_pie"),
                # tags$br(),
                # plotly::plotlyOutput(outputId="chippeakannotation_hist")
            )
        )
    )
}

RenderAnnotationHistogramCPA <- function(input, output, session)
{
  load(file.path(annotated.peaks.path, "flying_annotated_peaks.RData"))
  s.d <- annotated.peaks@elementMetadata$shortestDistance
  s.d <- s.d[which(s.d < 5000)]
  gghist <- plot_ly(x=s.d, type="histogram")
  return(gghist)
}

RenderAnnotationVolcanoCPA <- function(input, output, session)
{
    res.anno <- readRDS(file.path(annotated.peaks.path, "res.anno.rds"))
    # s.d <- annotated.peaks@elementMetadata$shortestDistance
    # s.d <- s.d[which(s.d < 5000)]
    # gghist <- plot_ly(x=s.d, type="histogram")
    ggp <- PlotVolcanoPlot(de.results=res.anno, design.matrix=NULL,
                        show.plot.flag=FALSE,
                        plotly.flag=TRUE, threshold=0.05)
    return(plotly::ggplotly(ggp))

}

RenderAnnotationPieCPA <- function(input,output,session){

  #if(!input$bedfile_param) {
  if((input$annotation_file=="NO FILEs") || (input$peak_file=="NO FILEs")) {stop("No file(s) selected")}

  anno.file <- input$annotation_file
  peak.file <- input$peak_file

  # if(input$rr_param) {
  #
  #   #MarkdownRCodeChunkEnd()
  # }

  chea.file.name <- paste("chippeakanno", anno.file, peak.file, input$featuretype_param, input$output_param,
                        input$maxgap_param, input$peaklocfordist_param, input$featurelocfordist_param,
                        input$select_param, input$ignorestrand_param, sep="_" )

  print(paste0("reading ", input$annotation_file))
  annotation.gtf <- ReadAnnotationGtf(file.path(annotations.path, anno.file))

    # print(head(annotation.gtf))
  annotation.gr.cpa <- Gtf2GrangeForChipPeakAnno(annotation.gtf)

    # print(head(annotation.gr.cpa))
  #} else {
  #  annotation.gr.cpa <- ReadAnnotationBed(annotation.data)
  #}
  print(paste0("reading ", input$peak_file))
  peaks <- ReadPeakFile(file.path(peaks.path, peak.file))
  # print(head(peaks))



  annotated.peaks <- AnnotationChippeakanno(ProjectPath, peak.list=peaks, annotation.data=annotation.gr.cpa,
                         feature.type=input$featuretype_param, output=input$output_param, max.gap=input$maxgap_param,
                         peak.loc.for.distance=input$peaklocfordist_param, feature.loc.for.distance=input$featurelocfordist_param,
                         select=input$select_param, ignore.strand=input$ignorestrand_param)

  WriteDataFrameToTSV(path.file=file.path(annotated.peaks.path, paste0(chea.file.name, "_PeakAnnotations.tsv")), dataframe=as.data.frame(annotated.peaks))
  save(file=file.path(annotated.peaks.path, "flying_annotated_peaks.RData"), annotated.peaks)


  require(plyr)
  inside.feat <- count(annotated.peaks@elementMetadata$insideFeature)
  #pie1(x=inside.feat$freq, labels=inside.feat$x)
  #ggpie <- plot_ly(inside.feat, labels=x, values=freq, type="pie")
  pie <- plotly::plot_ly(inside.feat, labels=~x, values=~freq, type="pie")
  #return(ggpie)

  saveRDS(pie, file="pie.rds")

  if(input$rr_param) {
    db.file.name <- paste("chippeakanno", anno.file, peak.file, input$featuretype_param, input$output_param,
                          input$maxgap_param, input$peaklocfordist_param, input$featurelocfordist_param,
                          input$select_param, input$ignorestrand_param, sep="_" )
    print(paste0("cache db name: ",db.file.name))

    MarkdownFirstMessageSession("FunctionalAnnotation", "ChipPeakAnno")

    MarkdownRCodeChunkStart(source.files.list=c("CachingFunctions.R"))


    ##save parameteres in rmd
    MarkdownParameterMessage( c(feature.type=input$featuretype_param, output=input$output_param, max.gap=input$maxgap_param, peak.loc.for.distance=input$peaklocfordist_param,
                                feature.loc.for.distance=input$featurelocfordist_param, select=input$select_param, ignore.strand=input$ignorestrand_param))
    MarkdownGeneralMessage("## For completeness here is reported the Executed Code:")
    MarkdownGeneralMessage("# annotation.gtf <- ReadAnnotationGtf(file.path(annotations.path, anno.file))")
    MarkdownGeneralMessage("# annotation.gr.cpa <- Gtf2GrangeForChipPeakAnno(annotation.gtf)")
    MarkdownGeneralMessage("# annotated.peaks <- AnnotationChippeakanno(ProjectPath, peak.list=peaks, annotation.data=annotation.gr.cpa,
#                             feature.type=input$featuretype_param, output=input$output_param, max.gap=input$maxgap_param,
#                             peak.loc.for.distance=input$peaklocfordist_param, feature.loc.for.distance=input$featurelocfordist_param,
#                             select=input$select_param, ignore.strand=input$ignorestrand_param)")



    ## caching section
    cpa.cache.db.obj <- InitCachingDb(db.name=db.file.name, db.path=Cache.Path)
    ## input
    SaveInCache(cpa.cache.db.obj, annotation.gr.cpa, "annotation_data")
    SaveInCache(cpa.cache.db.obj, peaks, "peaks_data")
    ## output
    SaveInCache(cpa.cache.db.obj, annotated.peaks, "annotated_peaks")

    ## markdown section
    MarkdownVariableAssignmentMessage("db.file.name", db.file.name)
    MarkdownVariableAssignmentMessage("Cache.Path", Cache.Path)
    MarkdownGeneralMessage(paste0("cpa.cache.db.obj <- InitCachingDb(db.name=db.file.name, db.path=Cache.Path)"))
    MarkdownLoadCachedObject("cpa.cache.db.obj", "annotation.gr.cpa", "annotation_data")
    MarkdownLoadCachedObject("cpa.cache.db.obj", "peaks", "peaks_data")
    MarkdownRCodeChunkEnd()
  }
  return(pie)
}

