
# ProjectInUse <<- Project(NULL)


# fluidPage(
#   list(tags$head(HTML('<link rel="icon", href="MyIcon.png",
#                                    type="image/png" />'))),
#   div(style="padding: 1px 0px; width: '100%'",
#       titlePanel(
#         title="", windowTitle="My Window Title"
#       )
#   ),
#   navbarPage(
#     title=div(img(src="myLogo.gif"), "My Title in the Navbar"),
#     shiny::tabPanel(....

IntegrhoUI <- shiny::fluidPage( theme=shinythemes::shinytheme("spacelab"),
    list(tags$head(shiny::HTML('<link rel="icon", href="dna.png",
        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel( title="", windowTitle="IntegrHO")
    ),

    shiny::navbarPage(title=div(img(src=system.file("png/integro_logo1.png",
                                            package="integrho"),
                        height=30, width=150)),
                shiny::tabPanel("Project",
                        shiny::uiOutput("project_ui")
                ),
                shiny::navbarMenu("Data Exploration",
                        shiny::tabPanel("Graphics",
                            shiny::uiOutput("graphics_ui")
                        ),
                        shiny::tabPanel("Explore BAM files",
                            shiny::uiOutput("bamfileexplore_ui")
                        ),
                        shiny::tabPanel("Explore BED files"
                        ),
                        shiny::tabPanel("Explore Counts file",
                            shiny::uiOutput("countsfileexplore_ui")
                        ),
                        shiny::tabPanel("Genomic Plots",
                            shiny::uiOutput("genomicplots_ui")
                        )
                ),
                shiny::navbarMenu("RNA-Seq",
                        shiny::tabPanel("Quantify Counts",
                                shiny::uiOutput("rnaCounts_ui")
                        ),
                        shiny::tabPanel("Filter Counts",
                                        # shiny::uiOutput("rnaFiltCounts_ui")
                                renderRnaSeqFilteringUI("rnaFiltCounts_ui")
                        ),
                        shiny::tabPanel("Normalize Counts",
                                        # shiny::uiOutput("rnaNormCounts_ui")
                                renderRnaSeqNormalizationUI("rnaNormCounts_ui")
                        )
                    ,
                    shiny::tabPanel("RNA-Seq Gene Expression",
                        shiny::uiOutput("rnaseqgenexpression_ui")
                    )
                    #,
                    # shiny::tabPanel("RNA-Seq Isoform Analysis",
                    #     shiny::uiOutput("rnaseqisoform_ui")
                    # ),
                    # shiny::tabPanel("ChIP-Seq",
                    #     shiny::uiOutput("chipseq_ui")
                    # ),
                    # shiny::tabPanel("ATAC-Seq",
                    #     shiny::uiOutput("methylseq_ui")
                    # ),
                    # shiny::tabPanel("Others")
                ),
                shiny::navbarMenu("ChIP-Seq"

                ),
                shiny::navbarMenu("ATAC-Seq"

                ),
                shiny::navbarMenu("Funct & Ann",
                    shiny::tabPanel("Gene Annotation",
                        shiny::uiOutput("geneannotation_ui")
                    ),
                    shiny::tabPanel("Gene Ontology",
                        shiny::uiOutput("geneontology_ui")
                    ),
                    shiny::tabPanel("Pathway Analysis",
                        shiny::uiOutput("pathway_ui")
                    )
                ),
                shiny::navbarMenu("Integration",
                        shiny::tabPanel("Functional"#,

                        ),
                        shiny::tabPanel("Annotate Peaks",
                                shiny::uiOutput("peakgeneannotation_ui")
                        ),
                        shiny::tabPanel("MixOmics"),
                        shiny::tabPanel("MoFa")
                ),
                shiny::navbarMenu("Utilities",
                    shiny::tabPanel("BAM Utility"),
                    shiny::tabPanel("BED Utility",
                        shiny::uiOutput("bedutilities_ui")
                    ),
                    shiny::tabPanel("Conversion Utility"),
                        shiny::tabPanel("Table Utility",
                            shiny::uiOutput("tabletool_ui")
                        ),
                        shiny::tabPanel("Count Utility",
                            shiny::uiOutput("counttool_ui")
                        ),
                        shiny::tabPanel("Track Utility"),
                           # shiny::tabPanel("Filtering Utility",
                           #          shiny::uiOutput("filteringutility_ui")),
                        shiny::tabPanel("...")
                ),
                shiny::navbarMenu("Repr Res",
                    shiny::tabPanel("Step Tracking"), ## visualizzazione a grafo degli step performati dall'utente
                    shiny::tabPanel("Caching Databases"), ## interfaccia di visualizzazione dei database di caching generati
                    shiny::tabPanel("R-Markdown",
                        shiny::uiOutput("rmarkdown_ui") ),
                    shiny::tabPanel("Generated HTML"),
                    shiny::tabPanel("General Settings")
                ),
                shiny::navbarMenu("Results",
                    shiny::tabPanel("Plots"),
                    shiny::tabPanel("Tables")
                )
                # shiny::navbarMenu("More",
                #            shiny::tabPanel("Help"),
                #            shiny::tabPanel("About")
                # )
     )
)


