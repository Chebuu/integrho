
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
#     tabPanel(....

IntegrhoUI <- fluidPage( theme=shinythemes::shinytheme("spacelab"),
    list(tags$head(shiny::HTML('<link rel="icon", href="dna.png",
        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel( title="", windowTitle="IntegrHO")
    ),

    navbarPage(title=div(img(src=system.file("png/integro_logo1.png",
                                            package="integrho"),
                        height=30, width=150)),
                tabPanel("Project",
                        uiOutput("project_ui")
                ),
                navbarMenu("Data Exploration",
                        tabPanel("Graphics",
                            uiOutput("graphics_ui")
                        ),
                        tabPanel("Explore BAM files",
                            uiOutput("bamfileexplore_ui")
                        ),
                        tabPanel("Explore BED files"
                        ),
                        tabPanel("Explore Counts file",
                            uiOutput("countsfileexplore_ui")
                        ),
                        tabPanel("Genomic Plots",
                            uiOutput("genomicplots_ui")
                        ),
                        tabPanel("Data Integration",
                            uiOutput("integrationdata_ui")
                        )
                ),
                navbarMenu("Sequencing",
                    tabPanel("RNA-Seq Gene Expression",
                        uiOutput("rnaseqgenexpression_ui")
                    ),
                    tabPanel("RNA-Seq Isoform Analysis",
                        uiOutput("rnaseqisoform_ui")
                    ),
                    tabPanel("ChIP-Seq",
                        uiOutput("chipseq_ui")
                    ),
                    tabPanel("ATAC-Seq",
                        uiOutput("methylseq_ui")
                    ),
                    tabPanel("Others")
                ),
                navbarMenu("Funct & Ann",
                    tabPanel("Gene Annotation",
                        uiOutput("geneannotation_ui")
                    ),
                    tabPanel("Gene Ontology",
                        uiOutput("geneontology_ui")
                    ),
                    tabPanel("Pathway Analysis",
                        uiOutput("pathway_ui")
                    )
                ),
               navbarMenu("Integrate",
                          tabPanel("Annotate Peaks",
                                   uiOutput("peakgeneannotation_ui")
                          ),
                          tabPanel("MixOmics"),
                                   #uiOutput("mixomics_ui")
                          #),
                          tabPanel("MoFa")#,
                                   #uiOutput("exploresingledataset_ui")
                          #)#,
                          # tabPanel("Explore Pairs of Dataset",
                          #          uiOutput("explorepairsdataset_ui")
                          # ),
                          # tabPanel("Explore Multiple Dataset",
                          #          uiOutput("exploremultipledataset_ui")
                          # )
                          # uiOutput("statistics_ui")
               ),

                # navbarMenu("Statistics",
                #     tabPanel("Test",
                #         uiOutput("tests_ui")
                #     ),
                #     tabPanel("Regression",
                #         uiOutput("regression_ui")
                #     ),
                #     tabPanel("Explore Single Dataset",
                #         uiOutput("exploresingledataset_ui")
                #     ),
                #     tabPanel("Explore Pairs of Dataset",
                #         uiOutput("explorepairsdataset_ui")
                #     ),
                #     tabPanel("Explore Multiple Dataset",
                #         uiOutput("exploremultipledataset_ui")
                #     )
                #            # uiOutput("statistics_ui")
                # ),

                navbarMenu("Utilities",
                    tabPanel("BAM Utility"),
                    tabPanel("BED Utility",
                        uiOutput("bedutilities_ui")
                    ),
                    tabPanel("Conversion Utility"),
                        tabPanel("Table Utility",
                            uiOutput("tabletool_ui")
                        ),
                        tabPanel("Count Utility",
                            uiOutput("counttool_ui")
                        ),
                        tabPanel("Track Utility"),
                           # tabPanel("Filtering Utility",
                           #          uiOutput("filteringutility_ui")),
                        tabPanel("...")
                ),
                navbarMenu("Repr Res",
                    tabPanel("Step Tracking"), ## visualizzazione a grafo degli step performati dall'utente
                    tabPanel("Caching Databases"), ## interfaccia di visualizzazione dei database di caching generati
                    tabPanel("R-Markdown",
                        uiOutput("rmarkdown_ui") ),
                    tabPanel("Generated HTML"),
                    tabPanel("General Settings")
                ),
                navbarMenu("Results",
                    tabPanel("Plots"),
                    tabPanel("Tables")
                )
                # navbarMenu("More",
                #            tabPanel("Help"),
                #            tabPanel("About")
                # )
     )
)


