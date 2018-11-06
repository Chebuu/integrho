IntegrhoServer <- function(input, output, session)
{

    ## project session
    output$project_ui <- renderUI(
    {
        RenderProjectUI(input, output, session)
    })

    output$project_definition_ui <- rhandsontable::renderRHandsontable(
        {
            des.file <- input$design_file
            if(!is.null(des.file))
            {
                return(RenderProjectDefinitionUI(input, output, session))
            }
            #if (input$create_project_button == 0) return()
            #ProjectDefinitionUI(input, output, session)
        })

    observeEvent(input$proj_next_but,{
        # output$project_definition_ui <- renderDataTable(

        filename <- input$design_file
        design.table <- read.table(file=filename$datapath, header=TRUE, sep="\t")
        if(colnames(design.table)[1] == "X")
        {
            colnames(design.table)[1] <- "Samples"
        }
        proj.name  <- input$project_name
        mainProject <- ProjectR6$new(name=proj.name,
                                    main.working.path="IntegrHO")
        mainProject$setDesignTable(design.df=design.table)
    })

    ## data exploration section
    output$integrationdata_ui <- renderUI({
        RenderIntegrationDataUI(input, output, session)
    })

    output$graphics_ui <- renderUI({
        RenderGraphicsUI(input, output, session)
    })

    output$genomicsplot_ui <- renderUI({
        RenderGenomicPlotsUI(input, output, session)
    })

    ##rna-seq section
    output$rnaseqgenexpression_ui <- renderUI({
        RenderRnaSeqGeneExpressionUI(input, output, session)
    })

    output$rnaseqnormalization_ui <- renderUI({
        RenderRnaSeqNormalizationUI(input, output, session)
    })

    # output$rnaseququa_ui <- renderUI({
    #   RenderRnaSeqUquaUI(input, output, session)
    # })

    output$rnaseqnormalization_ui <- renderUI({
        RenderRnaSeqNormalizationUI(input, output, session)
    })

    output$boxplotrnaseq_plotly <- plotly::renderPlotly({
        if (input$rnaseqnormalize_button == 0) return()
        RenderRnaSeqUnNormalizedBoxplotPlot(input, output, session)
    })

    output$boxplotrnaseqnormalized_plotly <- plotly::renderPlotly({
        if (input$rnaseqnormalize_button == 0) return()
        RenderRnaSeqNormalizationBoxplotPlot(input, output, session)
    })

    # output$boxplotrnaseququa_plotly <- renderPlotly({
    #   if (input$rnaseququa_button == 0) return()
    #   RenderRnaSeqUquaBoxplotPlot(input, output, session)
    # })

    output$rnaseqisoform_ui <- renderUI({
        RenderRnaSeqIsoformAnalysisUI(input, output, session)
    })

    output$filteringutility_ui <- renderUI({
        RenderFilteringUtilityUI(input, output, session)
    })

    output$filtered_dataset_table <- renderDataTable({
        if(input$filtering_button == 0) return()
        RenderFilteredDataSet(input, output, session)
    })

    ############################################

    ## chip-seq section
    output$chipseq_ui <- renderUI({
        RenderChipSeqUI(input, output, session)
    })

    ## methyl-seq section
    output$methylseq_ui <- renderUI({
        RenderMethylSeqUI(input, output, session)
    })

    ####################################################################
    ## data exploration section
    output$geneannotation_ui <- renderUI({
        RenderGeneAnnotationUI(input, output, session)
    })

    output$geneontology_ui <- renderUI({
        RenderGeneOntologyUI(input, output, session)
    })

    output$pathway_ui <- renderUI({
        RenderPathwayUI(input, output, session)
    })

    ## data exploration section
    output$tests_ui <- renderUI({
        RenderTestsUI(input, output, session)
    })

    output$regression_ui <- renderUI({
        RenderRegressionUI(input, output, session)
    })

    output$bamfileexplore_ui <- renderUI({
        RenderBamFileExploreUI(input, output, session)
    })
    ######################################
    ## Functional Annotations ##
    output$chippeakanno_ui <- renderUI({
        RenderAnnotationChIPUI(input, output, session)
    })

    shiny::observeEvent(input$annotationchipPEA_button, {
        output$chippeakannotation_pie <- plotly::renderPlotly({
            # if (input$annotationchipPEA_button == 0) return()
            RenderAnnotationPieCPA(input, output, session)
        })

        output$chippeakannotation_hist <- plotly::renderPlotly({
            # if (input$annotationchipPEA_button == 0) return()
            RenderAnnotationHistogramCPA(input, output, session)
        })
    })


    ####################
    ## integrate ##

    output$peakgeneannotation_ui <- renderUI({
        # RenderGeneAnnotationUI(input, output, session)
        # shiny::uiOutput("chippeakanno_ui")
        RenderAnnoPeakGeneChIPUI(input, output, session)
    })
    shiny::observeEvent(input$annotationchipPEA_button, {
        output$chippeakannotation_volc <- plotly::renderPlotly({
            # if (input$annotationchipPEA_button == 0) return()
            RenderAnnotationVolcanoCPA(input, output, session)
        })
    })

    ######################################
    ## statistics ##
    output$exploresingledataset_ui <- renderUI({
        RenderExploreSingleDSUI(input, output, session)
    })
    output$explorepairsdataset_ui <- renderUI({
        RenderExplorePairsDSUI(input, output, session)
    })
    output$exploremultipledataset_ui <- renderUI({
        RenderExploreMultipleDSUI(input, output, session)
    })

    output$countsfileexplore_ui <- renderUI({
        RenderCountsFileExploreUI(input, output, session)
    })
    ## clustering ##
    output$clustering_ui <- renderUI({
        RenderClusteringUI(input, output, session)
    })

    output$hierarchical_ui <- renderUI({
        RenderHierarchicalUI(input, output, session)
    })

    ####################################################################

    ### boxplot section ## to put in another function recalled here
    output$boxplot_ui <- renderUI({
        RenderBoxplotUI(input, output, session)
    })

    output$boxplot_plotly <- renderPlotly({
        if (input$boxplot_button == 0) return()
        RenderBoxplotPlot(input, output, session)
    })
    ####################################################################

    ## UTILITY SECTION ##
    #bed utilities section
    output$bedutilities_ui <- renderUI({
        RenderBedUtilitiesUI(input,output,session)
    })

    ##count file tool section
    output$counttool_ui <- renderUI({
        RenderCountToolUI(input,output,session)
    })

    output$tabletool_ui <- renderUI({
        RenderTableToolUI(input,output,session)
    })

    ### rmarkdown
    output$rmarkdown_ui <- renderUI({
        RenderRMarkdownUI(input, output, session)

    })
    output$rmd_knitted <- renderUI({
        knitRMD(input,output,session)
    })


    ####################################################################


}