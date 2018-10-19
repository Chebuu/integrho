# RenderNavListPanelUI <- function (panel.title, elements.list, input, output, session) {
#   navlistPanel(title = panel.title,
#                
#                for(element in elements.list) {
#                  tabPanel(
#                    element$label,
#                    element$elementUI
#                    
#                  )
#                }
#                tabPanel("Sort BED(s)",
#                         #  shinyApp(SortBedUtilityUI, SortBedUtilityServer)
#                         SortBedUtilityUI(input, output, session)
#                         #uiOutput(outputId = "sort_bed_utilities_ui")
#                )
#                
#   )
# }