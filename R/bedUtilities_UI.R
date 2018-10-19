 RenderBedUtilitiesUI <- function(input,output, session){
   fluidPage(
    navlistPanel(title = "BED Utilities",
                tabPanel("Sort BED(s)"),
                tabPanel("Merge BED(s)"),
                tabPanel("Disjoin BED(s)"),
                tabPanel("Count Overlaps"),
                tabPanel("Binary Overlaps"),
                widths=c(2,10)
       
    )

  )
}
 