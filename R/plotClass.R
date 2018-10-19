require("R6")

require("plotly")



PlotClass <- R6Class( 
  classname = "PlotClass",
  ## private section
  private = list(
    data.to.plot = data.frame(),
    ggpl =  NULL,#ggplot(),
    type = character(),
    aess = aes(),
    
    
    Plot = function() {
      return(ggplot(private$data.to.plot, aes=private$aess))
    },
    Box.Plot = function() {
      return(ggplot(private$data.to.plot, aes=private$aess) + geom_boxplot())
    }
  ),
  ## public section
  public = list(
    initialize = function(data, type, aes.o, plot=FALSE, plotly=FALSE) {
      private$data.to.plot = data
      print(head(private$data.to.plot))
      private$type = type
      private$aess = aes.o
      print(private$aess)
      
      if(private$type == "boxplot") {
        gggpl=private$Box.Plot()
      }
      
      if(plot) {
        if(plotly) {
          self$gg.plotly(gggpl)
        } else {
          self$gg.plot()
        }
      }
    },
    gg.plot = function() {
      print(private$ggpl)
    },
    gg.plotly= function(gggpl){
      ggplotly(gggpl)
    }
  )
                      
  
)