

# col.groups <- c("A", "A", "B", "B", "C", "C")
# data.frame.to.proc <- named.coverage.bed

PrepareDataFrameForBoxplot <- function(data.frame.to.proc, col.groups, to.stack=FALSE, to.log=FALSE) {
  ## @col.groups: vector of the same dimension of @dataframe.to.proc columns
  ##              each position is a label indicating the column group
  ##              example:
  ##              data.frame.to.proc has 3 columns named siRNA_1, siRNA_2, mRNA
  ##              the col.groups has 3 elements T1, T1, T2
  
  ## check input data
  # print(col.groups)
  if(to.stack) {
    if(to.log) {
      stacked.df <- stack(log(data.frame.to.proc))
    } else {
      stacked.df <- stack(data.frame.to.proc)
    }
    
  } else {
    if(to.log) {
      stacked.df <- data.frame.to.proc
      stacked.df$values <- log(stacked.df$values)
    } else {
      stacked.df <- data.frame.to.proc
    }
  }
  
  new.df <- cbind(stacked.df, rep(NA, times=dim(stacked.df)[1]))
  colnames(new.df) <- c(colnames(stacked.df), "fill")
  
  j=1
  k=dim(data.frame.to.proc)[1]
  
  for(gr in col.groups) {
    new.df[j:k,"fill"] <- gr
    j <- k+1
    k <- k + dim(data.frame.to.proc)[1]
  }
  
  return(new.df)
}

BoxPlot <- function(data.frame.to.plot, is.stacked, title=NULL, x.lbl="ind", y.lbl="values", fill.lbl=NULL, plot=FALSE, plotly=FALSE){#, want.log=FALSE) {
  require("plotly")
  ##check input parameters
  df <- data.frame.to.plot
  if(!is.stacked) {
    # if(want.log){
    #   df <- stack(log(df))
    # }
    df <- stack(df)
    warning("Dataframe stacked! Are you sure that your labels are present?")
  } else {
    # if(want.log){
    #   df$y.lab <- log(df$y.lab)
    # }
  }
  
  ggpl <- ggplot(df, aes_string(x=x.lbl, y=y.lbl, fill=fill.lbl)) + geom_boxplot() + ylab("values") + xlab("conditions") + theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8)) + guides(fill=guide_legend(title=NULL)) + ggtitle(title)
  
  if(plot) {
    if(plotly) {
      ggplotly(ggpl)
    } else {
      print(ggpl)
    }
  } 
  
  return(ggpl)
  
}



ScatterPlot <- function(data.frame.to.plot, log.transform=FALSE, title=NULL, x.lbl=NULL, y.lbl=NULL, plot=FALSE, plotly=FALSE) {
  df <- data.frame.to.plot
  
  if(log.transform) {
    df[,1] <- log((df[,1]+1))
    df[,2] <- log((df[,2]+1))
  }
  
  scatplot <- ggplot(df, aes_string(x=colnames(df)[1], y=colnames(df)[2])) + geom_point(color="darkblue") + geom_abline(intercept=0, slope=1, color="red") + ggtitle(title)
  
  if(plot) {
    if(plotly) {
      ggplotly(scatplot)
    } else {
      print(scatplot)
    }
  } else {
    if(plotly) {
      scatplot <- ggplotly(scatplot)
    }
  }
  return(scatplot)
}


# 
# BarPlot <- function(data.frame.to.plot, is.stacked, title=NULL, x.lbl="ind", y.lbl="values", fill.lbl=NULL, plot=FALSE, plotly=FALSE){#, want.log=FALSE) {
#   require("plotly")
#   ##check input parameters
#   df <- data.frame.to.plot
#   if(!is.stacked) {
#     # if(want.log){
#     #   df <- stack(log(df))
#     # }
#     df <- stack(df)
#     warning("Dataframe stacked! Are you sure that your labels are present?")
#   } else {
#     # if(want.log){
#     #   df$y.lab <- log(df$y.lab)
#     # }
#   }
#   
#   ggpl <- ggplot(df, aes_string(x=x.lbl, y=y.lbl)) + geom_bar() #+ ylab("log(values)") + xlab("conditions") + theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8)) + guides(fill=guide_legend(title=NULL)) + ggtitle(title)
#   
#   if(plot) {
#     if(plotly) {
#       ggplotly(ggpl)
#     } else {
#       print(ggpl)
#     }
#   } 
#   
#   return(ggpl)
#   
# }


ScatterPlotMatrix <- function(data.frame.to.plot) {
  
  # spm(log10(x+1), pch=19,cex=0.3,smoother=FALSE)
  
  # require(GGally)
  # data(tips, package="reshape")
  # 
  # ggpairs(data=tips, # data.frame with variables
  #         columns=1:3, # columns to plot, default to all.
  #         title="tips data", # title of the plot
  #         colour = "sex") # aesthetics, ggplot2 style
}

