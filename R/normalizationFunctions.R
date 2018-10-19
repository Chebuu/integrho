NormalizeData <- function(data.to.normalize, norm.type) {
  ## @ norm.type can be uqua, tmm or fqua
  
  
  x <- data.to.normalize
  
  if(norm.type!= "fqua") {
    require("edgeR")
    
    x <- edgeR::DGEList(counts = x)
    
    if(norm.type=="uqua") {
      x <- edgeR::calcNormFactors(x, method='upperquartile')
    } 
    
    if(norm.type=="tmm") {
      x <- edgeR::calcNormFactors(x, method='TMM')
    }
    
    x <- edgeR::estimateCommonDisp(x, verbose=FALSE)
    x <- edgeR::estimateTagwiseDisp(x) 
    normalized.data <- as.data.frame(x$pseudo.counts)
    
  } else if(norm.type=="fqua") {
    require("preprocessCore")
    x <- as.matrix(x)
    normalized.data <- as.data.frame(normalize.quantiles(x, copy=TRUE))
  }

  
  return(normalized.data)
}