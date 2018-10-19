FilterLowCountsNoiSeq <- function(data.frame.ns, groups, normalized.flag, method, cov.cutoff, cpm.in){
  require("NOISeq")
  
  if(method=="Counts Per Million"){
    meth=1
  }else if(method=="Wilcoxon Test"){
    meth=2
  }else if(method=="Proportion Test"){
    meth=3
  }
  
  auto.depth = NULL
  for(i in 1:ncol(data.frame.ns)){
    auto.depth[i] = sum(data.frame.ns[,i])
  }
  
  filtered.dataset <- filtered.data(dataset=data.frame.ns, factor=groups, norm=normalized.flag, depth=auto.depth, method=meth, cv.cutoff=cov.cutoff, cpm=cpm.in)
  
  return(filtered.dataset)
}