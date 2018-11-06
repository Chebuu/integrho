
AnnotationChippeakanno <- function(ProjectPath,
                                   peak.list,
                                   mart.obj=NULL,
                                   annotation.data,
                                   bed.flie.flag=FALSE, ## for bed annotation files
                                   feature.type= c("TSS", "miRNA","Exon"),
                                   output= c("nearestLocation", "overlapping", "both", "shortestDistance", "inside",
                                             "upstream&inside", "inside&downstream","upstream", "downstream", "upstreamORdownstream"),
                                   max.gap=0L,
                                   peak.loc.for.distance=c("start", "middle", "end"),
                                   feature.loc.for.distance=c("TSS", "middle","start", "end","geneEnd"),
                                   select=c("all", "first", "last", "arbitrary"),
                                   ignore.strand=TRUE
                                   )
{
  require(ChIPpeakAnno)
  ## check for parameters

  annotated.peaks <- annotatePeakInBatch(myPeakList=peak.list,
                                     AnnotationData=annotation.data,
                                     # AnnotationData=RangedData(name=gtf.bed.cpa$gene_id, space=gtf.bed.cpa$chr, ranges=IRanges(gtf.bed.cpa$start, gtf.bed.cpa$end), strand=gtf.bed.cpa$strand ),
                                     featureType=feature.type,
                                     output=output,
                                     maxgap=max.gap,
                                     PeakLocForDistance=peak.loc.for.distance,
                                     FeatureLocForDistance=feature.loc.for.distance,
                                     select=select,
                                     ignore.strand=ignore.strand)

  print(head(annotated.peaks))

  return(annotated.peaks)
}

Gtf2GrangeForChipPeakAnno <- function(gtf.data) {
  annotation.string <- strsplit( as.character(gtf.data[,9]), ";")
  gene.name <- unlist(lapply(X=annotation.string, FUN=function(x) { substr(x[1], 9, nchar(x[1])) }))
  gtf.bed.cpa <- data.frame(gtf.data[,1], gtf.data[,4], gtf.data[,5], as.character(gene.name), gtf.data[,7], stringsAsFactors=FALSE)
  colnames(gtf.bed.cpa) <- c("chr", "start", "end", "gene_id", "strand")
  gtf.bed.cpa.u <- subset(gtf.bed.cpa, !duplicated(gtf.bed.cpa$gene_id))
  # head(gtf.bed.cpa)
  ##print("grange")
  gtf.gr=GenomicRanges::GRanges(seqnames=S4Vectors::Rle(gtf.bed.cpa.u$chr), ranges=IRanges::IRanges(start=gtf.bed.cpa.u$start, end=gtf.bed.cpa.u$end, names=gtf.bed.cpa.u$gene_id), strand=gtf.bed.cpa.u$strand, name=as.character(gtf.bed.cpa.u$gene_id))
  #gtf.gr=RangedData(name=as.character(gtf.bed.cpa$gene_id), space=gtf.bed.cpa$chr, ranges=IRanges(start=gtf.bed.cpa$start, end=gtf.bed.cpa$end), strand=gtf.bed.cpa$strand )
  print(head(gtf.gr))
  return(gtf.gr)
}