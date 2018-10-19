source("https://bioconductor.org/biocLite.R")
biocLite("EnsDb.Hsapiens.v75")


library(EnsDb.Hsapiens.v75)
genes(EnsDb.Hsapiens.v75)

library(rtracklayer)
overlaps <- import("../../../data/gold_peaks_TBX11_2388.bed", format="BED")

annoData <- as.data.frame(read.table("../../../script monica/annotation_gene_mm9_filter_coding.gtf", header=F, sep="\t"))

annoData <- as.data.frame(read.table("../../../script monica/annotation_bed_coding[chiPpeakAnno].bed", header=T, sep="\t"))

head(annoData)

gr = GRanges(seqnames = Rle(annoData$chr), IRanges(annoData$start, annoData$end), strand = annoData$strand, name=annoData$gene_id)

ReadAnnotationGtf <- function(gtf.path.file) {
  gtf.data <- as.data.frame(read.table(gtf.path.file, header=FALSE, sep="\t", stringsAsFactors = FALSE))
  if(dim(gtf.data)[2] != 9) {
    stop("GTF file seems to have no standard columns!")
  } 
  return(gtf.data)
}

Gtf2GrangeForChipPeakAnno <- function(gtf.data) {
  annotation.string=strsplit(as.character(gtf.data[,9]),";")
  a <- lapply(X = annotation.string, FUN = function(x) {
    #print(x[1])
    substr(x[1], 9, nchar(x[1]))
  })
  #gene.name=substr(annotation.string, 12,29)
  
  gtf.bed.cpa <- data.frame(gtf.data[,1], gtf.data[,4], gtf.data[,5], gene.name, gtf.data[,7])
  colnames(gtf.bed.cpa) <- c("chr", "start", "end", "gene_id", "strand")
  gtf.gr = GRanges(seqnames = Rle(gtf.bed.cpa$chr), IRanges(gtf.bed.cpa$start, gtf.bed.cpa$end), strand = gtf.bed.cpa$strand, name=gtf.bed.cpa$gene_id)
  return(gtf.gr)
}


annoData <- genes(EnsDb.Hsapiens.v75)
class(annoData)

annotated <- annotatePeakInBatch(overlaps, AnnotationData=annoData, output="overlapping", maxgap=5000L)

annotatedPeak <- annotatePeakInBatch(overlaps, AnnotationData = gtf.gr, output="both", maxgap = 10000, PeakLocForDistance ="middle", FeatureLocForDistance="middle")

hist(annotatedPeak@elementMetadata$shortestDistance, nclass=50, col="red", border="black", main="Distance TBX1 peaks from the 3198 decreased regions", xlab="Distance", ylab="Freq", ylim=c(0,25))
#pie1(annotatedPeak@elementMetadata$shortestDistance, )
require(plyr)
inside.feat <- count(annotatedPeak@elementMetadata$insideFeature)
pie1(x = inside.feat$freq, labels = inside.feat$x)
plot_ly(inside.feat, labels=x, values=freq, type="pie")


require(rtracklayer)

file.path <- "data/Data_Baldini_coverage_2388peaks_extended300bp/cov_NT_SiRNA_1_vs_tbx1_peaks_300bp.bed"
file.path <- "data/Data_Baldini_coverage_2388peaks_extended300bp/coverage_all_vs_2388Tbx1_peaks_extend_300bp.txt"
col.separator <- "\t"



coverage.bed <- read.table(file=file.path, header = FALSE, sep = col.separator)

sample.cols <- c(4:dim(coverage.bed)[2])

colnames(coverage.bed) <- c("chromosome", "start", "end","UNTR_siRNA_1", "UNTR_siRNA_2", "Tbx1_siRNA_1", "Tbx1_siRNA_2", "Tbx1_siRNATCP_1", "Tbx1_siRNATCP_2")

head(coverage.bed)

#coverage.bed[is.na(coverage.bed)]

coverage.bed.r.names <- paste(coverage.bed$chromosome, coverage.bed$start, coverage.bed$end, sep = "_")

named.coverage.bed <- cbind(coverage.bed[,sample.cols])

rownames(named.coverage.bed) <- coverage.bed.r.names
colnames(named.coverage.bed) <- colnames(coverage.bed)[sample.cols]

head(named.coverage.bed)

write.table(named.coverage.bed, file = "~/IntegrHO/myproject/count_files/count_baldini.tsv", col.names = NA, sep = "\t", quote = FALSE)
groups <- c("1", "1", "2", "2", "3", "3")


x <- named.coverage.bed
quantile(named.coverage.bed[,1])

#rainb.cols <- rainbow(length(groups))

# colours <- rainb.cols[1]
# j=1
# 
# for(i in 2:length(groups)) {
#   
#   if(groups[i]!=groups[i-1]) {
#     j <- j+1
#   }
#   
#   colours <- c(colours, rainb.cols[j])
# }

# stacked.coverage.bed <- stack(log(named.coverage.bed))
# 
# 
# conds <- unique(stacked.coverage.bed$ind)
# 
# # fill.col <- stacked.coverage.bed[,"ind", drop=FALSE]
# stacked.coverage.bed.fill <- cbind(stacked.coverage.bed, rep(NA))
# colnames(stacked.coverage.bed.fill) <- c(colnames(stacked.coverage.bed), "fill")
# 
# head(stacked.coverage.bed.fill)
# 
# for(i in 1:length(conds)) {
#   stacked.coverage.bed.fill[which(stacked.coverage.bed.fill$ind %in% conds[i]),"fill"] <- i
# }
# 
# head(stacked.coverage.bed.fill)
# 
# 
# boxplot(log(named.coverage.bed))
# gpl <- ggplot(stacked.coverage.bed.fill, aes(x=ind, y=values), fill=stacked.coverage.bed.fill$fill) + geom_boxplot()
# 
#   print(gpl)
#   ggplotly(gpl)
# 
#  PlotClass$new(stack(log(named.coverage.bed.filtered)), aes.o = aes(x=ind, y=values), type="boxplot", plot=T, plotly=T)

stacked.coverage.bed <- stack(log(named.coverage.bed))


groups <- c("A", "A", "B", "B", "C", "C")

class(groups)
# fill.col <- stacked.coverage.bed[,"ind", drop=FALSE]
stacked.coverage.bed.fill <- cbind(stacked.coverage.bed, rep(NA))
colnames(stacked.coverage.bed.fill) <- c(colnames(stacked.coverage.bed), "fill")

 
head(stacked.coverage.bed.fill)
 
j=1
k=dim(named.coverage.bed)[1]
for(i in 1:length(groups)) {
  
  stacked.coverage.bed.fill[j:k,"fill"] <- groups[i]
  j<-k+1
  k <- k+dim(named.coverage.bed)[1]
}
 
head(stacked.coverage.bed.fill)

tail(stacked.coverage.bed.fill)



df<-stacked.coverage.bed.fill

df <- df[-which(!is.finite(df[,1])),]


x.lbl="ind"
y.lbl="values"
fill.lab="fill"
is.stacked=FALSE
plotly=TRUE



BoxPlot(log(named.coverage.bed), x.lbl = "ind", y.lbl = "values", fill.lbl="ind", is.stacked = FALSE, plot=TRUE, plotly = TRUE)

BoxPlot(stacked.coverage.bed.fill, x.lbl = "ind", y.lbl = "values", fill.lbl="fill", is.stacked = TRUE, plot=TRUE, plotly = TRUE, title="patet" )
