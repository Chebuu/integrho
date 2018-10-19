* In the *FunctionalAnnotation* section you choose the **ChipPeakAnno** section at `2016-10-17 16:35:49`



```r
# source("/home/dario/Dropbox/Lavori/IAC/coding/integrho/demo_src/src/CachingFunctions.R")
# library("filehash")
db.file.name <- "chippeakanno_annotation_gene_mm9_filter_coding.gtf_gold_peaks_TBX11_2388.bed_TSS_nearestLocation_0_start_TSS_all_TRUE"
## Selected parameters values: 
#feature.type<- 'TSS' 
#output<- 'nearestLocation' 
#max.gap<- '0' 
#peak.loc.for.distance<- 'start' 
#feature.loc.for.distance<- 'TSS' 
#select<- 'all' 
#ignore.strand<- 'TRUE' 

## For completeness here is reported the Executed Code:
# annotation.gtf <- ReadAnnotationGtf(file.path(annotations.path, anno.file))
# annotation.gr.cpa <- Gtf2GrangeForChipPeakAnno(annotation.gtf)
# annotated.peaks <- AnnotationChippeakanno(ProjectPath, peak.list = peaks, annotation.data = annotation.gr.cpa,
#                             feature.type = input$featuretype_param, output = input$output_param, max.gap = input$maxgap_param,
#                             peak.loc.for.distance = input$peaklocfordist_param, feature.loc.for.distance = input$featurelocfordist_param,
#                             select=input$select_param, ignore.strand=input$ignorestrand_param)

require("filehash")
source("/home/dario/Dropbox/Lavori/IAC/coding/integrho/demo_src/src/CachingFunctions.R")
annotated.peaks.path <- "/home/dario/Dropbox/Lavori/IAC/coding/integrho/demo_src/IntegrHO/myproject1/results/data/annotated_peaks" 
load(file.path(annotated.peaks.path,"flying_annotated_peaks.RData"))
require(plyr)
inside.feat <- count(annotated.peaks@elementMetadata$insideFeature)
require("plotly")
plot_ly(inside.feat, labels=x, values=freq, type="pie")
```

```
## Error in html_screenshot(x): Please install the webshot package (if not on CRAN, try devtools::install_github("wch/webshot"))
```

```r
# Cache.Path <- "/home/dario/Dropbox/Lavori/IAC/coding/integrho/demo_src/IntegrHO/myproject1/logs/cache"
# cpa.cache.db.obj <- InitCachingDb(db.name = db.file.name, db.path = Cache.Path)
# annotation.gr.cpa <- LoadCachedObject(cpa.cache.db.obj, "annotation_data")

# peaks <- LoadCachedObject(cpa.cache.db.obj, "peaks_data")
```

