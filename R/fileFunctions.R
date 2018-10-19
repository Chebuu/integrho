## file manager functions

CreatePath <- function(path.to.create, rec=FALSE) {
  if(!file.exists(path.to.create)){
    dir.create(path.to.create, recursive = rec) ##just for test!
    #stop(paste0("path", path.to.create, " already exists"))
  } else {
    #stop(paste0("path", path.to.create, " already exists"))
  }
  #dir.create(path.to.create, recursive = rec)
}

BrowsePath <- function(path.to.browse) {
  ##da spostare in file per gestione file e stringhe
  file.list <- list.files(path.to.browse)
  if(length(file.list)==0) return("NO FILEs")
  return(file.list)
}

ReadAnnotationGtf <- function(gtf.path.file) {
  gtf.data <- as.data.frame(read.table(gtf.path.file, header=FALSE, sep="\t", stringsAsFactors = FALSE))
  if(dim(gtf.data)[2] != 9) {
    stop("GTF file seems to have no standard columns!")
  }
  gtf.data.u <- gtf.data[which(gtf.data[,9] %in% unique(gtf.data[,9])),]
  # gtf.data.u <- subset(gtf.data, !duplicated(gtf.data[,9]))
  # which(duplicated(gtf.data.u))
  return(gtf.data.u)
}

ReadAnnotationBed <- function(bed.path.file) {
  bed.annotation.data <- import(bed.path.file)
  return(bed.annotation.data)
}

ReadPeakFile <- function(peaks.file) {
  peaks <- import(peaks.file, format="BED")
  return(peaks)
}

WriteDataFrameToTSV <- function(path.file, dataframe, quote = FALSE, col.names = TRUE, row.names = TRUE) {
  write.table(file = path.file, x = dataframe, quote = quote, sep = "\t", col.names = NA, row.names = row.names)
  cat("File ", path.file, "written on disk!\n")
}