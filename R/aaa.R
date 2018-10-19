InitReportFilename <- function(file.name=NULL) {
    ## @file.name: optional, if not NULL the global variable Report.Filename will be used
    if(!is.null(file.name)) {
        report.filename <- file.path(Report.Path, file.name)
        if(!file.exists(report.filename)) {
            file.create(report.filename)
        }
    }else{
        stop(paste0("Error: report file name is NULL!"))
    }
    return(report.filename)
}