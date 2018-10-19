# path="/home/dario/Dropbox/Lavori/IAC/coding/integrho/demo_src/src/"
#
# path.dropbox="."
#
#
# # path.dropbox="/Users/majin/Desktop/Dropbox"
# source.path <- file.path(path.dropbox, "src/")
#
# source(paste0(source.path, "Project_UI.R"))
#
# source(paste0(source.path, "NormalizationFunctions.R"))
#
# source(paste0(source.path, "RnaSeq_UI.R"))
# source(paste0(source.path, "ChipSeq_UI.R"))
# source(paste0(source.path, "MethylSeq_UI.R"))
# source(paste0(source.path, "FunctionalAnnotation_UI.R"))
# source(paste0(source.path, "AnnotationChip_UI.R"))
# source(paste0(source.path, "AnnotationFunctions.R"))
#
# source(paste0(source.path, "Graphics_UI.R"))
# source(paste0(source.path, "BamFileExploration_UI.R"))
# source(paste0(source.path, "CountsFileExploration_UI.R"))
#
# ##
# source(paste0(source.path, "Boxplot_UI.R"))
# source(paste0(source.path, "Scatterplot_UI.R"))
# source(paste0(source.path, "PlotFunctions.R"))
#
# ## statistics
# source(paste0(source.path, "Statistics_UI.R"))
# source(paste0(source.path, "Clustering_UI.R"))
#
# source(paste0(source.path, "BedUtilities_UI.R"))
# source(paste0(source.path, "CountTool_UI.R"))
# source(paste0(source.path, "TableTool_UI.R"))
# source(paste0(source.path, "FilteringUtility_UI.R"))
# source(paste0(source.path, "FilteringFunctions.R"))
#
# ##rmarkdown
# source(paste0(source.path, "RMarkdown_UI.R"))
#
# ## file functions
# source(paste0(source.path, "FileFunctions.R"))
#
# ## reproducible research
# source(paste0(source.path, "CachingFunctions.R"))
# source(paste0(source.path, "RMarkdownFunctions.R"))

project.name = "myproject1"
working.path.integrho = "./IntegrHO"

ProjectPath <<- file.path(working.path.integrho, project.name)

results.path <<- file.path(ProjectPath, "results")
plots.path <<- file.path(results.path, "plots")
data.path <<- file.path(results.path, "data")
peaks.path <<- file.path(data.path, "peaks")
annotated.peaks.path <<- file.path(data.path, "annotated_peaks")
annotations.path <<- file.path(ProjectPath, "annotations")
config.path <<- file.path(ProjectPath, "config")
Logs.Path <<- file.path(ProjectPath, "logs")
Report.Path <<- file.path(Logs.Path, "report")
Cache.Path <<- file.path(ProjectPath, "logs", "cache")

CreatePath(ProjectPath, rec=TRUE)
CreatePath(results.path)
CreatePath(data.path)
CreatePath(peaks.path)
CreatePath(annotated.peaks.path)
CreatePath(annotations.path)
CreatePath(config.path)
CreatePath(Logs.Path)
CreatePath(Report.Path)
CreatePath(Cache.Path)

Report.Filename <<- InitReportFilename("MyProject_report.rmd")

global.chunk.eval <- TRUE
global.chunk.echo <- TRUE
