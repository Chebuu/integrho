Project <- R6Class( "Project",
    private=list (
        name="character"
    ) ,

    public=list(
        ## project name

        ## paths
        results.path="character",
        plots.path="character",
        report.path="character",
        ## others
        working.path="character",
        already.exists="logical",
        filename="character",

        initialize=function(name, main.working.path="./")
        {
            if(missing(name)) stop("Please provide a project name!")
            self$SetName(name)
            self$working.path <- self$UpdateFolderPath(main.working.path, name)
            self$results.path <- self$UpdateFolderPath(self$working.path, "results")
            self$plots.path <- self$UpdateFolderPath(self$results.path, "plots")
            self$report.path <- self$UpdateFolderPath(self$working.path, "report")
        }
        ,

        finalize=function()
        {

        }
        ,

        GeneratePlotStrings=function(path, prefix, plot.type)
        {
            title <- gsub(pattern="_", replacement=" ", x=UpdatePrefix(prefix, plot.type))
            plot.folder <- gsub(pattern=" ", replacement="_", x=file.path(path, plot.type))
            plot.file.name <- gsub(pattern=" ", replacement="_", x=UpdatePrefix(prefix, plot.type))
            dir.create(plot.folder, showWarnings=FALSE, recursive=TRUE)
            return(list("title"= title, "plot.folder"=plot.folder, "plot.file.name"=plot.file.name))
        }
        ,

        UpdatePrefix=function(prefix, ...)
        {
            # new.prefix <- paste(prefix, postix, sep=sep)
            dots <- list(...)
            if(length(dots) != 0)
            {
              for (str in dots) new.prefix <- paste(prefix, str, sep=" " )
            } else {
              stop("provide a string to append to ", new.prefix)
            }
            return(new.prefix)
        }
        ,

        UpdateFolderPath=function(path, ...)
        {
            dots <- list(...)
            if(length(dots) != 0) {
              for (str in dots) path <- file.path(path, str)
            } else {
              stop("provide a string to append to ", path)
            }
            dir.create(path, recursive=TRUE, showWarnings=FALSE)
            return(path)
        }
        ,

        GetProjectPaths=function()
        {
            return(
                "name"=self$name
                , "working.path"=self$working.path
                , "results.path"=self$results.path
                , "plots.path"=self$plots.path
               )
        }
        ,

        UpdateWorkingPath=function(sub.folder.to.generate)
        {
            self$working.path <- self$UpdateFolderPath(self$working.path,
                                                    sub.folder.to.generate)
            return(self$working.path)
        }
        ,
        SetName=function(string)
        {
            private$name <- string
        }
        ,
        GetName=function()
        {
            return(private$name)
        }
    )
)


