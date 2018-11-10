ProjectR6 <- R6Class( "ProjectR6",
    private=list (
        name="character",
        ## paths
        design.path="character",
        results.path="character",
        plots.path="character",
        report.path="character",

        working.path="character",
        already.exists="logical",
        filename="character",
        ## data str
        design.table="data.frame"
    ) ,

    public=list(
        ## project name


        ## others


        initialize=function(name, main.working.path="./")
        {
            if(missing(name)) stop("Please provide a project name!")
            self$SetName(name)
            private$working.path <- self$UpdateFolderPath(main.working.path,
                                                            name)
            private$results.path <- self$UpdateFolderPath(private$working.path,
                                                            "results")
            private$plots.path <- self$UpdateFolderPath(private$results.path,
                                                            "plots")
            private$report.path <- self$UpdateFolderPath(private$working.path,
                                                            "report")
            private$design.path <- self$UpdateFolderPath(private$working.path,
                                                            "design")
        }
        ,

        finalize=function()
        {

        }
        ,

        setDesignTable=function(design.df)
        {
            private$design.table <- design.df
            write.table(x=design.df,
                        file=file.path(private$design.path, "design.tsv"),
                        quote=FALSE, sep="\t", col.names=NA)
        }
        ,

        GeneratePlotStrings=function(path, prefix, plot.type)
        {
            title <- gsub(pattern="_",
                                replacement=" ",
                                x=UpdatePrefix(prefix, plot.type))
            plot.folder <- gsub(pattern=" ",
                                replacement="_",
                                x=file.path(path, plot.type))
            plot.file.name <- gsub(pattern=" ",
                                replacement="_",
                                x=UpdatePrefix(prefix, plot.type))
            dir.create(plot.folder,
                                showWarnings=FALSE,
                                recursive=TRUE)
            return(list("title"= title,
                                "plot.folder"=plot.folder,
                                "plot.file.name"=plot.file.name))
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
                # "name"=self$name,
                "working.path"=private$working.path,
                "results.path"=private$results.path,
                "plots.path"=private$plots.path
               )
        }
        ,
        UpdateWorkingPath=function(sub.folder.to.generate)
        {
            private$working.path <- self$UpdateFolderPath(private$working.path,
                                                    sub.folder.to.generate)
            return(private$working.path)
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


