

UpdatePrefix <- function(prefix, ...) {
    # new.prefix <- paste(prefix, postix, sep=sep)
    dots <- list(...)
    if( length(dots) != 0 ) {
        for (str in dots) {
            # str <- gsub(pattern = ".", replacement = "_", str)
            prefix <- paste(prefix, str, sep = " " )
        }

    } else {
        stop("provide a string to append to ", new.prefix)
    }
    return(prefix)
}



PlotVolcanoPlot <- function (de.results, counts.dataframe=NULL,
                             design.matrix=NULL,
                             show.plot.flag=TRUE, plotly.flag=FALSE,
                             save.plot=FALSE, plot.folder=NULL,
                             prefix.plot=NULL, threshold=0.05,
                             positive.ctrls.list=NULL)
{
    require("plotly")
    strings <- GeneratePlotStrings(path=plot.folder, prefix=prefix.plot,
                                   plot.type="VolcanoPlot")
    processed.de.results <- ProcessDEResultsForPlot(de.results = de.results,
                                        threshold = 0.05,
                                        counts.dataframe=counts.dataframe,
                                        design.matrix=design.matrix,
                                        pos.ctrls.list=positive.ctrls.list)
    ggp <- GenerateGGVolcano(processed.de.results, strings, plotly.flag)
    if (save.plot) {
        if (is.null(plot.folder)) {
            stop("Please set a folder where to plot the Volcano-Plot!")
        }
        if (!is.null(strings$plot.file.name)) {
            SaveGGplot(ggplot.to.save = ggp, plot.folder = strings$plot.folder,
                       plot.file.name = strings$plot.file.name, plotly.flag = plotly.flag)
        }
        else {
            stop("Please set a name for the Volcano-Plot!")
        }
    }
    if (show.plot.flag) {
        if (plotly.flag) {
            ggplotly(ggp)
        }
        else {
            plot(ggp)
        }
    }
    else {
        return(ggp)
    }
}

ProcessDEResultsForPlot<-function (de.results, threshold = 0.05, counts.dataframe = NULL,
                                   design.matrix = NULL, pos.ctrls.list = NULL)
{
    de.results.new <- de.results
    de.results.new <- de.results.new[order(rownames(de.results.new)), ]
    if (!is.null(counts.dataframe)) {
        counts.dataframe.ord <- counts.dataframe[order(rownames(counts.dataframe)),
                                                 ]
    }
    if ("F" %in% colnames(de.results.new)) {
        symb.flag <- (length(which("symbol" %in% colnames(de.results.new))) !=0)
        if(symb.flag)
        {
            de.results.new <- de.results.new[, c("logFC", "PValue", "FDR", "symbol")]
        } else {
            de.results.new <- de.results.new[, c("logFC", "PValue", "FDR")]
        }

        de.results.new$padj <- format(round(de.results.new$FDR, 8), nsmall=8)
        de.results.new$pval <- de.results.new$PValue
        de.results.new$log2FoldChange <- de.results.new$logFC

        de.results.new$minuslog10pval <- -log10(de.results.new$PValue)
        de.results.new$significance <- paste("padj >=", threshold)
        idx <- which(de.results.new$FDR < threshold)
        de.results.new$significance[idx] <- paste("padj <", threshold)
        de.results.new$minuslog10PAdj <- (-1) * log10(de.results.new$FDR)
        de.results.new$method <- rep(x = "edgeR", times = dim(de.results.new)[1])
        if(symb.flag)
        {
            de.results.new$gene <- de.results.new$symbol
            de.results.new$annotated <- FALSE
            de.results.new$annotated[which(!is.na(de.results.new$symbol))] <- TRUE
        }

        if (!is.null(pos.ctrls.list)) {
            de.results.new$posc <- NA
            idx.pos <- which(tolower(de.results.new$gene) %in%
                                 tolower(pos.ctrls.list))
            print(length(idx.pos))
            if (length(idx.pos) != 0) {
                de.results.new$posc[idx.pos] <- "pos-ctrl"
            }
            else {
                warning("no positive controls found!")
            }
        }
    }
    return(de.results.new)
}


GeneratePlotStrings <- function (path = NULL, prefix, plot.type)
{
    title <- gsub(pattern = "_", replacement = " ", x = UpdatePrefix(prefix,
                                                                     plot.type))
    plot.folder <- gsub(pattern = " ", replacement = "_", x = file.path(path,
                                                                        plot.type))
    plot.file.name <- gsub(pattern = " ", replacement = "_",
                           x = UpdatePrefix(prefix, plot.type))
    if (!is.null(path))
        dir.create(plot.folder, showWarnings = FALSE, recursive = TRUE)
    return(list(title = title, plot.folder = plot.folder, plot.file.name = plot.file.name))
}

GenerateGGVolcano <- function (processed.de.results, strings, plotly.flag)
{
    require("ggplot2")
    switch(processed.de.results$method[1], edgeR = {
        if (plotly.flag) {
            xlabl <- paste0("log<sub>2</sub>(FC)")
            ylabl <- paste("-log<sub>10</sub>(PValue)")
        } else {
            xlabl <- bquote(~log[2] ~ "(FC)")
            ylabl <- bquote(~-log[10] ~ "(PValue)")
        }
        ggp <- ggplot2::ggplot(processed.de.results) +
                geom_point(aes(x = log2FoldChange,
                            y = minuslog10pval, color = significance, padj = format(padj,
                            nsmall = 10)), size = 0.7) + labs(list(title = strings$title,
                            x = xlabl, y = ylabl))
        idx.posc <- which(colnames(processed.de.results) %in% "annotated")
        if (length(idx.posc) > 0) {
            idx.pc <- which(processed.de.results$annotated)
            pos.data <- processed.de.results[processed.de.results$annotated, ]
            ggp <- ggp + geom_point(data = pos.data,
                            aes(x = log2FoldChange,
                                y = minuslog10pval,
                                text = paste0("padj=", padj, " name=", gene)),
                                shape = 1, size = 3, color = "green2")
        }
        ggp <- ggp + scale_color_manual(values = c("red2", "blue2"))
    })
    ggp <- ggp + geom_vline(xintercept = 0) +
        geom_vline(xintercept = 1, colour = "darkgreen", linetype = "dashed") +
        geom_vline(xintercept = -1,
        colour = "darkgreen", linetype = "dashed")
    return(ggp)
}
