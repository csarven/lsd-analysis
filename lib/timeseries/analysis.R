getAnalysisTimeSeries <- function(datasetX, refArea, data) {
    id <- digest(paste0(datasetX, refArea), algo="sha1", serialize=FALSE)

    write.csv(data, file=paste0("www/csv/", id, ".csv"))

    meta <- data.frame("n"= nrow(data$x))

    return(list("datasetX"=datasetX, "refArea"=refArea, "data"=data, "meta"=meta, "id"=id))
}


outputPlotTimeSeries <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
        csvPath <- paste0("/csv/", analysis$id, ".csv")

        print("Warning PLOT")
        print(analysis$datasetX)
        print(analysis$refArea)
        print(analysis$refPeriod)
        print(analysis$data)
        print(analysis$data$x)

        if (is.null(analysis$meta$graph)) {
            plotPath <- paste0("plots/", analysis$id, ".svg")

            if (!file.exists(paste0("www/", plotPath))) {
                data <- analysis$data

                print(data)
                
                x <- data$x
                datasetXLabel <- resourceLabels[analysis$datasetX]
                refArea <- resourceLabels[analysis$refArea]

                g <- ggplot(data, environment = environment(), aes(x=data$refPeriod, y=data$x)) + geom_line(aes=(size=2)) + labs(list(x="Reference Period", y="Value", title=paste0(datasetXLabel, " for ", refArea)))


                g <- g + annotate("text", x=Inf, y=Inf, label="270a.info", hjust=1.3, vjust=2, color="#0000E4", size=4)

                ggsave(plot=g, file=paste0("www/", plotPath), width=7, height=7)
            }


            o <- HTML(paste0("
                <img src=\"", urlProtocol(), "//", urlHostname(), "/", plotPath, "\" width=\"100%\"/>
            "))
        }
        else {
            o <- HTML(paste0("
                <img src=\"", gsub("<|>", '', as.character(analysis$meta$graph)), "\" width=\"100%\"/>
            "))
        }

        o <- HTML(paste0(o, "<p id=\"download-csv\"><a href=\"", csvPath , "\">CSV</a></p>"))

        cat(format(o))
    }
}


outputAnalysisSummaryTimeSeries <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
        o <- HTML(paste0("
            <table id=\"lsd-analysis-results\">
                <caption>Analysis results</caption>
                <tbody>
                    <tr><th>Time Series</th><td><a href=\"", analysis$datasetX, "\">", resourceLabels[analysis$datasetX] ,"</a></td></tr>
                    <tr><th>N (sample size)</th><td>", nrow(analysis$data), "</td></tr>
                </tbody>
            <table>

            <p id=\"oh-yeah\"><a href=\"", siteURI, "provenance/", analysis$id, "\">Oh yeah?</a></p>
        "))

        cat(format(o))
    }
    else {
        warning <- analysis[["warning"]]
        o <- HTML(warning)

        cat(format(o))
    }
}
