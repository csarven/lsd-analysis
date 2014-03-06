getAnalysisGroupedBarPlot <- function(datasetX, datasetY, refArea, refPeriod, data) { # refPeriod & datasetY hinzugefügt
    id <- digest(paste0(datasetX, refArea), algo="sha1", serialize=FALSE)

    write.csv(data, file=paste0("www/csv/", id, ".csv"))

    meta <- data.frame("n"= nrow(data$x))

    return(list("datasetX"=datasetX, "datasetY"=datasetY, "refArea"=refArea, "refPeriod"=refPeriod, "data"=data, "meta"=meta, "id"=id)) # refPeriod & datasetY hinzugefügt damit Ausgabe möglich
}


outputPlotGroupedBarPlot <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
        csvPath <- paste0("/csv/", analysis$id, ".csv")
        
        print(analysis$datasetX)
        print(analysis$refArea)
        print(analysis$refPeriod)
        print(analysis$data)
        print(analysis$data$x)

        if (is.null(analysis$meta$graph)) {
            plotPath <- paste0("plots/", analysis$id, ".svg") # erstellt Pfadname des Plots

            print("test1")

            # Plot doesn't exists in directory
            if (!file.exists(paste0("www/", plotPath))) { # wenn nicht auskommentiert, springt gar nicht in File, da Plot schon existiert, aber leer

                print("test2")                

                data <- analysis$data
                x <- data$x
                datasetXLabel <- resourceLabels[analysis$datasetX]
                datasetYLabel <- resourceLabels[analysis$datasetY] # Zeile hinzugefügt
                refArea <- resourceLabels[analysis$refArea] # resourceLabels in /var/shiny-server/www/lsd-analysis/lib/resourceLabels.R -> dort Länder ergänzen

                print(datasetXLabel)
                print("test3")
                print(data)
                print(data[1,2])
                print(analysis$refArea)
                print(data$refArea)
                print(paste0("REFAREA1: ", data$refArea[1]))
                print(data$x)

                #datasetLabels <- data.frame(dsl = c(analysis$datasetX, analysis$datasetY))
                #print(datasetLabels[1,])

                #g <- ggplot(data, environment = environment(), aes(x=data$refArea, y=data$x, fill=datasetLabels[,1])) + geom_bar(stat="identity", position="dodge") + labs(list(x="Reference Area", y="Value", title=paste0(datasetXLabel, " for ", refArea)))

                # TODO: Bar Plot only gets data from datasetX
                g <- ggplot(data, environment = environment(), aes(x=data$refArea, y=data$x)) + geom_bar(stat="identity", width=.5, position="dodge") + labs(list(x="Reference Area", y="Value", title=paste0(datasetXLabel, " and ", datasetYLabel, "for ", data$refArea)))

               # g <- ggplot(data, environment = environment(), aes(x=data$refPeriod, y=data$x)) + geom_line(aes=(size=2)) + labs(list(x="Reference Period", y="Value", title=paste0(datasetXLabel, " for ", refArea)))

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


outputAnalysisSummaryGroupedBarPlot <- function(analysis) {
    if (is.null(analysis[["warning"]])) {
        
        data <- analysis$data        

        # refArea URIs that will be used in Summary 
        # fügt Namespace von Worldbank mit skos:notation refArea zusammen, damit als Link verwendet werden kann -> http://worldbank.270a.info/classification/country/CH        
        refAreaURI1 <- paste0(namespaces$wbcountry, data$refArea[1])
        refAreaURI2 <- paste0(namespaces$wbcountry, data$refArea[2])
        refAreaURI3 <- paste0(namespaces$wbcountry, data$refArea[3])

        # Variables used for Summary
        qX <- quantile(data$x)
        minX <- qX[1]
        q1X <- qX[2]      
        meanX <- qX[3]
        q3X <- qX[4] 
        maxX <- qX[5]
        medianX <- median(data$x) # speichert Median für datasetX

        qY <- quantile(data$y)
        minY <- qY[1]
        q1Y <- qY[2]      
        meanY <- qY[3]
        q3Y <- qY[4] 
        maxY <- qY[5]
        medianY <- median(data$y) # speichert Median für datasetX



        # TODO: Summary erstellen
        # TODO: refAreas NICHT in Summary darstellen -> unnötige Information, da schon in Plot vorhanden
        # refAreas will probably not be outputed, as information is available in Plot
        o <- HTML(paste0("


            <table id=\"lsd-analysis-results-quantile\">
                <caption>Analysis results</caption>
                <tbody>
                    
                    <tr><th></th><th>Min</th><th>Q1</th><th>Mean</th><th>Q3</th><th>Max</th><th>Median</th></tr>
                    <tr><td><a href=\"", analysis$datasetX, "\">", resourceLabels[analysis$datasetX] ,"</a></td>
                        <td>", minX, "</td><td>", q1X, "</td><td>", meanX, "</td><td>", q3X, "</td><td>", maxX, "</td><td>", medianX, "</td></tr>
                    <tr><th></th><th>Min</th><th>Q1</th><th>Mean</th><th>Q3</th><th>Max</th><th>Median</th></tr>
                    <tr><td><a href=\"", analysis$datasetY, "\">", resourceLabels[analysis$datasetY] ,"</a></td>
                        <td>", minY, "</td><td>", q1Y, "</td><td>", meanY, "</td><td>", q3Y, "</td><td>", maxY, "</td><td>", medianY, "</td></tr>
                </tbody>
            <table>
            <table id=\"lsd-analysis-results\">
                <tbody>
                    <tr><th>Reference Areas</th><td><a href=\"", refAreaURI1, "\">", resourceLabels[data$refArea[1]] ,"</a></td></tr>
                    <tr><th></th><td><a href=\"", refAreaURI2, "\">", resourceLabels[data$refArea[2]] ,"</a></td></tr>
                    <tr><th></th><td><a href=\"", refAreaURI3, "\">", resourceLabels[data$refArea[3]] ,"</a></td></tr>
                    <tr><th>Reference Period </th><td>", analysis$refPeriod, "</td></tr>
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
