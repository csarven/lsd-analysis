library(shiny)
library(memoise)
library(ggplot2)
library(SPARQL)
library(digest)
library(gridSVG)

urlProtocol <- ''
urlHostname <- ''
urlPathname <- ''

source("config.R", local=TRUE)
source("resourceLabels.R", local=TRUE)
source("lib/sparql.R", local=TRUE)
source("lib/regression.sparql.R", local=TRUE)
source("lib/regression.analysis.R", local=TRUE)
source("lib/timeseries.sparql.R", local=TRUE)
source("lib/timeseries.analysis.R", local=TRUE)

#sQCA <- memoise(sparqlQueryCheckAnalysis)
sQRegression <- memoise(sparqlQueryRegression)
sURegression <- memoise(sparqlUpdateRegression)
#sQGADRegression <- memoise(sparqlQueryGetAnalysisDataRegression)
sQGASRegression <- memoise(sparqlQueryGetAnalysisSummaryRegression)

sQTimeSeries <- memoise(sparqlQueryTimeSeries)
sUTimeSeries <- memoise(sparqlUpdateTimeSeries)
#sQGADTimeSeries <- memoise(sparqlQueryGetAnalysisDataTimeSeries)
sQGASTimeSeries <- memoise(sparqlQueryGetAnalysisSummaryTimeSeries)


shinyServer(function(input, output, session) {
    getURLQueryString <- function() { parseQueryString(session$clientData$url_search) }
    urlProtocol <<- function() { c(p = session$clientData$url_protocol) }
    urlHostname <<- function() { c(p = session$clientData$url_hostname) }
    urlPathname <<- function() { c(p = session$clientData$url_pathname) }
    paths <- function() { unlist(strsplit(urlPathname(), "/|.html")) }

    getData <- reactive({
        analysisURI <- paste0(urlProtocol(), "//", urlHostname(), strsplit(c(s = urlPathname()), ".html"))

        paths <- paths()

        if(length(paths) == 2) {
            return(NULL)
        }

        switch(paste0("case", length(paths)),
            #Regression Analysis
            case5={
                s <- strsplit(c(s = paths[3]), ":")
                datasetX <- paste0(namespaces[s$s[1]], s$s[2])
                s <- strsplit(c(s = paths[4]), ":")
                datasetY <- paste0(namespaces[s$s[1]], s$s[2])
                s <- strsplit(c(s = paths[5]), ":")
                refPeriod <- paste0(namespaces[s$s[1]], s$s[2])

                analysisParams = paste0(datasetX, datasetY, refPeriod)

                analysisSummary <- sQGASRegression(analysisURI)
            },
            #Time Series
            case4={
                s <- strsplit(c(s = paths[3]), ":")
                datasetX <- paste0(namespaces[s$s[1]], s$s[2])
                s <- strsplit(c(s = paths[4]), ":")
                refArea <- paste0(namespaces[s$s[1]], s$s[2])
#cat(paste0("paths: ", paths, " s: ", s, " refArea:", refArea ,"--"), file=stderr())
                analysisParams = paste0(datasetX, refArea)

                analysisSummary <- sQGASTimeSeries(analysisURI)
            },
            #XXX: What was this for?
            {
                datasetX <- input$datasetX
                datasetY <- input$datasetY
                refPeriod <- input$refPeriod

                analysisParams = paste0(datasetX, datasetY, refPeriod)

                analysisSummary <- sQGASRegression(analysisURI)
            }
#            stop("Enter something that switches me!")
        )

        if (length(analysisSummary) > 0) {
            #Exists in store

#            analysis <- getAnalysis(datasetX, datasetY, refPeriod, data)
#            data <- sQGADRegression(analysisURI)

            id <- digest(analysisParams, algo="sha1", serialize=FALSE)

            data <- read.csv(paste0("www/csv/", id, ".csv"), header=T)


            switch(paste0("case", length(paths)),
                #Regression Analysis
                case5={
                    meta <- data.frame("correlation"=analysisSummary$correlation, "pValue"=analysisSummary$pValue, "maxAdjustedRSquared"=analysisSummary$maxAdjustedRSquared, "bestModel"=analysisSummary$bestModel, "correlationMethod"=analysisSummary$correlationMethod, "graph"=analysisSummary$graph)

                    analysis <- list("datasetX"=datasetX, "datasetY"=datasetY, "refPeriod"=refPeriod, "data"=data, "meta"=meta, "id"=id)
                },
                #Time Series
                case4={
                    meta <- data.frame("n"=analysisSummary$n, "graph"=analysisSummary$graph)

                    analysis <- list("datasetX"=datasetX, "refArea"=refArea, "data"=data, "meta"=meta, "id"=id)
                },
                {}
            )

#print(session$sendCustomMessage)
#isolate({
#    session$sendCustomMessage("progress", "foo bar baz")
#})

        }
        else {
            #Query analysis
            switch(paste0("case", length(paths)),
                #Regression Analysis
                case5={
                    data <- sQRegression(datasetX, datasetY, refPeriod)
                },
                #Time Series
                case4={
                    data <- sQTimeSeries(datasetX, refArea)
                },
                {}
            )

#cat(paste0("data: ", data), file=stderr())
#cat(paste0("data: ", data, " length(data[,1]): ", length(data[,1]), " data[2, 'refPeriodX']: ", data[2, 'refPeriodX']), file=stderr())


            if (length(data) > 0) {
                switch(paste0("case", length(paths)),
                    #Regression Analysis
                    case5={
                        #Build analysis
                        analysis <- getAnalysisRegression(datasetX, datasetY, refPeriod, data)

                        #Update store
                        storeUpdated <- sURegression(analysisURI, datasetX, datasetY, refPeriod, data, analysis)
                    },
                    #Time Series
                    case4={
                        #Build analysis
                        analysis <- getAnalysisTimeSeries(datasetX, refArea, data)
                        #Update store
                        storeUpdated <- sUTimeSeries(analysisURI, datasetX, refArea, data, analysis)
                    },
                    {}
                )
            }
#            else {
#                analysis <- list("warning" = paste0("<p class=\"warning\">Insufficient observations to analyze <em><a href=\"", datasetX, "\">", resourceLabels[datasetX], "</a></em> and <em><a href=\"", datasetY, "\">", resourceLabels[datasetY], "</a> for reference period <a href=\"", refPeriod, "\">", resourceLabels[refPeriod], "</a></em>. Please try a different combination.</p>"))
#            }
        }

        return(analysis)
    })



    output$plot <- renderPrint({
        paths <- paths()

        if(length(paths) != 2) {
            analysis <- getData()

            switch(paste0("case", length(paths)),
                #Regression Analysis
                case5={
                    outputPlotRegression(analysis)
                },
                #Time Series
                case4={
                    outputPlotTimeSeries(analysis)
                },
                {}
            )

        }
    })



    output$statsSummary <- renderPrint({
        paths <- paths()

        if(length(paths) != 2) {
            analysis <- getData()
cat(paste0(analysis), file=stderr())
            switch(paste0("case", length(paths)),
                #Regression Analysis
                case5={
                    outputAnalysisSummaryRegression(analysis)
                },
                #Time Series
                case4={
                    outputAnalysisSummaryTimeSeries(analysis)
                },
                {
                }
            )
        }
    })
})
