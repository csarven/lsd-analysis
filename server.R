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
source("lib/regression/sparql.R", local=TRUE)
source("lib/regression/analysis.R", local=TRUE)
source("lib/timeseries/sparql.R", local=TRUE)
source("lib/timeseries/analysis.R", local=TRUE)
source("lib/groupedbaranalysis/sparql.R", local=TRUE)
source("lib/groupedbaranalysis/analysis.R", local=TRUE)

#sQCA <- memoise(sparqlQueryCheckAnalysis)
sQRegression <- memoise(sparqlQueryRegression)
sURegression <- memoise(sparqlUpdateRegression)
#sQGADRegression <- memoise(sparqlQueryGetAnalysisDataRegression)
sQGASRegression <- memoise(sparqlQueryGetAnalysisSummaryRegression)

sQTimeSeries <- memoise(sparqlQueryTimeSeries)
sUTimeSeries <- memoise(sparqlUpdateTimeSeries)
#sQGADTimeSeries <- memoise(sparqlQueryGetAnalysisDataTimeSeries)
sQGASTimeSeries <- memoise(sparqlQueryGetAnalysisSummaryTimeSeries)

sQGroupedBarPlot <- memoise(sparqlQueryGroupedBarPlot)
sUGroupedBarPlot <- memoise(sparqlUpdateGroupedBarPlot)
#sQGADGroupedBarPlot <- memoise(sparqlQueryGetAnalysisDataGroupedBarPlot)
sQGASGroupedBarPlot <- memoise(sparqlQueryGetAnalysisSummaryGroupedBarPlot)


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
            #Regression Analysis http://stats.270a.info/analysis/worldbank:SP.DYN.IMRT.IN/transparency:CPI2009/year:2009.html
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

            #Time Series http://localhost.stats.270a.info/analysis/worldbank:SP.DYN.IMRT.IN/wbcountry:CH.html
            case4={
                print ("test")
                s <- strsplit(c(s = paths[3]), ":")
                print(s)
                datasetX <- paste0(namespaces[s$s[1]], s$s[2])
                s <- strsplit(c(s = paths[4]), ":")
                refArea <- paste0(namespaces[s$s[1]], s$s[2])
#cat(paste0("paths: ", paths, " s: ", s, " refArea:", refArea ,"--"), file=stderr())
                analysisParams = paste0(datasetX, refArea)
                print(datasetX)
                print(refArea)
                print(analysisParams)

                analysisSummary <- sQGASTimeSeries(analysisURI)
            },

            #Grouped Bar Plot Analysis http://localhost.stats.270a.info/analysis/dev/worldbank:SE.XPD.PRIM.PC.ZS/CA,FR/2009.html
            case6={
                # Teilung der Datasets aus URL
                d <- strsplit(c(d = paths[4]), ",") # teilt unterschiedliche Datasets in URL bei ","
                # print(d$d[1]) # worldbank:SE.XPD.PRIM.PC.ZS
                # print(d$d[2]) # worldbank:SE.XPD.SECO.PC.ZS
                s <- strsplit(c(s = d$d[1]), ":") # teilt 1. Dataset bei : um Prefix und Dataset zu erhalten
                datasetX <- paste0(namespaces[s$s[1]], s$s[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.PRIM.PC.ZS
                print(paste0("datasetX = ", datasetX))
                s <- strsplit(c(s = d$d[2]), ":") # teilt 2. Dataset bei : um Prefix und Dataset zu erhalten
                datasetY <- paste0(namespaces[s$s[1]], s$s[2]) # setzt Namespace vor Dataset -> http://worldbank.270a.info/dataset/SE.XPD.SECO.PC.ZS
                print(paste0("datasetY = ", datasetY))

                #s <- strsplit(c(s = paths[4]), ":")
                #print(s$s)
                #print(s$s[1])
                #datasetX <- paste0(namespaces[s$s[1]], s$s[2])

                
                #s <- strsplit(c(s = paths[5]), ",") # ist überflüssig, da Trennung der refArea erst in sparql.R geschieht
                #print(s$s[1])
                #print(s$s[2])
                
                #refArea <- paste0(s$s[1]) # s$s[1] ist 1. Land
                #refArea2 <- paste0(s$s[2]) # s$s[2] ist 2. Land -> ist überflüssig -> in sparql.R gelöst
                #refArea <- paste0(s$s[1], s$s[2]) # speichert refArea aus URI -> im Moment nur 1 refArea möglich (z.B. CH)
                
                # refAreas (refAreas are split in sparql.R)
                refArea <- paths[5] # schreibt alle refAreas aus URL in Variabel -> Trennung der , erfolgt erst in sparql.R

                # Teilung der refPeriod aus URL
                s <- strsplit(c(s = paths[6]), ":")
                refPeriod <- paste0(s$s[1]) # speichert refPeriod aus URI

                analysisParams = paste0(datasetX, datasetY, refArea, refPeriod) # refPeriod & datasetY hinzugefügt

                print(paste0(paste="ALL Reference Areas: ", refArea))
                print(refPeriod)
                print(analysisParams)

                analysisSummary <- sQGASGroupedBarPlot(analysisURI)
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
                #Grouped Bar Plot
                case6={
                    print("Exists in store")
                    meta <- data.frame("n"=analysisSummary$n, "graph"=analysisSummary$graph)

                    analysis <- list("datasetX"=datasetX, "datasetX"=datasetY, "refArea"=refArea, "refPeriod"=refPeriod, "data"=data, "meta"=meta, "id"=id) # refPeriod & datasetY hinzugefügt
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
                #Grouped Bar Plot
                case6={
                    # sendet Daten ansparqlQueryStringGroupedBarPlot(....) in sparql.R
    
                    print("Query analysis")
                    print(refPeriod)

                    if(!is.na(datasetX) && !is.na(datasetY)) { # TODO: wird Schliefe benötigt?
                        print("datasetX & datasetY VORHANDEN")
                        data <- sQGroupedBarPlot(datasetX, datasetY, refArea, refPeriod) # refPeriod & datasetY hinzugefügt
                        #data <- sQGroupedBarPlot(datasetX, paste0(namespaces$wbcountry, refArea), refPeriod) # refPeriod hinzugefügt 
                        #data <- sQGroupedBarPlot(datasetX, paste0("http://worldbank.270a.info/classification/country/CH"))                    
                        #data <- sQGroupedBarPlot(datasetX, refArea)
                    }
                    else if(!is.na(datasetX) && is.na(datasetY)) {
                        print("datasetY NICHT VORHANDEN")
                    }
                    
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
                  #Grouped Bar Plot
                    case6={
                        #Build analysis
                        analysis <- getAnalysisGroupedBarPlot(datasetX, datasetY, refArea, refPeriod, data) # refPeriod & datasetY hinzugefügt
                        #Update store
                        storeUpdated <- sUGroupedBarPlot(analysisURI, datasetX, datasetY, refArea, refPeriod, data, analysis) # refPeriod & datasetY hinzugefügt
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
                #Grouped Bar Plot
                case6={
                    outputPlotGroupedBarPlot(analysis)
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
                #Grouped Bar Plot
                case6={
                    outputAnalysisSummaryGroupedBarPlot(analysis)
                },
                {
                }
            )
        }
    })
})
