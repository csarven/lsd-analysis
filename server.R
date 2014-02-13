library(shiny)
library(memoise)
library(ggplot2)
library(SPARQL)
library(digest)
library(gridSVG)

source("config.R", local=TRUE)
source("resourceLabels.R", local=TRUE)
source("sparql.R", local=TRUE)
sQS <- memoise(sparqlQueryString)
sQ <- memoise(sparqlQuery)
sQCA <- memoise(sparqlQueryCheckAnalysis)
sU <- memoise(sparqlUpdate)
sQGAD <- memoise(sparqlQueryGetAnalysisData)
sQGAS <- memoise(sparqlQueryGetAnalysisSummary)


shinyServer(function(input, output, session) {
    getURLQueryString <- function() { parseQueryString(session$clientData$url_search) }
    urlPath <- function() { c(p = session$clientData$url_pathname) }
    paths <- function() { unlist(strsplit(urlPath(), "/|.html")) }

    getData <- reactive({
        paths <- paths()
        if(length(paths) == 2) {
            return(NULL)
        }

        if(length(paths) == 5) {
            s <- strsplit(c(s = paths[3]), ":")
            datasetX <- paste0(namespaces[s$s[1]], s$s[2])
            s <- strsplit(c(s = paths[4]), ":")
            datasetY <- paste0(namespaces[s$s[1]], s$s[2])
            s <- strsplit(c(s = paths[5]), ":")
            refPeriod <- paste0(namespaces[s$s[1]], s$s[2])
        }
        else {
            datasetX <- input$datasetX
            datasetY <- input$datasetY
            refPeriod <- input$refPeriod
        }

        analysisURI <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, strsplit(c(s = session$clientData$url_pathname), ".html"))

        analysisSummary <- sQGAS(analysisURI)
        if (length(analysisSummary) > 0) {
            #Exists in store

#            analysis <- getAnalysis(datasetX, datasetY, refPeriod, data)
#            data <- sQGAD(analysisURI)

            id <- digest(paste0(datasetX, datasetY, refPeriod), algo="sha1", serialize=FALSE)

            data <- read.csv(paste0("www/csv/", id, ".csv"), header=T)

            meta <- data.frame("correlation"=analysisSummary$correlation, "pValue"=analysisSummary$pValue, "maxAdjustedRSquared"=analysisSummary$maxAdjustedRSquared, "bestModel"=analysisSummary$bestModel, "correlationMethod"=analysisSummary$correlationMethod, "graph"=analysisSummary$graph)

            analysis <- list("datasetX"=datasetX, "datasetY"=datasetY, "refPeriod"=refPeriod, "data"=data, "meta"=meta, "id"=id)

#print(session$sendCustomMessage)
#isolate({
#    session$sendCustomMessage("progress", "foo bar baz")
#})

        }
        else {
            #Query analysis
            data <- sQ(datasetX, datasetY, refPeriod)

            if (length(data) > 0) {
                #Build analysis
                analysis <- getAnalysis(datasetX, datasetY, refPeriod, data)

                #Update store
                storeUpdated <- sU(analysisURI, datasetX, datasetY, refPeriod, data, analysis)
            }
            else {
                analysis <- list("warning" = paste0("<p class=\"warning\">Insufficient observations to analyze <em><a href=\"", datasetX, "\">", resourceLabels[datasetX], "</a></em> and <em><a href=\"", datasetY, "\">", resourceLabels[datasetY], "</a> for reference period <a href=\"", refPeriod, "\">", resourceLabels[refPeriod], "</a></em>. Please try a different combination.</p>"))
            }
        }

        return(analysis)
    })


    getAnalysis <- function(datasetX, datasetY, refPeriod, data) {
        #From http://stackoverflow.com/questions/10492817/how-can-i-generate-a-guid-in-r#answer-10493590
        baseuuid <- paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")
        uuid <- list("uuid"=paste0(substr(baseuuid,1,8), "-", substr(baseuuid,9,12), "-", "4", substr(baseuuid,13,15), "-", sample(c("8","9","a","b"),1), substr(baseuuid,16,18), "-", substr(baseuuid,19,30)))

        id <- digest(paste0(datasetX, datasetY, refPeriod), algo="sha1", serialize=FALSE)

        x <- data$x
        y <- data$y

        write.csv(data, file=paste0("www/csv/", id, ".csv"))

        correlation <- cor(x, y, use="complete.obs", method=correlationMethod)
        pValue <- cor.test(x, y, method=correlationMethod)$p.value

        modelsData <- data.frame("linearModel"=models, "adjustedRSquared"=NA, "yIntercept"=NA, "slope"=NA, "lobf"=NA)

        for (m in models) {
#XXX: Revisit subset x > 0 .. Alternative: Add 0.0000000001 to x.
            model <- lm(formula=m, data=data, subset=x>0)
            s <- summary(model)
            adjustedRSquared <- s$adj.r.squared
            yIntercept <- model$coefficients[["(Intercept)"]]
            slope <- model$coefficients[[2]]
#print(m)
#print(model)
#print(s)
#print(adjustedRSquared)
#print(yIntercept)
#print(slope)

            if(!is.na(yIntercept) & !is.na(slope)) {
                modelsData[modelsData$linearModel == m, "adjustedRSquared"] = adjustedRSquared
                modelsData[modelsData$linearModel == m, "yIntercept"] = yIntercept
                modelsData[modelsData$linearModel == m, "slope"] = slope
#            modelsData[modelsData$lm == m, "lobf"] = paste("y=",yIntercept,"+(",slope,"*x)", sep="")
            }
        }
        maxAdjustedRSquared <- which.max(modelsData[,"adjustedRSquared"])
        bestModel <- modelsData[maxAdjustedRSquared, "linearModel"]
#print(modelsData)
#print(bestModel)

        meta <- data.frame("correlation"=correlation, "pValue"=pValue, "maxAdjustedRSquared"=modelsData[maxAdjustedRSquared, "adjustedRSquared"], "bestModel"=bestModel, "correlationMethod"=correlationMethod)

        return(list("datasetX"=datasetX, "datasetY"=datasetY, "refPeriod"=refPeriod, "data"=data, "modelsData"=modelsData, "meta"=meta, "id"=id))
    }



    output$plot <- renderPrint({
        paths <- paths()
        if(length(paths) != 2) {
            analysis <- getData()

            if (is.null(analysis[["warning"]])) {
                csvPath <- paste0("/csv/", analysis$id, ".csv")

                if (is.null(analysis$meta$graph)) {
                    url_protocol <- session$clientData$url_protocol
                    url_hostname <- session$clientData$url_hostname
                    url_pathname <- session$clientData$url_pathname

                    plotPath <- paste0("plots/", analysis$id, ".svg")

                    if (!file.exists(paste0("www/", plotPath))) {
                        data <- analysis$data
                        x <- data$x
                        y <- data$y
                        xLabel <- resourceLabels[analysis$datasetX]
                        yLabel <- resourceLabels[analysis$datasetY]

                        refPeriod <- resourceLabels[analysis$refPeriod]
                        correlation <- analysis$meta$correlation
                        pValue <- analysis$meta$pValue
                        bestModel <- analysis$meta$bestModel

                        identityXCount <- length(unique(data$identityX))
                        identityYCount <- length(unique(data$identityY))
                        Group <- ''
                        if (identityYCount >= identityXCount) {
                            Group <- data$identityY
                        }
                        else {
                            Group <- data$identityX
                        }

                        g <- ggplot(data, environment = environment(), aes(x=data$x, y=data$y)) + geom_point(size=2, shape=1) + labs(list(x=xLabel, y=yLabel, title=paste0(refPeriod, " correlation")))

                        if (length(unique(Group)) > 1) {
                            plot_labeller <- function(variable, value){
                                return(resourceLabels[gsub("<|>", '', as.character(value))])
                            }

                            g <- g + facet_grid(identityY ~ identityX, labeller=plot_labeller) + theme(legend.position="none")
                        }

                #TODO: Refactor
    #                    statsmooth <- ''
                        if (bestModel == "y ~ x") {
                            g <- g + stat_smooth(method=lm, formula = y ~ x, na.rm=TRUE)
                        }
                        else {
                            if (bestModel == "y ~ log(x)") {
                                g <- g + stat_smooth(method=lm, formula = y ~ log(x), na.rm=TRUE)
                            }
                            else {
                                if (bestModel == "y ~ poly(x, 2, raw=TRUE)") {
                                    g <- g + stat_smooth(method=lm, formula = y ~ poly(x, 2, raw=TRUE), na.rm=TRUE)
                                }
                                else {
                                    if (bestModel == "y ~ poly(x, 3, raw=TRUE)") {
                                       g <- g + stat_smooth(method=lm, formula = y ~ poly(x, 3, raw=TRUE), na.rm=TRUE)
                                    }
                                }
                            }
                        }

    #                    g <- g + stat_smooth()

                        g <- g + annotate("text", x=Inf, y=Inf, label="270a.info", hjust=1.3, vjust=2, color="#0000E4", size=4)

    #                    width <- 7 * length(unique(Group))

                        ggsave(plot=g, file=paste0("www/", plotPath), width=7, height=7)
                    }

    #                sparqlQueryStringEncoded <- URLencode(sparqlQueryString(analysis$datasetX, analysis$datasetY, analysis$refPeriod))
    #                sparqlQueryURI <- paste0("http://stats.270a.info/sparql?query=", sparqlQueryStringEncoded)

                    o <- HTML(paste0("
                        <img src=\"", url_protocol, "//", url_hostname, "/", plotPath, "\" width=\"100%\"/>
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
    })



    output$statsSummary <- renderPrint({
        paths <- paths()
        if(length(paths) != 2) {
            analysis <- getData()

            if (is.null(analysis[["warning"]])) {
                o <- HTML(paste0("
                    <table id=\"lsd-analysis-results\">
                        <caption>Analysis results</caption>
                        <tbody>
                            <tr><th>Independent variable</th><td><a href=\"", analysis$datasetX, "\">", resourceLabels[analysis$datasetX] ,"</a></td></tr>
                            <tr><th>Dependent variable</th><td><a href=\"", analysis$datasetY, "\">", resourceLabels[analysis$datasetY] ,"</a></td></tr>
                            <tr><th>Reference period</th><td><a href=\"", analysis$refPeriod, "\">", resourceLabels[analysis$refPeriod] ,"</a></td></tr>
                            <tr><th>N (sample size)</th><td>", nrow(analysis$data), "</td></tr>
                            <tr><th>Correlation (", correlationMethod, ")</th><td>", analysis$meta$correlation, "</td></tr>
                            <tr><th>p-value</th><td>", analysis$meta$pValue, "</td></tr>
                            <tr><th>Adjusted R<sup>2</sup> (max tested)</th><td>", as.character(analysis$meta$maxAdjustedRSquared), "</td></tr>
                            <tr><th>Linear model (best tested)</th><td>", as.character(analysis$meta$bestModel), "</td></tr>
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
    })
})
