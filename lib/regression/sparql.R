prefixes <- paste0("PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx#>
PREFIX sdmx-dimension: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX sdmx-measure: <http://purl.org/linked-data/sdmx/2009/measure#>
PREFIX year: <http://reference.data.gov.uk/id/year/>
PREFIX prov: <http://www.w3.org/ns/prov#>
PREFIX stats: <http://stats.270a.info/vocab#>
PREFIX provenance: <", siteURI, "provenance/>
")

sparqlUpdateRegression <- function(analysisURI, datasetX, datasetY, refPeriod, data, analysis) {
    sparqlQueryStringEncoded <- URLencode(sparqlQueryStringRegression(datasetX, datasetY, refPeriod), reserved=TRUE)


#FIXME: xsd:decimal assignment is problematic because not all values are xsd:decimal!
    statsData <- paste0("<", analysisURI, ">")
    for (i in 1:length(data[, 1])) {
        statsData <- paste0(statsData, "     
            stats:data [
                a stats:DataRow ;
                stats:refArea ", data[i, 'refAreaY'], " ;
                stats:measureX \"", data[i, 'x'], "\"^^xsd:decimal ;
                stats:measureY \"", data[i, 'y'], "\"^^xsd:decimal ;
                stats:identityX ", data[i, 'identityX'], " ;
                stats:identityY ", data[i, 'identityY'], "
            ] ;"
        )
    }

    statsSummary <- paste0("<", analysisURI, ">")
    for (i in 1:length(analysis$modelsData[, 1])) {
        statsSummary <- paste0(statsSummary, "
            stats:summary [
                a stats:Summary ;
                stats:linearModel \"", analysis$modelsData[i, 'linearModel'], "\" ;
                stats:adjustedRSquared \"", analysis$modelsData[i, 'adjustedRSquared'], "\"^^xsd:double ;
                stats:yIntercept \"", analysis$modelsData[i, 'yIntercept'], "\"^^xsd:double ;
                stats:slope \"", analysis$modelsData[i, 'slope'], "\"^^xsd:double
            ] ;"
#                stats:lineOfBestFit \"y=5048.421+(15304.1273*x)\" ;
        )
    }


    now <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

    plotURI <- paste0(siteURI, "plots/", digest(paste0(datasetX, datasetY, refPeriod), algo="sha1", serialize=FALSE), ".svg")

    sparqlQueryURI <- paste0("<", sparqlEndpoints$stats, "?query=", sparqlQueryStringEncoded, ">")

    query <- paste0("
INSERT DATA {
    GRAPH <http://stats.270a.info/graph/analysis> {
        ", sparqlQueryURI, "
            rdfs:label \"SPARQL Query URI to retrieve the data for '", resourceLabels[datasetX], "' and '", resourceLabels[datasetY], "' at '", resourceLabels[refPeriod], "'\"@en .

        provenance:", analysis$id, "
            a prov:Activity ;
            rdfs:label \"Generated Analysis '", resourceLabels[datasetX], "' and '", resourceLabels[datasetY], "' at '", resourceLabels[refPeriod], "'\"@en ;

            prov:startedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasAssociatedWith <http://csarven.ca/#i> ;
            prov:used ", sparqlQueryURI, " ;
            prov:used <https://github.com/csarven/lsd-analysis> ;
            prov:used <", datasetX, "> ;
            prov:used <", datasetY, "> ;
            prov:used <", refPeriod, "> ;

            prov:generated <", analysisURI, "> ;
            dcterms:license <", licenseURI, ">
        .

        <", analysisURI, ">
            a stats:Analysis ;
            a prov:Entity ;
            rdfs:label \"Analysis of '", resourceLabels[datasetX], "' and '", resourceLabels[datasetY], "' at '", resourceLabels[refPeriod], "'\"@en ;

            prov:wasGeneratedBy provenance:", analysis$id, " ;
            prov:generatedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasDerivedFrom ", sparqlQueryURI, " ;
            prov:wasAttributedTo <", creatorURI, "> ;
            dcterms:creator <", creatorURI, "> ;
            dcterms:license <", licenseURI, "> ;

            stats:graph <", plotURI ,"> ;

            stats:independentVariable <", datasetX, "> ;
            stats:dependentVariable <", datasetY, "> ;
            stats:refPeriod <", refPeriod, "> ;

            stats:n \"", nrow(data), "\"^^xsd:integer ;
            stats:pValue \"", analysis$meta$pValue , "\"^^xsd:double ;
            stats:correlation \"", analysis$meta$correlation, "\"^^xsd:double ;
            stats:correlationMethod \"", analysis$meta$correlationMethod, "\" ;
            stats:maxAdjustedRSquared \"", analysis$meta$maxAdjustedRSquared, "\"^^xsd:double ;
            stats:bestModel \"", analysis$meta$bestModel, "\"
        .

        ", statsData, "
        .

        ", statsSummary, "
        .
    }
}
")
    q <- paste0(prefixes, query)
    r <- SPARQL(sparqlServiceUpdateURI, update=q, curl_args=list(style="post"))

    return(r)
}



sparqlQueryGetAnalysisSummaryRegression <- function(analysisURI) {
    q <- paste0("
PREFIX stats: <http://stats.270a.info/vocab#>

SELECT *
WHERE {
    GRAPH <http://stats.270a.info/graph/analysis> {
        <", analysisURI, ">
            stats:independentVariable ?datasetX ;
            stats:dependentVariable ?datasetY ;
            stats:refPeriod ?refPeriod ;
            stats:graph ?graph ;
            stats:n ?n ;
            stats:pValue ?pValue ;
            stats:correlation ?correlation ;
            stats:correlationMethod ?correlationMethod ;
            stats:maxAdjustedRSquared ?maxAdjustedRSquared ;
            stats:bestModel ?bestModel
    }
}
");

    r <- SPARQL(sparqlServiceQueryURI, q)
    return(r$results)
}



#sparqlQueryGetAnalysisDataRegression <- function(analysisURI) {
#    q <- paste0("
#PREFIX stats: <http://stats.270a.info/vocab#>

#SELECT DISTINCT ?refAreaY ?x ?y ?identityX ?identityY
#WHERE {
#    GRAPH <http://stats.270a.info/graph/analysis> {
#        <", analysisURI, ">
#            stats:data [
#                stats:refArea ?refAreaY ;
#                stats:measureX ?x ;
#                stats:measureY ?y ;
#                stats:identityX ?identityX ;
#                stats:identityY ?identityY
#            ]
#    }
#}
#");

#    r <- SPARQL(sparqlServiceQueryURI, q)
#    return(r$results)
#}


sparqlQueryRegression <- function(datasetX, datasetY, refPeriod) {
    q <- sparqlQueryStringRegression(datasetX, datasetY, refPeriod)
    r <- SPARQL(sparqlEndpoints$stats, q)
    return(r$results)
}

sparqlQueryStringRegression <- function(datasetX, datasetY, refPeriod) {
#XXX: Move this to config
    datasetNameX <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetX, perl=TRUE)
    datasetNameY <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetY, perl=TRUE)
#print(datasetNameX)
#print(datasetNameY)

    domainX <- gsub("http://([^/]*).*", "\\1", datasetX, perl=TRUE)
    domainY <- gsub("http://([^/]*).*", "\\1", datasetY, perl=TRUE)
#print(domainX)
#print(domainY)

    if (datasetNameX != datasetX && datasetNameY != datasetY) {
        endpointX <- sparqlEndpoints[datasetNameX]
        endpointY <- sparqlEndpoints[datasetNameY]
#print(endpointX)
#print(endpointY)

    query <- paste0("
SELECT DISTINCT ?refAreaY ?x ?y ?identityX ?identityY
WHERE { 
    SERVICE <",endpointX,"> {
        SELECT DISTINCT ?identityX ?refAreaX ?refAreaXExactMatch ?x
        WHERE {
            ?observationX qb:dataSet <", datasetX, "> .
            ?observationX ?propertyRefPeriodX <", refPeriod, "> .
            ?propertyRefAreaX rdfs:subPropertyOf* sdmx-dimension:refArea .
            ?observationX ?propertyRefAreaX ?refAreaX .
            ?propertyMeasureX rdfs:subPropertyOf* sdmx-measure:obsValue .
            ?observationX ?propertyMeasureX ?x .

            <", datasetX, "> qb:structure/stats:identityDimension ?propertyIdentityX .
            ?observationX ?propertyIdentityX ?identityX .

            OPTIONAL {
                ?refAreaX skos:exactMatch ?refAreaXExactMatch .
                FILTER (REGEX(STR(?refAreaXExactMatch), \"^http://", domainY, "/\"))
            }

            ?refAreaX skos:notation ?refAreaCodeX .
            FILTER (!REGEX(?refAreaCodeX, \"^[0-9]\"))
#            FILTER (DATATYPE(?x) = xsd:decimal || DATATYPE(?x) = xsd:double)
        }
    }
    SERVICE <",endpointY,"> {
        SELECT DISTINCT ?identityY ?refAreaY ?refAreaYExactMatch ?y
        WHERE {
            ?observationY qb:dataSet <", datasetY, "> .
            ?observationY ?propertyRefPeriodY <", refPeriod, "> .
            ?propertyRefAreaY rdfs:subPropertyOf* sdmx-dimension:refArea .
            ?observationY ?propertyRefAreaY ?refAreaY .
            ?propertyMeasureY rdfs:subPropertyOf* sdmx-measure:obsValue .
            ?observationY ?propertyMeasureY ?y .

            <", datasetY, "> qb:structure/stats:identityDimension ?propertyIdentityY .
            ?observationY ?propertyIdentityY ?identityY .

            OPTIONAL {
                ?refAreaY skos:exactMatch ?refAreaYExactMatch .
                FILTER (REGEX(STR(?refAreaYExactMatch), \"^http://", domainX, "/\"))
            }

            ?refAreaY skos:notation ?refAreaCodeY .
            FILTER (!REGEX(?refAreaCodeY, \"^[0-9]\"))
#            FILTER (DATATYPE(?y) = xsd:decimal || DATATYPE(?y) = xsd:double)
        }
    }
    FILTER (?refAreaYExactMatch = ?refAreaX || ?refAreaXExactMatch = ?refAreaY || ?refAreaY = ?refAreaX)
}
ORDER BY ?identityY ?identityX ?x ?y
")


#                <", datasetY, ">
#                    qb:structure/qb:component/qb:dimension ?propertyRefPeriodY ;
#                    qb:structure/qb:component/qb:dimension ?propertyRefAreaY ;
#                    qb:structure/qb:component/qb:measure ?propertyMeasureY .

#                ?propertyRefPeriodY rdfs:subPropertyOf* sdmx-dimension:refPeriod .

#                MINUS {
#                    ?refAreaY skos:exactMatch ?refAreaYExactMatch .
#                    FILTER (!CONTAINS(STR(?refAreaYExactMatch), \"http://", domainX, "/\"))
#                }


        q <- paste(prefixes, query)
#        print(q)

        return(q)
    }


#    }
#    else {
#        #TODO: Error: Unrecognized dataset
#    }
}
