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

sparqlUpdateTimeSeries <- function(analysisURI, datasetX, refArea, data, analysis) {
    sparqlQueryStringEncoded <- URLencode(sparqlQueryStringTimeSeries(datasetX, refArea), reserved=TRUE)

#cat(paste0(data), file=stderr())
#cat(data[,1], file=stderr())

#FIXME: xsd:decimal assignment is problematic because not all values are xsd:decimal!
    statsData <- paste0("<", analysisURI, ">")
    for (i in 1:length(data[, 1])) {
        statsData <- paste0(statsData, "
            stats:data [
                a stats:DataRow ;
                stats:refPeriodX ", data[i, 'refPeriodX'], " ;
                stats:measureX \"", data[i, 'x'], "\"^^xsd:decimal
            ] ;"
        )
    }

    statsSummary <- paste0("<", analysisURI, ">")
    for (i in 1:length(analysis$modelsData[, 1])) {
        statsSummary <- paste0(statsSummary, "
            stats:summary [
                a stats:Summary ;
                stats:n \"", analysis$modelsData[i, 'n'], "\"^^xsd:double
            ] ;"
        )
    }


    now <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

    plotURI <- paste0(siteURI, "plots/", digest(paste0(datasetX, refArea), algo="sha1", serialize=FALSE), ".svg")

    sparqlQueryURI <- paste0("<", sparqlEndpoints$stats, "?query=", sparqlQueryStringEncoded, ">")

    query <- paste0("
INSERT DATA {
    GRAPH <http://stats.270a.info/graph/analysis> {
        ", sparqlQueryURI, "
            rdfs:label \"SPARQL Query URI to retrieve the data for '", resourceLabels[datasetX], "' and '", resourceLabels[refArea], "'\"@en .

        provenance:", analysis$id, "
            a prov:Activity ;
            rdfs:label \"Generated Analysis '", resourceLabels[datasetX], "' and '", resourceLabels[refArea], "'\"@en ;

            prov:startedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasAssociatedWith <http://csarven.ca/#i> ;
            prov:used ", sparqlQueryURI, " ;
            prov:used <https://github.com/csarven/lsd-analysis> ;
            prov:used <", datasetX, "> ;
            prov:used <", refArea, "> ;

            prov:generated <", analysisURI, "> ;
            dcterms:license <", licenseURI, ">
        .

        <", analysisURI, ">
            a stats:Analysis ;
            a prov:Entity ;
            rdfs:label \"Analysis of '", resourceLabels[datasetX], "' and '", resourceLabels[refArea], "'\"@en ;

            prov:wasGeneratedBy provenance:", analysis$id, " ;
            prov:generatedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasDerivedFrom ", sparqlQueryURI, " ;
            prov:wasAttributedTo <", creatorURI, "> ;
            dcterms:creator <", creatorURI, "> ;
            dcterms:license <", licenseURI, "> ;

            stats:graph <", plotURI ,"> ;

            stats:datasetX <", datasetX, "> ;
            stats:refArea <", refArea, "> ;

            stats:n \"", nrow(data), "\"^^xsd:integer
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



sparqlQueryGetAnalysisSummaryTimeSeries <- function(analysisURI) {
    q <- paste0("
PREFIX stats: <http://stats.270a.info/vocab#>

SELECT *
WHERE {
    GRAPH <http://stats.270a.info/graph/analysis> {
        <", analysisURI, ">
            stats:datasetX ?datasetX ;
            stats:refArea ?refArea ;
            stats:graph ?graph ;
            stats:n ?n ;
    }
}
");

    r <- SPARQL(sparqlServiceQueryURI, q)
    return(r$results)
}





sparqlQueryTimeSeries <- function(datasetX, refArea) {
    q <- sparqlQueryStringTimeSeries(datasetX, refArea)
    r <- SPARQL(sparqlEndpoints$stats, q)
    return(r$results)
}

sparqlQueryStringTimeSeries <- function(datasetX, refArea) {
#XXX: Move this to config
    datasetNameX <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetX, perl=TRUE)
#print(datasetNameX)

    domainX <- gsub("http://([^/]*).*", "\\1", datasetX, perl=TRUE)
#print(domainX)

    if (datasetNameX != datasetX) {
        endpointX <- sparqlEndpoints[datasetNameX]
#print(endpointX)

    query <- paste0("
SELECT DISTINCT ?refPeriodX ?x
WHERE {
    SERVICE <",endpointX,"> {
        SELECT DISTINCT ?refPeriodX ?x
        WHERE {
            ?observationX qb:dataSet <", datasetX, "> .
            ?propertyRefArea rdfs:subPropertyOf* sdmx-dimension:refArea .
            ?observationX ?propertyRefArea <", refArea, "> .
            ?propertyRefPeriodX rdfs:subPropertyOf* sdmx-dimension:refPeriod .
            ?observationX ?propertyRefPeriodX ?refPeriodXURI .
            ?propertyMeasureX rdfs:subPropertyOf* sdmx-measure:obsValue .
            ?observationX ?propertyMeasureX ?x .

            BIND(xsd:int(SUBSTR(STR(?refPeriodXURI), 38, 4)) AS ?refPeriodX)
        }
    }
}
ORDER BY ?refPeriodX ?x
")

        q <- paste(prefixes, query)
#        print(q)

        return(q)
    }


#    }
#    else {
#        #TODO: Error: Unrecognized dataset
#    }
}
