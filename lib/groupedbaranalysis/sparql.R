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

sparqlUpdateGroupedBarPlot <- function(analysisURI, datasetX, datasetY, refArea, refPeriod, data, analysis) { # refPeriod & datasetY hinzugefügt
    sparqlQueryStringEncoded <- URLencode(sparqlQueryStringGroupedBarPlot(datasetX, datasetY, refArea, refPeriod), reserved=TRUE) # refPeriod & datasetY hinzugefügt

#cat(paste0(data), file=stderr())
#cat(data[,1], file=stderr())

#FIXME: xsd:decimal assignment is problematic because not all values are xsd:decimal!
    statsData <- paste0("<", analysisURI, ">")
    for (i in 1:length(data[, 1])) {
        statsData <- paste0(statsData, "
            stats:data [
                a stats:DataRow ;
                stats:refArea \"", data[i, 'refArea'], "\" ;  
                stats:measureX \"", data[i, 'x'], "\"^^xsd:decimal ;
                #measureY hinzugefügt
                stats:measureY \"", data[i, 'y'], "\"^^xsd:decimal 
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

    plotURI <- paste0(siteURI, "plots/", digest(paste0(datasetX, datasetY, refArea, refPeriod), algo="sha1", serialize=FALSE), ".svg") # refPeriod  & datasetY hinzugefügt

    sparqlQueryURI <- paste0("<", sparqlEndpoints$stats, "?query=", sparqlQueryStringEncoded, ">")
  

    query <- paste0("
INSERT DATA {
    GRAPH <http://stats.270a.info/graph/analysis> {
        ", sparqlQueryURI, "
            rdfs:label \"SPARQL Query URI to retrieve the data for '", resourceLabels[datasetX], "' and '", resourceLabels[datasetY], "'\"@en .

        provenance:", analysis$id, "
            a prov:Activity ;
            rdfs:label \"Generated Analysis '", resourceLabels[datasetX], "' and '", resourceLabels[datasetY], "'\"@en ;

            prov:startedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasAssociatedWith <http://csarven.ca/#i> ;
            prov:used ", sparqlQueryURI, " ;
            prov:used <https://github.com/csarven/lsd-analysis> ;
            prov:used <", datasetX, "> ;
            prov:used <", datasetY, "> ; # datasetY hinzugefügt
            #prov:used <", refArea, "> ; # TODO: refArea entfernen

            prov:generated <", analysisURI, "> ;
            dcterms:license <", licenseURI, ">
        .

        <", analysisURI, ">
            a stats:Analysis ;
            a prov:Entity ;
            rdfs:label \"Analysis of '", resourceLabels[datasetX], "' and '", resourceLabels[datasetY], "'\"@en ;

            prov:wasGeneratedBy provenance:", analysis$id, " ;
            prov:generatedAtTime \"", now, "\"^^xsd:dateTime ;
            prov:wasDerivedFrom ", sparqlQueryURI, " ;
            prov:wasAttributedTo <", creatorURI, "> ;
            dcterms:creator <", creatorURI, "> ;
            dcterms:license <", licenseURI, "> ;

            stats:graph <", plotURI ,"> ;

            stats:datasetX <", datasetX, "> ;
            stats:datasetX <", datasetY, "> ; # datasetY hinzugefügt
            #stats:refArea <", refArea, "> ; # TODO: refArea entfernen

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



sparqlQueryGetAnalysisSummaryGroupedBarPlot <- function(analysisURI) {
   

    q <- paste0("
PREFIX stats: <http://stats.270a.info/vocab#>

SELECT *
WHERE {
    GRAPH <http://stats.270a.info/graph/analysis> {
        <", analysisURI, ">
            stats:datasetX ?datasetX ;
            stats:datasetY ?datasetY ; # datasetY hinzugefügt
            #stats:refArea ?refArea ; # TODO: refArea entfernen
            stats:graph ?graph ;
            stats:n ?n ;
    }
}
");

    r <- SPARQL(sparqlServiceQueryURI, q)
    return(r$results)
}





sparqlQueryGroupedBarPlot <- function(datasetX, datasetY, refArea, refPeriod) { # refPeriod & datasetY hinzugefügt
    q <- sparqlQueryStringGroupedBarPlot(datasetX, datasetY, refArea, refPeriod) # refPeriod & datasetY hinzugefügt
    r <- SPARQL(sparqlEndpoints$stats, q)
    return(r$results)
}

# erhält Daten aus sQGroupedBarPlot(....) aus server.R
sparqlQueryStringGroupedBarPlot <- function(datasetX, datasetY, refArea, refPeriod) { # refPeriod & datasetY hinzugefügt
#XXX: Move this to config
    datasetNameX <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetX, perl=TRUE)
    datasetNameY <- gsub("http://([^.]*).270a.info/dataset/.*", "\\1", datasetY, perl=TRUE) # datasetY hinzugefügt
#print(datasetNameX)

    domainX <- gsub("http://([^/]*).*", "\\1", datasetX, perl=TRUE)
    domainY <- gsub("http://([^/]*).*", "\\1", datasetY, perl=TRUE) # datasetY hinzugefügt
#print(domainX)

    if (datasetNameX != datasetX && datasetNameY != datasetY) { # ganzer Block hinzugefügt (3 Zeilen & obere 2 Zeilen auskommentiert)
        endpointX <- sparqlEndpoints[datasetNameX]
        endpointY <- sparqlEndpoints[datasetNameY]

        print(paste0("RefAreaListe: ", refArea))
        
        # Splits refAreas & writes them in Vector
        s <- strsplit(c(s = refArea), ",") # trennt refArea, wo "," sind & schreibt in Vector -> refArea besteht aus allen refAreas in URL

        # TODO: Schleife, die durch refArea geht, je nach dem wie viele refAreas in URL vorhanden sind
        #for(i in 1:length(refArea)) {
        #    print(s$s[i])
        #}  
        
        # TODO: Teil wird nicht benötigt -> Ausgabe in SPARQL Query mit s$s[1] usw.
        refArea1 <- s$s[1] # teilt 1. refArea der refArea1 zu
        refArea2 <- s$s[2] # teilt 2. refArea der refArea2 zu
        print(paste0("REFAREA1: ", refArea1))
        print(paste0("REFAREA2: ", refArea2))


#print(endpointX)

# TODO: eigenes SPARQL Query einbauen
    query <- paste0("
SELECT DISTINCT ?refArea ?x ?y # ?refPeriodX in ?refArea geändert, ?y hinzugefügt
WHERE {
    SERVICE <",endpointX,"> {
        SELECT DISTINCT ?refArea ?x # ?refPeriodX in ?refArea geändert
        WHERE {
            ?observationX qb:dataSet <", datasetX, "> .

            ?propertyRefArea rdfs:subPropertyOf* sdmx-dimension:refArea .
            ?observationX ?propertyRefArea ?refAreaEndpoint . # Zeile hinzugefügt

            ?observationX ?propertyRefPeriod year:", refPeriod, " . # Zeile hinzugefügt


            ?propertyMeasureX rdfs:subPropertyOf* sdmx-measure:obsValue .
            ?observationX ?propertyMeasureX ?x .

            ?refAreaEndpoint skos:notation* ?refArea. # Zeile hinzugefügt 

            # TODO: filter anpassen, je nach Anzahl vorhandener refAreas
            FILTER (?refArea  = '", s$s[1], "' || ?refArea  = '", s$s[2], "' || ?refArea  = '", s$s[3], "') # Zeile hinzugefügt
        }
    }

    SERVICE <",endpointY,"> {
        SELECT DISTINCT ?refArea ?y # ?refPeriodX in ?refArea geändert
        WHERE {
            ?observationY qb:dataSet <", datasetY, "> .

            ?propertyRefArea rdfs:subPropertyOf* sdmx-dimension:refArea .
 
            ?observationY ?propertyRefArea ?refAreaEndpoint . # Zeile hinzugefügt

            ?observationY ?propertyRefPeriod year:", refPeriod, " . # Zeile hinzugefügt

            ?propertyMeasureY rdfs:subPropertyOf* sdmx-measure:obsValue .
            ?observationY ?propertyMeasureY ?y .

            ?refAreaEndpoint skos:notation* ?refArea. # Zeile hinzugefügt 

            # TODO: filter anpassen, je nach Anzahl vorhandener refAreas
            FILTER (?refArea  = '", s$s[1], "' || ?refArea  = '", s$s[2], "' || ?refArea  = '", s$s[3], "') # Zeile hinzugefügt

        }
    }
}
#ORDER BY ?refPeriodX ?x
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
