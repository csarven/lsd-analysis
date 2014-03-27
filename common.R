buildString <- function(prefix, itemPrefix, values, valuesSeparator, itemSuffix, suffix, separator, useResourceLabels) {
    string = prefix
    s <- strsplit(c(s = values), valuesSeparator)
    for (i in 1:length(s$s)) {
        v <- s$s[i]

        if (useResourceLabels == TRUE) {
            v <- resourceLabels[v]
        }

        item = paste0(itemPrefix, v, itemSuffix)

        if (i == 1) {
            string <- paste(string, item, sep="")
        }
        else {
            string <- paste(string, item, sep=separator)            
        }
    }
    string <- paste(string, suffix, sep="")

    return(string)
}

enrichment <- function(refAreas) {
    wars <- sparqlQueryGetWars(refAreas)

    enrichment <- list("wars"=wars)

    return(enrichment)
}


sparqlQueryGetWars <- function(refAreas) {
        refAreasFILTER <- buildString("FILTER (", "?refArea = '", refAreas, ",", "'", ")", " || ", FALSE)

    q <- paste0("
SELECT ?event
WHERE {
    {
        ?refAreaURI skos:notation ?refArea .
        ", refAreasFILTER, "
    }
    UNION
    { ?refAreaURI owl:sameAs ?dbpediaRefArea }
    UNION
    { ?refAreaURI skos:exactMatch ?dbpediaRefArea }
    UNION
    {
        SERVICE <http://dbpedia.org/sparql> {
            SELECT *
            WHERE {
                ?event a <http://dbpedia.org/ontology/MilitaryConflict> .
                ?event dbpedia:place ?dbpediaRefArea .
                ?event rdfs:label ?label .

                FILTER (LANG(?label) = 'en')
            }
        }
    }
}
    ")

    r <- SPARQL(sparqlServiceQueryURI, q)
    return(r$results)
}
