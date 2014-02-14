sparqlQueryCheckAnalysis <- function(analysisURI) {
    r <- SPARQL(sparqlServiceQueryURI, paste0("SELECT * { GRAPH <http://stats.270a.info/graph/analysis> { <" , analysisURI , "> ?p ?o } } LIMIT 1"))
    return(r$results)
}

