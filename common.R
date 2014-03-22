#TODO: Move this to common functions
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
