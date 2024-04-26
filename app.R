library(shiny)
library(DT)

# Import data
samples <- read.csv("/home/thomkall/Develop/GenotypeCheck/data/newdb.csv")

# Extract most common genotype for inds in NRM genetic database
fetchCommon <- function(data) {
    sortbyNA <- data[order(rowSums(is.na(data[, 9:24]))),]
    Indname <- sortbyNA$individ
    sortbyNA <- sortbyNA[, 9:24]
    mlGT <- c()
    for (i in 1:16) {
        temp <- as.numeric(names(sort(table(sortbyNA[,i]),
                                      decreasing = TRUE)[1]))
        temp <- ifelse(is.null(temp), yes = NA, no = temp)
        mlGT <- c(mlGT, temp)
    }

    return(mlGT)
}

listofinds <- tapply(samples, samples$individ, fetchCommon)
dfofinds <- data.frame(t(sapply(listofinds,c)))
colnames(dfofinds) <- colnames(samples)[9:24]
shinyApp(
    ui = fluidPage(DTOutput('tbl')),
    server = function(input, output) {
        output$tbl = renderDT(
            dfofinds, options = list(lengthChange = FALSE)
        )
    }
)
