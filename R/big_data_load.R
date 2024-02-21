#' Load expression and differential expression data
#'
#' Due to Github single file size limitations, expression and differential
#' expression data for the datasets are not automatically loaded in package
#' load. Use this function to load \code{\link{expression}} and \code{\link{differential_expression_values}}
#' objects
#' @param load Which datasets to load.
#'
#' @export
load_values <- function(load = c('expression','differential_expression'),envir = parent.frame()){
    if('expression' %in% load){
        load_split_rds(system.file('expression.rda',package = 'gemmaLegacy'),envir = envir)
    }
    if('differential_expression' %in% load){
        load_split_rds(system.file('differential_expression_values.rda',package = 'gemmaLegacy'), envir = envir)
    }
}
