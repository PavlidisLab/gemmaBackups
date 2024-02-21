# overrides to gemma.R processors to prevent requiring more API calls when processing

#' Processes a response as a gzip file
#'
#' @param content The content from an `http_get` request
#'
#' @return A processed data.table
#'
#' @keywords internal
processFile <- function(content, ...) {
    attr <- attributes(content)
    attributes(content)<- NULL
    tmp <- tempfile() # Make a temp file
    writeBin(content, tmp) # Save to that file
    tmp2 <- gzfile(tmp)
    ret <- tmp2 %>%
        readLines() %>%
        .[which(!startsWith(., "#"))[1]:length(.)] %>%
        # Strip comments
        paste0(collapse = "\n") %>%
        paste0('\n') %>%
        {
            fread(text = .)
        }
    close(tmp2)
    unlink(tmp) # Delete the temp file
    # Process matrix according to data type
    if (colnames(ret)[1] == "Probe") {
        ret <- processExpressionMatrix(ret,...)
    } else if (colnames(ret)[1] == "id") {
        ret <- processDEMatrix(ret)
    } else {
        ret <- processDesignMatrix(ret)
    }
    attributes(ret) <- c(attributes(ret),attr)
    return(ret)
}


#' Processes expression matrix
#'
#' @param m The expression matrix to process
#'
#' @return A processed matrix
#'
#' @keywords internal
processExpressionMatrix <- function(m,samples) {


    m <- m[,!colnames(m) %in% c('Sequence','GemmaId'),with = FALSE]
    # here we standardize the output column names so that they fit output
    # from other endpoints
    m_cols <- make.names(colnames(m))

    sample_ids <- samples %>% purrr::map('sample') %>% purrr::map_chr('name')
    sample_names <- samples %>% purrr::map_chr('name')


    sample_matches <- sample_ids %>% gsub(' ','',.,fixed = TRUE) %>%
        make.names %>% purrr::map_int(function(x){
            o <- grep(paste0(x,'_'),m_cols, fixed = TRUE)
            if(length(o)==0){
                return(NA_integer_)
            } else{
                return(o)
            }
        })
    sample_names <- sample_names[!is.na(sample_matches)]
    sample_matches <- sample_matches[!is.na(sample_matches)]


    colnames(m)[sample_matches] <- sample_names
    assertthat::assert_that(all(sample_names %in% colnames(m)))

    m
}



# good test cases 442, 448, 200, 174
# 200 also has statements, 548 double statements
# for values 326
# GSE26366 has a gene fusion
# GSE106 has measurements
#' Processes JSON as a differential expression analysis
#'
#' @param d The JSON to process
#'
#' @return A data table with information about the differential expression
#' analysis of the queried dataset. Note that this funciton does not return
#' differential expression values themselves. Use \code{\link{get_differential_expression_values}}
#' to get differential expression values (see examples).
#'
#' The fields of the output data.table are:
#'
#' \itemize{
#'     \item \code{result.ID}: Result set ID of the differential expression analysis.
#'     May represent multiple factors in a single model.
#'     \item \code{contrast.ID}: Id of the specific contrast factor. Together with the result.ID
#'     they uniquely represent a given contrast.
#'     \item \code{experiment.ID}: Id of the source experiment
#'     \item \code{baseline.category}: Category for the contrast
#'     \item \code{baseline.categoryURI}: URI for the baseline category
#'     \item \code{baseline.factors}: Characteristics of the baseline. This field is a data.table
#'     \item \code{experimental.factors}: Characteristics of the experimental group. This field is a data.table
#'     \item \code{subsetFactor.subset}: TRUE if the result set belong to a subset, FALSE if not. Subsets are created when performing differential expression to avoid unhelpful comparisons.
#'     \item \code{subsetFactor.category}: Category of the subset
#'     \item \code{subsetFactor}: Characteristics of the subset. This field is a data.table
#'     \item \code{probes.analyzed}: Number of probesets represented in the contrast
#'     \item \code{genes.analyzed}: Number of genes represented in the contrast
#' }
#'
#' @keywords internal
processDEA <- function(d, dif_exp_vals) {
    # Initialize internal variables to avoid R CMD check notes

    result_ids <- d %>% purrr::map('resultSets') %>% purrr::map(function(x){x %>% accessField('id')})

    result_factors <- seq_along(result_ids) %>% lapply(function(i){
        results <- seq_along(result_ids[[i]]) %>% lapply(function(j){


            if(length(d[[i]]$resultSets[[j]]$experimentalFactors)==1){

                contrast.id <-  d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values %>% accessField('id',NA_integer_)

                baseline_id <- d[[i]]$resultSets[[j]]$baselineGroup$id

                baseline_factor <-  d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values[contrast.id == baseline_id]

                non_control_factors <- d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$values[!contrast.id %in% baseline_id]
                non_control_ids <- contrast.id[!contrast.id %in% baseline_id]
                size <- length(non_control_ids)

                exp.factors <- non_control_factors %>%
                    purrr::map(processFactorValueValueObject)

                out <- data.table(
                    result.ID = d[[i]]$resultSets[[j]]$id,
                    contrast.ID = non_control_ids,
                    experiment.ID = ifelse(is.null(d[[i]]$sourceExperiment),
                                           d[[i]]$bioAssaySetId,
                                           d[[i]]$sourceExperiment),
                    factor.category = d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$category,
                    factor.category.URI = d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$categoryUri %>%
                        nullCheck(NA_character_),
                    factor.ID =  d[[i]]$resultSets[[j]]$experimentalFactors[[1]]$id %>%
                        nullCheck(NA_integer_),
                    baseline.factors = d[[i]]$resultSets[[j]]$baselineGroup %>%
                        processFactorValueValueObject %>% list() %>% rep(size),
                    experimental.factors = exp.factors,
                    subsetFactor.subset = d[[i]]$isSubset %>% nullCheck(),
                    subsetFactor = d[i] %>% purrr::map('subsetFactorValue') %>%
                        purrr::map(processFactorValueValueObject) %>%
                        do.call(rbind,.) %>% list() %>%
                        rep(size),
                    probes.analyzed = d[[i]]$resultSets[[j]]$numberOfProbesAnalyzed %>% nullCheck(NA_integer_),
                    genes.analyzed =  d[[i]]$resultSets[[j]]$numberOfGenesAnalyzed %>% nullCheck(NA_integer_)
                )

            }else{
                # if more than 2 factors are present take a look at the
                # differential expression results to isolate the relevant results
                # this adds quite a bit of overhead for studies like this but
                # they should be relatively rare. if coupled with memoisation
                # overall hit on performance should not be too much
                # this was needed because for multi-factor result-sets, the baseline
                # for each factor is not specified


                ids <- d[[i]]$resultSets[[j]]$experimentalFactors %>%
                    purrr::map('values')  %>%
                    purrr::map(function(x){x %>% accessField('id')}) %>%
                    expand.grid()

                factor_ids <- d[[i]]$resultSets[[j]]$experimentalFactors %>% purrr::map_int('id')

                dif_exp <- dif_exp_vals[as.character(d[[i]]$resultSets[[j]]$id)]

                relevant_ids <- dif_exp[[1]] %>% colnames %>%
                    {.[grepl('[0-9]_pvalue',.)]} %>% strsplit('_') %>% lapply(function(x){
                        x[c(-1,-length(x))] %>% as.integer()
                    }) %>% as.data.frame %>% t

                if(ncol(relevant_ids)>0){
                    relevant_id_factor_id <- relevant_ids[1,] %>% purrr::map_int(function(x){
                        apply(ids,2,function(y){x %in% y}) %>% which %>% {factor_ids[[.]]}
                    })

                    colnames(relevant_ids) <- relevant_id_factor_id

                    experimental_factors <-
                        d[[i]]$resultSets[[j]]$experimentalFactors %>%
                        purrr::map('id') %>%
                        purrr::map(function(x){d[[i]]$factorValuesUsed[[as.character(x)]]})
                    names(experimental_factors) <- d[[i]]$resultSets[[j]]$experimentalFactors %>%
                        purrr::map_int('id')
                    exp.factors <- seq_len(nrow(relevant_ids)) %>%
                        purrr::map(function(k){
                            seq_along(relevant_ids[k,]) %>% purrr::map(function(l){
                                factors <- experimental_factors[[colnames(relevant_ids)[l]]]
                                ids <- factors %>% purrr::map_int('id')
                                out <- factors[[which(ids == relevant_ids[k,l])]]%>% processFactorValueValueObject
                                return(out)

                            }) %>% {do.call(rbind,.)}
                        })

                    size <- length(exp.factors)

                    out <- data.table(
                        result.ID = d[[i]]$resultSets[[j]]$id,
                        contrast.ID = unname(apply(relevant_ids,1,paste,collapse = '_')),
                        experiment.ID = ifelse(is.null(d[[i]]$sourceExperiment), d[[i]]$bioAssaySetId, d[[i]]$sourceExperiment),
                        factor.category = d[[i]]$resultSets[[j]]$experimentalFactors %>%
                            purrr::map_chr('category') %>% unlist %>% sort %>%
                            paste(collapse = ','),
                        factor.category.URI = d[[i]]$resultSets[[j]]$experimentalFactors %>%
                            purrr::map_chr('categoryUri') %>% unlist %>% sort %>% paste(collapse = ','),
                        factor.ID = d[[i]]$resultSets[[j]]$experimentalFactors %>%  purrr::map_int('id') %>% unlist %>% sort %>% paste(collapse=','),
                        baseline.factors = d[[i]]$resultSets[[j]]$baselineGroup %>%
                            processFactorValueValueObject %>% list() %>% rep(size),
                        experimental.factors = exp.factors,
                        subsetFactor.subset = d[[i]]$isSubset %>% nullCheck(),
                        subsetFactor = d[i] %>% purrr::map('subsetFactorValue') %>%
                            purrr::map(processFactorValueValueObject) %>%
                            do.call(rbind,.) %>% list() %>%
                            rep(size),
                        probes.analyzed = d[[i]]$resultSets[[j]]$numberOfProbesAnalyzed %>% nullCheck(NA_integer_),
                        genes.analyzed =  d[[i]]$resultSets[[j]]$numberOfGenesAnalyzed %>% nullCheck(NA_integer_)
                    )

                } else {
                    # if no ids were present in the expression_values matrix,
                    # there's nothing to return
                    return(NULL)
                }
            }
        }) %>% do.call(rbind,.)

        # process baseline factors for interaction effects
        contrast_factors <- results$factor.ID %>% as.character %>% strsplit(',')
        interactions <- contrast_factors %>%
            purrr::map_int(length) %>%
            {.>1} %>%
            which
        for(j in interactions){
            factors <- contrast_factors[[j]]

            baselines <- factors %>% lapply(function(x){
                baseline <- results[results$factor.ID == x,] %$%
                    baseline.factors %>% unique
                # baseline is accessed per result set. all should be the same
                # this should hold unless something upstream changes
                assertthat::assert_that(length(baseline)==1)
                return(baseline[[1]])
            }) %>% do.call(rbind,.)

            results$baseline.factors[[j]] = baselines
        }

        return(results)
    }) %>% do.call(rbind,.)
}
