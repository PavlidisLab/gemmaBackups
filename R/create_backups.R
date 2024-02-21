#' Backup datasets
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_datasets = function(dataset_ids,
                           file_directory = here::here('data-raw'),
                           overwrite = TRUE){

    dg = digest::digest(dataset_ids)

    datasets_list = gemma.R::get_datasets_by_ids(dataset_ids, raw=TRUE) %>%
        gemma.R::get_all_pages(binder = c,
                               directory = file.path(file_directory,'datasets',dg),overwrite = overwrite)
    NULL

}

#' Backup sample metadata
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_sample_metadata = function(dataset_ids,
                                  file_directory = here::here('data-raw'),
                                  overwrite = TRUE){

    dataset_ids %>% lapply(function(id){
        gemma.R::get_dataset_samples(id,raw = TRUE,file = file.path(file_directory,'metadata',id),
                                     overwrite = overwrite)
    })
    NULL
}

#' Backup expression data
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_expression_data = function(dataset_ids,
                                  file_directory = here::here('data-raw'),
                                  overwrite = TRUE){

    dataset_ids %>% pbapply::pblapply(function(id){
        tryCatch(
            gemma.R::get_dataset_processed_expression(
                id, raw = TRUE,
                file = file.path(file_directory,'expression',id),
                overwrite = overwrite),error = function(e){NULL})
        NULL
    })
    NULL
}

#' Backup differential expression contrasts
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_differential_expression_contrasts = function(dataset_ids,
                                                    file_directory = here::here('data-raw'),
                                                    overwrite = TRUE){

    dataset_ids %>% pbapply::pblapply(function(id){
        tryCatch(
            gemma.R::get_dataset_differential_expression_analyses(
                id, raw = TRUE,
                file = file.path(file_directory,'difExpAna',id),
                overwrite = overwrite),error = function(e){NULL})
        NULL
    })
    NULL
}

#' Backup differential expression values
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_differential_expression_values = function(dataset_ids,
                                                 file_directory = here::here('data-raw'),
                                                 overwrite = TRUE){

    dataset_ids %>% pbapply::pblapply(function(id){
        diffs <- gemma.R::get_dataset_differential_expression_analyses(id,raw = TRUE)
        resultSet <- diffs %>% purrr::map('resultSets') %>% purrr::map(function(x){x %>% purrr::map('id')}) %>% unlist %>% unique

        dir.create(file.path(file_directory,'difExpVals',id),recursive = TRUE)
        lapply(resultSet,function(x){
            gemma.R:::.getResultSets(x,raw = TRUE,
                           file = file.path(file_directory,'difExpVals',id,x),overwrite = overwrite)
        })
        NULL
    })

    NULL
}

#' Backup Platforms
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_plaftorms = function(platform_ids,
                            file_directory = here::here('data-raw'),
                            overwrite = TRUE){

    dg = digest::digest(platform_ids)
    platforms_to_backup <- gemma.R::get_platforms_by_ids(platform_ids,
                                                raw = TRUE) %>%
        gemma.R::get_all_pages(binder = c, directory = file.path(file_directory,'platforms',dg),
                      overwrite = overwrite)
    NULL
}

#' Backup dataset platforms
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_dataset_platforms = function(dataset_ids,
                                    file_directory = here::here('data-raw'),
                                    overwrite = TRUE){

    dataset_ids %>% pbapply::pblapply(function(id){
        gemma.R::get_dataset_platforms(id,raw = TRUE,file = file.path(file_directory,'dataset_platforms',id),overwrite = overwrite)
    })

    dataset_plaftorms <- generate_dataset_platforms_table(file.path(file_directory,'dataset_platforms'))
}


#' @title backup_args
#' @description Saves raw outputs from the gemma API to be processed
#'
#'
#' @name backup_args
#' @param platform_ids ids of platforms to be backed up
#' @param dataset_ids ids of datasets to be backed up
#' @param file_directory path to save the raw output files. "data-raw" is default
#' for easy package creation
#' @param overwrite Boolean. Should existing files be overwritten
#'
NULL
