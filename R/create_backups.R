#' Backup datasets
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_datasets = function(dataset_ids,
                           file_directory = here::here('data-raw'),
                           overwrite = TRUE){


    # url length limit prevents us from calling all ids at once
    lapply(seq(0,length(dataset_ids),100),function(offset){
        ds = dataset_ids[(offset+1):(offset+100)] %>% na.omit() %>% as.integer()
        dg = digest::digest(ds)
        gemma.R::get_datasets_by_ids(ds, raw=TRUE,file = file.path(file_directory,'datasets',dg),overwrite = overwrite)
    })

    NULL

}

#' Backup sample metadata
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_sample_metadata = function(dataset_ids,
                                  file_directory = here::here('data-raw'),
                                  overwrite = TRUE){

    dataset_ids %>% pblapply(function(id){
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

        dir.create(file.path(file_directory,'difExpVals',id),recursive = TRUE,showWarnings = FALSE)
        lapply(resultSet,function(x){
            gemma.R:::.getResultSets(x,raw = TRUE,
                           file = file.path(file_directory,'difExpVals',id,x),overwrite = overwrite)
        })
        NULL
    })

    NULL
}

#' Backup platforms
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_plaftorms = function(platform_ids,
                            file_directory = here::here('data-raw'),
                            overwrite = TRUE){


    lapply(seq(0,length(platform_ids),100),function(offset){
        ps = platform_ids[(offset+1):(offset+100)] %>% na.omit() %>% as.integer()
        dg = digest::digest(ps)
        gemma.R::get_platforms_by_ids(ps, raw=TRUE,file = file.path(file_directory,'platforms',dg),overwrite = overwrite)
    })

    NULL
}


#' Backup platform annotations
#' @inherit backup_args description
#' @inheritParams backup_args
#' @export
backup_platform_annotations = function(platform_ids,
                                       file_directory = here::here('data-raw'),
                                       overwrite = TRUE){

    dir.create(file.path(file_directory,'annotations'),recursive = TRUE,showWarnings = FALSE)

    platform_ids %>% pblapply(function(id){
        tryCatch(gemma.R::get_platform_annotations(id,file = file.path(file_directory,'annotations',id),overwrite = overwrite),error = function(e){NULL})
        NULL
    })
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


#' Populate "data" directory with backed up data
#'
#' @export
package_rdas = function(file_directory = here::here('data-raw'),overwrite=TRUE){
    existing_backups = list.files(file_directory,recursive = FALSE)

    if ('datasets' %in% existing_backups){
        datasets <- generate_datasets_table(file.path(file_directory,'datasets'))
        datasets_list <- generate_datasets_list(file.path(file_directory,'datasets'))

        usethis::use_data(datasets,overwrite = overwrite)
        usethis::use_data(datasets_list,overwrite = overwrite)
    }


    if('metadata' %in% existing_backups){
        metadata <- generate_metadata_tables(file.path(file_directory,'metadata/'))
        metadata_list <- generate_metadata_list(file.path(file_directory,'metadata/'))

        usethis::use_data(metadata,overwrite = overwrite)
        usethis::use_data(metadata_list,overwrite = overwrite)
    }

    if('expression' %in% existing_backups){
        if('metadata' %in% existing_backups){
            expression <- generate_expression_tables(file.path(file_directory,'expression/'),metadata_list)
        } else{
            warning('Metadata is not backed up with expression data, referring to Gemma API to process expression results. This is OK if you only wish to keep the rda files and discard the raw API output')
            expression <- generate_expression_tables_no_meta(file.path(file_directory,'expression/'))
        }
        usethis::use_data(expression,overwrite = overwrite)
        rm(expression)
        gc()
    }

    if('difExpVals' %in% existing_backups){
        differential_expression_values <- generate_differential_expression_values_tables(file.path(file_directory,'difExpVals/'))
        usethis::use_data(differential_expression_values,overwrite = overwrite)
    }

    if('difExpAna' %in% existing_backups){
        if('difExpVals' %in% existing_backups){
            differential_expression_analyses <-
                generate_differential_expression_analyses_tables(file.path(file_directory,'difExpAna/'),
                                                                 differential_expression_values)
        } else{
            warning('Differential expression values are not backed up with contrast metadata, referring to Gemma API to process expression results. This is OK if you only wish to keep the rda files and discard the raw API output')
            differential_expression_analyses = generate_differential_expression_analyses_tables_no_values(file.path(file_directory,'difExpAna/'))
        }
        usethis::use_data(differential_expression_analyses,overwrite = overwrite)
        rm(differential_expression_values)
        gc()
    }

    if('platforms' %in% existing_backups){
        platforms = generate_platforms_table(file.path(file_directory,'platforms'))
        platforms_list = generate_platforms_list(file.path(file_directory,'platforms'))

        usethis::use_data(platforms,overwrite = overwrite)
        usethis::use_data(platforms_list,overwrite = overwrite)
    }

    if('annotations' %in% existing_backups){
        platform_annotations = generate_platform_annotations_table(file.path(file_directory,'annotations'))
        usethis::use_data(platform_annotations,overwrite = overwrite)
    }

    if("dataset_platforms" %in% existing_backups){
        dataset_plaftorms = generate_dataset_platforms_table(file.path(file_directory,'dataset_platforms'))
        dataset_plaftorms_list = generate_dataset_platforms_list(file.path(file_directory,'dataset_platforms'))

        usethis::use_data(dataset_plaftorms,overwrite = overwrite)
        usethis::use_data(dataset_plaftorms_list,overwrite = overwrite)
    }

}
