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
        gemma.R::get_datasets_by_ids(ds,limit = 100, raw=TRUE,file = file.path(file_directory,'datasets',dg),overwrite = overwrite)
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

    dataset_ids %>% pbapply::pblapply(function(id){
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
        gemma.R::get_platforms_by_ids(ps, limit = 100,raw=TRUE,file = file.path(file_directory,'platforms',dg),overwrite = overwrite)
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

    platform_ids %>% pbapply::pblapply(function(id){
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


#' Partition large RDS
#'
#' This function partitions large RDS files into smaller pieces.
#' @param file_directory Path to all rda files
#' @param output_directory Path to partitioned files
#' @param backup_directory Directory to save the backup to if remove_origin is TRUE.
#' set to null to don't keep a backup
#' @param limit Size limit for files and partitions
#' @param remove_origin If TRUE files partioned will be removed from the directory

#'
#' @export
partition_big_rdas = function(file_directory = here::here('data'),
                              output_directory = here::here('inst/big_data'),
                              backup_directory = here::here('data-backup'),
                              limit = 1e+8,
                              remove_origin = TRUE,
                              create_loading_function = TRUE){


    all_data <- list.files(file_directory,full.names = FALSE)

    data_info <- all_data %>% sapply(function(x){file.info(file.path(file_directory,x))})
    big_files <- which(data_info['size',] > limit) %>% names

    big_files %>% lapply(function(x){
        print(x)
        split_file(file = file.path(file_directory,x),
                   size = limit,
                   file_name_root = file.path(output_directory,x))
        if(!is.null(backup_directory) && remove_origin){
            dir.create(backup_directory,recursive = TRUE,showWarnings = FALSE)
            file.copy(file.path(file_directory,x),
                      file.path(backup_directory,x),overwrite = TRUE)
        }
        if(remove_origin){file.remove(file.path(file_directory,x))}
    })

    if(create_loading_function && length(big_files)>0){
        package_name = pkgload::pkg_name(here::here())
        auto_add = readLines(system.file('script/load_big_data.R',package = 'gemmaBackups'))
        available = tools::file_path_sans_ext(big_files)
        env = environment()
        auto_add %>% sapply(glue::glue,.open ='<',.close = '>',.envir = env) %>%
            writeLines(here::here("R/load_big_data.R"))
        devtools::document()
    }

    NULL
}

#' Zip raw data and create partitioned files
#'
#' @export
create_raw_archive = function(file_directory = here::here('data-raw'),
                              limit = 1e+8){

    list.files(file_directory,full.names = TRUE,
               recursive = TRUE,
               include.dirs = FALSE) %>%
        {.[!grepl(pattern = 'archive.',.)]} %>%
        fs::path_rel()%>%
        tar(file.path(file_directory,'archive.tar.gz'),
            files = .,compression = 'gzip',
            tar="tar")

    split_file(file = file.path(file_directory,'archive.tar.gz'),
               size = 1e+8,
               file_name_root =  file.path(file_directory,'archive'))
}
