
#' Backup everything about given datasets
#'
#' This function backs up everything about listed datasets as raw files
#'
#' @inheritParams backup_args
#' @param metadata if TRUE, sample metadata will be backed up
#' @param expression if TRUE, expression data will be backed up
#' @param dif_exp_contrasts if TRUE, differential expression contrasts will be backed up
#' @param dif_exp_values if TRUE, sample differential expression values will be backed up
#' @param platforms if TRUE, platforms used with the datasets will be backed up
#' @export
backup_all =function(dataset_ids,
                     file_directory = here::here('data-raw'),
                     overwrite = TRUE,
                     metadata = TRUE,
                     expression = TRUE,
                     dif_exp_contrasts = TRUE,
                     dif_exp_values = TRUE,
                     platforms = TRUE){

    if(expression && !metadata) warning('Offline processing expression files require sample metadata to be backed up. This is OK if you only wish to keep the rda files and discard the raw API output')
    if(dif_exp_contrasts && ! dif_exp_values) warning('Offline processing differential expression contrasts require differential expression values to be backed up. This is OK if you only wish to keep the rda files and discard the raw API output')

    backup_datasets(dataset_ids,file_directory,overwrite)
    if (metadata){
        print('downloading sample metadata')
        backup_sample_metadata(dataset_ids,file_directory,overwrite)
    }
    if (expression) {
        print('downloading expression data')
        backup_expression_data(dataset_ids,file_directory,overwrite)
    }
    if (dif_exp_contrasts){
        print('downloading contrast data')
        backup_differential_expression_contrasts(dataset_ids,file_directory,overwrite)
    }
    if(dif_exp_values){
        print('downloading differential expression values data')
        backup_differential_expression_values(dataset_ids,file_directory,overwrite)
    }

    if(platforms){
        print('downloading platform data')
        pf = backup_dataset_platforms(dataset_ids,file_directory,overwrite)
        backup_plaftorms(pf$platform.ID,file_directory,overwrite)
        backup_platform_annotations(pf$platform.ID,file_directory,overwrite)
    }
}

