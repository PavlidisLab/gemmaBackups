read_raw_json <- function(json){
    jsonlite::fromJSON(json,simplifyVector = FALSE)$data
}


generate_platforms_table <- function(json_directory){
    out <- list.files(json_directory,full.names = TRUE,recursive = TRUE,include.dirs = FALSE) %>%
        lapply(read_raw_json) %>% do.call(c,.) %>% processPlatforms() %>%
        dplyr::filter(!duplicated(platform.ID))
}

generate_platforms_list <- function(json_directory){
    out <- list.files(json_directory,full.names = TRUE,recursive = TRUE,include.dirs = FALSE) %>%
        lapply(read_raw_json) %>% do.call(c,.)

    names(out) = out %>% purrr::map_int('id')
    out
}

generate_dataset_platforms_table <- function(json_directory){
    files <- list.files(json_directory,full.names = TRUE)
    out <- files %>%
        lapply(read_raw_json) %>% lapply(processPlatforms) %>%
        do.call(rbind,.)

    out <- data.table(experiment.ID = basename(files), out)

    out
}

generate_dataset_platforms_list <- function(json_directory){
    files <- list.files(json_directory,full.names = TRUE)

    out <- list.files(json_directory,full.names = TRUE) %>%
        lapply(read_raw_json)
    names(out) <- basename(files)
    out
}



generate_platform_annotations_table <- function(file_directory){
    files <- list.files(file_directory, full.names=TRUE)

    doReadFile <- function(file) {
        if (file.exists(file)) {
            tmp <- gzfile(file)
            ret <- tmp %>%
                readLines() %>%
                .[which(!startsWith(., "#"))[1]:length(.)] %>%
                # Strip comments
                paste0(collapse = "\n") %>%
                {
                    fread(text = .)
                }
            close(tmp)

            ret
        } else {
            fread(tools::file_path_sans_ext(file))
        }
    }

    out <- files %>% lapply(doReadFile)

    names(out) = basename(files) %>% gsub('.gz','',.,fixed = TRUE)

    out <- out %>% lapply(function(x){
        cols = colnames(x)
        if(!cols[1] %in% c('ElementName','ProbeName')){
            return(NULL)
        } else{
            cols[1] = 'ElementName'
            colnames(x) = cols
            return(x)
        }
    })

    out = out[!(out %>% sapply(is.null))]
}


generate_datasets_table <- function(json_directory){
    out <- list.files(json_directory,full.names = TRUE,recursive = TRUE,include.dirs = FALSE) %>%
        lapply(read_raw_json) %>% do.call(c,.) %>% processDatasets() %>%
        dplyr::filter(!duplicated(experiment.ID))
}


generate_datasets_list <- function(json_directory){
    out <- list.files(json_directory,full.names = TRUE,recursive = TRUE,include.dirs = FALSE) %>%
        lapply(read_raw_json) %>% do.call(c,.)

    names(out) = out %>% purrr::map_int('id')
    out
}


generate_metadata_tables <- function(json_directory){
    files <- list.files(json_directory,full.names = TRUE)
    out <- files %>%
        lapply(read_raw_json) %>% lapply(processSamples)

    names(out) = basename(files)
    out
}

generate_metadata_list <- function(json_directory){
    files <- list.files(json_directory,full.names = TRUE)

    out <- list.files(json_directory,full.names = TRUE) %>%
        lapply(read_raw_json)
    names(out) <- basename(files)
    out
}

read_raw_file <- function(file){
    info <- file.info(file)
    data <- readBin(file,what = 'raw', n = info$size)
}

generate_expression_tables <- function(file_directory,metadata_list){
    files <- list.files(file_directory,full.names = TRUE)

    out <- files %>% lapply(function(file){
        print(file)
        data <- read_raw_file(file)
        processFile(data,samples = metadata_list[[basename(file)]])
    })
    names(out) = basename(files)
    out
}

generate_expression_tables_no_meta <- function(file_directory){
    files <- list.files(file_directory,full.names = TRUE)

    out <- files %>% lapply(function(file){
        print(file)
        data <- read_raw_file(file)
        dataset = basename(file)
        gemma.R:::processFile(data)
    })
    names(out) = basename(files)
    out
}

generate_differential_expression_analyses_tables_no_values <- function(file_directory){
    files <- list.files(file_directory,full.names = TRUE)

    out <- files %>% lapply(function(file){
        print(file)
        data <- read_raw_json(file)
        gemma.R:::processDEA(data)
    })
    names(out) = basename(files)

    out = out[!sapply(out,is.null)]
    out

}

generate_differential_expression_analyses_tables <- function(file_directory, differential_expression_values){
    files <- list.files(file_directory,full.names = TRUE)

    out <- files %>% lapply(function(file){
        print(file)
        data <- read_raw_json(file)
        processDEA(data, differential_expression_values[[basename(file)]])
    })
    names(out) = basename(files)

    out = out[!sapply(out,is.null)]
    out

}


generate_differential_expression_analyses_list <- function(file_directory){
    files <- list.files(file_directory,full.names = TRUE)

    out <- files %>% lapply(function(file){
        print(file)
        data <- read_raw_json(file)
    })
    names(out) = basename(files)

    out = out[sapply(out,length)!=0]
    out

}

generate_differential_expression_values_tables <- function(file_directory){
    files <- list.files(file_directory,recursive = TRUE,full.names = TRUE)
    exp <- files %>% dirname %>% basename()

    out <- unique(exp) %>% lapply(function(id){
        fs <- files[exp %in% id]
        exp_out <- fs %>% lapply(function(file){
            data <- file %>% read_raw_file()
            processFile(data)
        })

        names(exp_out) <- basename(fs)
        exp_out
    })

    names(out) <- unique(exp)

    out
}


split_file <- function(file,size,file_name_root){
    con = file(file,raw=TRUE,open = 'rb')
    info = file.info(file)
    piece_count = ceiling(info$size/size)
    digit_count = piece_count %>% nchar


    dir.create(dirname(paste0(file_name_root,piece_count)),recursive = TRUE,showWarnings = FALSE)

    seq_len(piece_count) %>% lapply(function(i){
        part <- readBin(con,what='raw',n=size)
        writeBin(part,paste0(file_name_root,formatC(i,width = digit_count,flag='0')))
    })
    close(con)
}

load_split_rds <- function(split_dir,envir = parent.frame()){
    files <- list.files(split_dir,full.names = TRUE)

    merged <- files %>% lapply(function(x){
        info <- file.info(x)
        readBin(x,what = 'raw',n = info$size)
    }) %>% do.call(c,.)

    tmp = tempfile()
    writeBin(merged, tmp)

    load(tmp,envir = envir)
}

untar_split_tar <- function(split_dir,path){
    files <- list.files(split_dir,full.names = TRUE)
    merged <- files %>% lapply(function(x){
        info <- file.info(x)
        readBin(x,what = 'raw',n = info$size)
    }) %>% do.call(c,.)

    tmp = tempfile()
    writeBin(merged, tmp)
    dir.create(path,recursive = TRUE,showWarnings = FALSE)
    untar(tmp,exdir = path)

}
