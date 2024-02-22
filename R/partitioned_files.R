#' @export
load_big_data = function(package_name, load = 'all',envir = parent.frame()){
    data_path = system.file('big_data',package = package_name)

    available = list.files(data_path)

    if(load!= 'all'){available = available[tools::file_path_sans_ext(available) %in% load]}
    available %>% lapply(function(x){
        load_split_rds(file.path(data_path,available),envir = envir)
    })

}




split_file <- function(file,size,file_name_root){
    con = file(file,raw=TRUE,open = 'rb')
    info = file.info(file)
    piece_count = ceiling(info$size/size)
    digit_count = piece_count %>% nchar


    dir.create(file.path(file_name_root),recursive = TRUE,showWarnings = FALSE)

    seq_len(piece_count) %>% lapply(function(i){
        part <- readBin(con,what='raw',n=size)
        writeBin(part,file.path(file_name_root,formatC(i,width = digit_count,flag='0')))
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

