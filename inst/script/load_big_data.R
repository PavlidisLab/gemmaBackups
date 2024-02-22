#' Load partitioned data
#'
#' Some large data included in this package are not
#' automatically loaded during package load. Use this
#' function to load them into an environment
#'
#' @param load Which objects to load. Defaults to all available objects
#' @param envir Which environments to load the objects into
#' @export
load_big_data = function(load = <capture.output(dput(available))>,
                         envir = parent.frame()){
    gemmaBackups::load_big_data("<package_name>",
                                load = load,
                                envir = envir)

}
