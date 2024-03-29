% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_backups.R
\name{partition_big_rdas}
\alias{partition_big_rdas}
\title{Partition large RDS}
\usage{
partition_big_rdas(
  file_directory = here::here("data"),
  output_directory = here::here("inst/big_data"),
  backup_directory = here::here("data-backup"),
  limit = 1e+08,
  remove_origin = TRUE,
  create_loading_function = TRUE
)
}
\arguments{
\item{file_directory}{Path to all rda files}

\item{output_directory}{Path to partitioned files}

\item{backup_directory}{Directory to save the backup to if remove_origin is TRUE.
set to null to don't keep a backup}

\item{limit}{Size limit for files and partitions}

\item{remove_origin}{If TRUE files partioned will be removed from the directory}
}
\description{
This function partitions large RDS files into smaller pieces.
}
