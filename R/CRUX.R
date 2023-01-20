#' Gistic
#'
#' @inheritParams maftools::readGistic
#' @inherit maftools::readGistic return
#'
gistic_to_maftools <- function(gisticAllLesionsFile, gisticAmpGenesFile, gisticDelGenesFile, gisticScoresFile,
                               cnLevel = c("all", "deep", "shallow"), isTCGA = FALSE, verbose = TRUE){
  requireNamespace("maftools", quietly = TRUE)
  cnLevel <- rlang::arg_match(cnLevel)

  maftools_gistic_object <- maftools::readGistic(
    gisticAllLesionsFile = gisticAllLesionsFile,
    gisticAmpGenesFile = gisticAmpGenesFile,
    gisticDelGenesFile = gisticDelGenesFile,
    gisticScoresFile = gisticScoresFile,
    cnLevel = cnLevel,
    isTCGA = isTCGA,
    verbose = verbose
  )
}


#' Gistic to CRUX
#'
#' Create Crux-compatible GISTIC RDS file
#'
#' @inheritParams maftools::readGistic
#' @param outfile outfile path
#' @param compress compression method
#' @return NULL, invisibly
#'
#' @export
gistic_files_to_crux <- function(gisticAllLesionsFile, gisticAmpGenesFile, gisticDelGenesFile, gisticScoresFile, outfile,
                           cnLevel = c("all", "deep", "shallow"), isTCGA = FALSE, verbose = TRUE, compress = c("gzip", "bzip2", "xz", "none")){

  # Parse Arguments
  cnLevel <- rlang::arg_match(cnLevel)
  compress <- rlang::arg_match(compress)
  if(compress == "none") compress <- FALSE

  # Create maftools gistic object
  maftools_gistic_object <- gistic_to_maftools(gisticAllLesionsFile = gisticAllLesionsFile,
                                                gisticAmpGenesFile = gisticAmpGenesFile,
                                                gisticDelGenesFile = gisticDelGenesFile,
                                                gisticScoresFile = gisticScoresFile,
                                                cnLevel = cnLevel,
                                                isTCGA = isTCGA,
                                                verbose = verbose)

  # Write output file
  saveRDS(object = maftools_gistic_object, file = outfile, compress = compress)
}

#' Gistic Folder to CRUX
#'
#' Create Crux-compatible GISTIC RDS file
#'
#' @param gistic_folder path to a folder containing gistic output files
#' @inheritParams gistic_files_to_crux
#' @inheritDotParams gistic_files_to_crux
#'
#' @return NULL, invisibly.
#' @export
#'
gistic_folder_to_crux <- function(gistic_folder, outfile, ...){

  child_paths <- dir(gistic_folder, full.names = TRUE, recursive = TRUE, pattern = "\\.txt$")

  path_all_lesions <- child_paths[grep(x = basename(child_paths),pattern = "^all_lesions.conf_[0-9]+.txt$")]
  path_amp_genes <-  child_paths[grep(x = basename(child_paths),pattern = "^amp_genes.conf_[0-9]+.txt$")]
  path_del_genes <-  child_paths[grep(x = basename(child_paths),pattern = "^del_genes.conf_[0-9]+.txt$")]
  path_scores <-  child_paths[grep(x = basename(child_paths),pattern = "^scores.gistic$")]

  all_files <- c(path_all_lesions, path_amp_genes, path_del_genes, path_scores)

  browser()
  # add some assertions maybe

  gistic_files_to_crux(
    gisticAllLesionsFile = path_all_lesions,
    gisticAmpGenesFile = path_amp_genes,
    gisticDelGenesFile = path_del_genes,
    gisticScoresFile = path_scores,
    outfile = outfile,
    ...
  )
}

#' Gistic Zip to CRUX
#'
#' Create Crux-compatible GISTIC RDS file
#'
#' @param gistic_zip path to gistic zip file
#' @param outfile path to write CRUX RDS to
#' @param inheritDotParams gistic_files_to_crux
#' @return Run for its side effects (creation of an RDS crux gistic at path 'outfile')
#' @export
#'
gistic_tar_to_crux <- function(gistic_tar, outfile, ...){

  # name of folder to extract files to
  gistic_folder <-  tools::file_path_sans_ext(x = gistic_tar, compression = TRUE)

  # Unzip
  untar(gistic_tar, exdir = gistic_folder)

  # Create
  gistic_folder_to_crux(gistic_folder, outfile = outfile, ...)


}
