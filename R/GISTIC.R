#' Gistic
#'
#' @inheritParams maftools::readGistic
#' @inherit maftools::readGistic return
#'
#' @export
convert_gistic_to_maftools <- function(gisticAllLesionsFile, gisticAmpGenesFile, gisticDelGenesFile, gisticScoresFile,
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
convert_gistic_files_to_crux <- function(gisticAllLesionsFile, gisticAmpGenesFile, gisticDelGenesFile, gisticScoresFile, outfile,
                           cnLevel = c("all", "deep", "shallow"), isTCGA = FALSE, verbose = TRUE, compress = c("gzip", "bzip2", "xz", "none")){

  # Parse Arguments
  cnLevel <- rlang::arg_match(cnLevel)
  compress <- rlang::arg_match(compress)
  if(compress == "none") compress <- FALSE

  # Create maftools gistic object
  maftools_gistic_object <- convert_gistic_to_maftools(gisticAllLesionsFile = gisticAllLesionsFile,
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
#' @inheritParams convert_gistic_files_to_crux
#' @inheritDotParams convert_gistic_files_to_crux
#'
#' @return NULL, invisibly.
#' @export
#'
gistic_folder_to_crux <- function(gistic_folder, outfile, ...){

  child_paths <- dir(gistic_folder, full.names = TRUE, recursive = TRUE)

  path_all_lesions <- child_paths[grep(x = basename(child_paths),pattern = "^all_lesions.conf_[0-9]+.txt$")]
  path_amp_genes <-  child_paths[grep(x = basename(child_paths),pattern = "^amp_genes.conf_[0-9]+.txt$")]
  path_del_genes <-  child_paths[grep(x = basename(child_paths),pattern = "^del_genes.conf_[0-9]+.txt$")]
  path_scores <-  child_paths[grep(x = basename(child_paths),pattern = "^scores\\.gistic$")]

  all_files <- c(path_all_lesions, path_amp_genes, path_del_genes, path_scores)

  #browser()
  # add some assertions maybe

  convert_gistic_files_to_crux(
    gisticAllLesionsFile = path_all_lesions,
    gisticAmpGenesFile = path_amp_genes,
    gisticDelGenesFile = path_del_genes,
    gisticScoresFile = path_scores,
    outfile = outfile,
    ...
  )
}

assert_file_exists <- assertions::assert_create(func = file.exists, default_error_msg = "Could not find file: {.file {arg_name}}")
has_permission <- function(filepaths, permission = c('write', 'execute', 'read')){
  permission_to_mode <- c('write'=2, 'execute'=1, 'read'=4)
  permission <- rlang::arg_match(permission)
  mode <- permission_to_mode[match(permission, names(permission_to_mode))]

  exit_code <- file.access(names = filepaths, mode)
  if (exit_code == 0) return(TRUE)
  else return(FALSE)
}

assert_file_does_not_exist <- assertions::assert_create(func = function(x) {!file.exists(x)}, default_error_msg = "File already exists: {.file {arg_value}}. Please remove then try again")

assert_file_permission <- assertions::assert_create_chain(
  assert_file_exists,
  assertions::assert_create(
    func = has_permission,
    default_error_msg = "No permission to {permission} file/folder {.file {arg_name}}"
  )
)



#' Gistic Zip to CRUX
#'
#' Create Crux-compatible GISTIC RDS file
#'
#' @param gistic_tar path to gistic tar file (can be gzip compressed)
#' @param outfile path to write CRUX RDS
#' @inheritDotParams convert_gistic_files_to_crux
#' @return Run for its side effects (creation of an RDS crux gistic at path 'outfile')
#' @export
#'
convert_gistic_tar_to_crux <- function(gistic_tar, outfile, ...){
  assert_file_exists(gistic_tar)
  assert_file_permission(dirname(gistic_tar), permission = "write")
  assert_file_permission(dirname(outfile), permission = "write")

  # name of folder to extract files to
  gistic_folder <-  tools::file_path_sans_ext(x = gistic_tar, compression = TRUE)


  # Figure out what folder your going to get
  untarred = sub(
    x=utils::untar(gistic_tar, list = TRUE),
    pattern = "\\/.*$",
    replacement = ""
  )[1]
  gistic_folder = paste0(dirname(gistic_tar), "/",untarred)
  assert_file_does_not_exist(gistic_folder)

  # Unzip
  utils::untar(gistic_tar, exdir = dirname(gistic_tar))

  # Create
  gistic_folder_to_crux(gistic_folder, outfile = outfile, ...)

  # Messages
  cli::cli_alert_info("deleting untarred folder: {.file {gistic_folder}}")

}


