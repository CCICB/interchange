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


#' Gistic2CRUX
#'
#'
#' @inheritDotParams maftools::readGistic
#' @param outfile outfile path
#' @return NULL, invisibly
#'
#' @export
gistic_to_crux <- function(gisticAllLesionsFile, gisticAmpGenesFile, gisticDelGenesFile, gisticScoresFile, outfile,
                           cnLevel = c("all", "deep", "shallow"), isTCGA = FALSE, verbose = TRUE, compress = c("gzip", "bzip2")){

  cnLevel <- rlang::arg_match(cnLevel)

  maftools_gistic_object <- gistic_to_maftools(...)

  saveRDS(object = maftools_gistic_object, file = outfile)
}
