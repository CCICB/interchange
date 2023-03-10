% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GISTIC.R
\name{convert_gistic_tar_to_crux}
\alias{convert_gistic_tar_to_crux}
\title{Gistic Zip to CRUX}
\usage{
convert_gistic_tar_to_crux(gistic_tar, outfile, ...)
}
\arguments{
\item{gistic_tar}{path to gistic tar file (can be gzip compressed)}

\item{outfile}{path to write CRUX RDS}

\item{...}{
  Arguments passed on to \code{\link[=convert_gistic_files_to_crux]{convert_gistic_files_to_crux}}
  \describe{
    \item{\code{compress}}{compression method}
    \item{\code{gisticAllLesionsFile}}{All Lesions file generated by gistic. e.g; all_lesions.conf_XX.txt, where XX is the confidence level. Required. Default NULL.}
    \item{\code{gisticAmpGenesFile}}{Amplification Genes file generated by gistic. e.g; amp_genes.conf_XX.txt, where XX is the confidence level. Default NULL.}
    \item{\code{gisticDelGenesFile}}{Deletion Genes file generated by gistic. e.g; del_genes.conf_XX.txt, where XX is the confidence level. Default NULL.}
    \item{\code{gisticScoresFile}}{scores.gistic file generated by gistic.}
    \item{\code{cnLevel}}{level of CN changes to use. Can be 'all', 'deep' or 'shallow'. Default uses all i.e, genes with both 'shallow' or 'deep' CN changes}
    \item{\code{isTCGA}}{Is the data from TCGA. Default FALSE.}
    \item{\code{verbose}}{Default TRUE}
  }}
}
\value{
Run for its side effects (creation of an RDS crux gistic at path 'outfile')
}
\description{
Create Crux-compatible GISTIC RDS file
}
