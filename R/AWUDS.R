.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}

#' AWUDS
#'
#' \tabular{ll}{
#' Package: \tab AWUDS\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to help parse and analyze AWUDS data.
#'
#' @name AWUDS-package
#' @docType package
NULL


#' Included data
#' 
#'\itemize{
#'  \item{category}{category data.frame}
#'  \item{county}{county data.frame}
#'  \item{dataelement}{dataelement data.frame}
#'  \item{huc8}{huc8 data.frame}
#'  \item{calculation}{calculation data.frame}
#'  \item{calculation.formula}{calculation.formula data.frame}
#'}
#' @name lookupTables
#' @docType data
#' @aliases category county dataelement huc8 calculation calculation.formula
#' @export category county dataelement huc8 calculation calculation.formula
#'
#' @examples 
#' head(category)
#' head(county)
#' head(dataelement)
#' head(huc8)
#' head(calculation)
#' head(calculation.formula)
NULL