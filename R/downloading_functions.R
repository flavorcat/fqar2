#' Download FQA by id number
#'
#' Downloads a specified floristic quality assessment from \href{https://universalfqa.org/}{universalfqa.org}. ID numbers for assessments in various databases can be found using the \code{\link{download_fqa_assessments}} function.
#'
#' @param assessment_id A numeric identifier of the desired floristic quality assessment, as specified by \href{https://universalfqa.org/}{universalfqa.org}. ID numbers for specified databases can be viewed with the \code{\link{download_fqa_assessments}} function.
#'
#' @return An untidy data frame in the original format of the Universal FQA website. Use \code{\link{assessment_glance}} for a tidy summary and \code{\link{assessment_inventory}} for raw species-level data.
#'
#' @import jsonlite
#'
#' @examples \dontrun{
#' databases <- download_fqa_databases()
#' # Note database 1 is the original 1994 Chicago edition.
#' chicago_assessments <- download_fqa_assessments(1) # Edison dune and swale has id number 25002.
#' edison <- download_assessment(25002)
#' edison_tidy <- glance_fqa(edison)
#' edison_species <- assess_fqa(edison)
#' }
#'
#' @export
download_assessment <- function(assessment_id){
  if (!is.numeric(assessment_id)) {stop("assessment_id must be an integer.", call. = FALSE)}
  if (assessment_id %% 1 != 0) {stop("assessment_id must be an integer.", call. = FALSE)}
  path <- paste0("http://universalfqa.org/get/inventory/", assessment_id)
  list_data <- jsonlite::fromJSON(path)[[2]]
  if ((list_data[[1]] == "The requested assessment is not public") & (!is.na(list_data[[1]]))) {
    stop("The requested assessment is not public", call. = FALSE)
  }
  max_length <- max(unlist(lapply(list_data, length))) # determines how wide the df must be
  list_data <- lapply(list_data, function(x) {
    length(x) <- max_length
    unlist(x)
  })
  as.data.frame(do.call(rbind, list_data))
}



#' Download multiple assessments
#'
#' Searches a specified FQA database for assessments matching given criteria, then downloads all the matches from \href{https://universalfqa.org/}{universalfqa.org}. Download speeds from that website may be slow, causing delays in the evaluation of this function.
#'
#' @param database A numeric identifier of the desired floristic quality assessment database, as specified by \href{https://universalfqa.org/}{universalfqa.org}. Database id numbers can be viewed with the \code{\link{download_fqa_databases}} function.
#'
#' @param ... \code{dplyr}-style filtering criteria for the desired assessments. The following variables may be used:
#' \itemize{
#'   \item id (numeric)
#'   \item inventory (character)
#'   \item date (POSIXct)
#'   \item location (character)
#'   \item practitioner (character)
#' }
#'
#' @return A list of data frames matching the given search criteria. Each is an untidy data frame in the original format of the Universal FQA website. Use \code{\link{assessment_list_glance}} for a tidy summary.
#'
#' @import dplyr utils
#'
#' @examples
#' \dontrun{
#' databases <- download_fqa_databases # Note database 1 is the original 1994 Chicago edition.
#' chicago_assessments <- download_fqa_assessments(1) # lists all assessments in that database.
#' somme_assessments <- download_assessment_list(1, site == "Somme Woods")
#' somme_summary <- assessment_list_glance(somme_assessments)
#' }
#'
#' @export
download_assessment_list <- function(database, ...){
  tryCatch(inventories_summary <- download_fqa_assessments(database),
           error = function(e){stop("invalid database", call. = FALSE)})
  inventories_requested <- inventories_summary |> dplyr::filter(...)
  if (length(inventories_requested$id) >= 5){
    message("Downloading...")
    results <- list(0)
    pb <- utils::txtProgressBar(min = 0,
                         max = length(inventories_requested$id),
                         style = 3,
                         width = length(inventories_requested$id),
                         char = "=")
    for (i in seq_along(inventories_requested$id)) {
      results[[i]] <-  download_assessment(inventories_requested$id[i])
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  } else {
    results <- lapply(inventories_requested$id, download_assessment)
  }
  if (length(results) == 0) warning("No matches found. Empty list returned.", call. = FALSE)
  results
}



#' Download transect-level FQA data by id number
#'
#' Downloads a specified transect-level floristic quality assessment from \href{https://universalfqa.org/}{universalfqa.org}. ID numbers for assessments in different databases can be found using \code{\link{download_fqa_assessments}}.
#'
#' @param transect_id A numeric identifier of the desired transect-level floristic quality assessment, as specified by \href{https://universalfqa.org/}{universalfqa.org}. ID numbers for specified databases can be viewed with the \code{\link{download_fqa_transects}} function.
#'
#' @return An untidy data frame in the original format of the Universal FQA website. Use \code{\link{transect_glance}} for a tidy summary, \code{\link{transect_phys}} for a physiognometric overview, and \code{\link{transect_inventory}} for raw species-level data.
#'
#' @examples \dontrun{
#' databases <- download_fqa_databases # Note database 1 is the original 1994 Chicago edition.
#' chicago_transects <- download_fqa_transects(1) # CBG Sand prairie swale fen A has id number 5932.
#' cbg <- download_transect(5932)
#' cbg_tidy <- transect_glance(edison)
#' cbg_species <- transect_inventory(edison)
#' cbg_phys <- transect_phys(edison)
#' }
#'
#' @export
download_transect <- function(transect_id){
  if (!is.numeric(transect_id)) {stop("transect_id must be an integer.", call. = FALSE)}
  if (transect_id %% 1 != 0) {stop("transect_id must be an integer.", call. = FALSE)}
  path <- paste0("http://universalfqa.org/get/transect/", transect_id)
  list_data <- jsonlite::fromJSON(path)[[2]]
  if ((list_data[[1]] == "The requested assessment is not public") & (!is.na(list_data[[1]]))) {
    stop("The requested assessment is not public", call. = FALSE)
  }
  max_length <- max(unlist(lapply(list_data, length))) # determines how wide the df must be
  list_data <- lapply(list_data, function(x) {
    length(x) <- max_length
    unlist(x)
  })
  as.data.frame(do.call(rbind, list_data))
}



#' Download multiple transect-level FQAs
#'
#' This function searches a specified FQA database for transect-level assessments matching given criteria, then downloads all the matches from \href{https://universalfqa.org/}{universalfqa.org}. Download speeds from that website may be slow, causing delays in the evaluation of this function.
#'
#' @param database A numeric identifier of the desired floristic quality assessment database, as specified by \href{https://universalfqa.org/}{universalfqa.org}. Database id numbers can be viewed with the \code{\link{download_fqa_databases}} function.
#'
#' @param ... `dplyr`-style filtering criteria for the desired transect-level assessments. The following variables may be used:
#' \itemize{
#'   \item id (numeric)
#'   \item assessment (character)
#'   \item date (POSIXct)
#'   \item site (character)
#'   \item practitioner (character)
#' }
#'
#' @return A list of data frames matching the given search criteria. Each is an untidy data frame in the original format of the Universal FQA website. Use \code{\link{transect_list_glance}} for a tidy summary.
#'
#' @examples \dontrun{
#' databases <- download_fqa_databases()
#' # Note database 1 is the original 1994 Chicago edition.
#' chicago_transects <- download_fqa_transects(1)
#' fcna <- download_transect_list(site == "FCNA")
#' }
#'
#' @export
download_transect_list <- function(database, ...){
  tryCatch(transects_summary <- download_fqa_transects(database),
           error = function(e){stop("invalid database", call. = FALSE)})
  transects_requested <- transects_summary |> dplyr::filter(...)
  if (length(transects_requested$id) >= 5){
    message("Downloading...")
    results <- list(0)
    pb <- utils::txtProgressBar(min = 0,
                         max = length(transects_requested$id),
                         style = 3,
                         width = length(transects_requested$id),
                         char = "=")
    for (i in seq_along(transects_requested$id)) {
      results[[i]] <-  download_transect(transects_requested$id[i])
      utils::setTxtProgressBar(pb, i)
    }
    close(pb)
  } else {
    results <- lapply(transects_requested$id, download_transect)
  }
  if (length(results) == 0) warning("No matches found. Empty list returned.", call. = FALSE)
  results
}





