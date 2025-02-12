
#' Read ARU deployment data from Google Sheets
#'
#' @param google_sheet_url
#'
#' @return
#' @export
#'
#' @examples

read_aru_deployments <- function(google_sheet_url) {

  googlesheets4::read_sheet(google_sheet_url) |>
    janitor::clean_names()

}
