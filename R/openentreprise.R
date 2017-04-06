#' Get Openentreprise
#'
#' Get data from OpenEntreprise API : https://www.openentreprise.fr/swagger-ui.html
#'
#' @param .siren a valid siren number
#'
#' @return a list
#' @export
#'
#' @examples
#' get_openentreprise(.siren = "533735932")
#'
get_openentreprise <- function(.siren) {
  request <- httr::GET(
    url = paste0(
      "https://www.openentreprise.fr/api/companies/",
      .siren)
  )

  if (httr::status_code(request) == "200") {
    return(httr::content(request))
  }
  else {
    message(httr::status_code(request))
  }
}


#' Extract financial data
#'
#' Extract financial data from openentreprise API
#'
#' @param x a request to the open entreprise API
#'
#' @return a tibble with operating income and revenue
#' @export
#'
#' @examples
#' extract_financialdata(x = get_openentreprise(.siren = "533735932"))
#'
#' extract_financialdata(get_openentreprise(.siren = "339507394"))
#'
extract_financialdata <- function(x) {

  financialdata <- x %>%
    magrittr::extract2("company") %>%
    magrittr::extract2("financialData")

  if (is.null(financialdata) == TRUE) {
    message("Financial data aren't available")

    tibble::tibble(
      date = as.Date(integer(0), origin = "1970-01-01"),
      revenue = numeric(0),
      operatingincome = numeric(0)
    )

  }
  else {
    tibble::tibble(
      date = lubridate::ymd(purrr::map_chr(
        .x = purrr::map(.x = financialdata, "date"),
        .f = function(x) {
          as.character(
            as.Date(
              as.POSIXct(x / 1000, origin = "1970-01-01", tz = "GMT")
            )
          )
        })),
      revenue = purrr::map_dbl(.x = financialdata, "revenue"),
      operatingincome = purrr::map_dbl(.x = financialdata, "operatingIncome")
    )
  }

}

#' Extract URL
#'
#' extract the URL of the webpage
#'
#' @param x a request to open API
#'
#' @return an URL as a string
#' @export
#'
#' @examples
#'
#' extract_url(x = get_openentreprise(.siren = "533735932"))
#'
#' \dontrun{
#' extract_url(x = get_openentreprise(.siren = "533735932")) %>%
#' browseURL()
#' }
#'
extract_url <- function(x) {

  seo <- magrittr::extract2(x, "seo")

  paste0(
    "https://www.openentreprise.fr/",
    magrittr::extract2(seo, "normalizedNaf"),
    "/",
    magrittr::extract2(
      magrittr::extract2(x, "company"),
      "id"
    ),
    "/",
    magrittr::extract2(seo, "normalizedName")
  )

}

#' Extract BODACC
#'
#' extract bodacc infos from openentreprise
#'
#' @param x a get request to openentreprise
#'
#' @return a tibble
#' @export
#'
#' @examples
#' extract_bodacc(x = get_openentreprise(.siren = "533735932"))
#'
#'
extract_bodacc <- function(x) {

  bodacc <- magrittr::extract2(
    magrittr::extract2(x, "company"),
    "bodacc")

  tibble::tibble(
    date = lubridate::ymd(purrr::map_chr(
      .x = purrr::map(.x = bodacc, "date"),
      .f = function(x) {
        as.character(
          as.Date(
            as.POSIXct(x / 1000, origin = "1970-01-01", tz = "GMT")
          )
        )
      })),
    type = purrr::map_chr(.x = bodacc, "type"),
    label = purrr::map_chr(.x = bodacc, "label"),
    url = purrr::map_chr(.x = bodacc, "url")
  )
}