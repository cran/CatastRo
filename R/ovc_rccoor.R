#' OVCCoordenadas: Reverse geocode a cadastral reference
#'
#' @description
#' Implementation of the OVCCoordenadas service
#' [Consulta RCCOOR](`r ovcurl("RCCOOR")`).
#'
#' Return the cadastral reference found of a set of specific coordinates.
#'
#' @references
#' [Consulta RCCOOR](`r ovcurl("RCCOOR")`).
#'
#' @family OVCCoordenadas
#' @family cadastral references
#' @seealso [catr_srs_values], `vignette("ovcservice", package = "CatastRo")`
#' @inheritParams catr_ovc_get_rccoor_distancia
#'
#' @return A [`tibble`][tibble::tibble]. See **Details**
#'
#' @export
#'
#' @details
#'
#' When the API does not provide any result, the function returns a
#' [`tibble`][tibble::tibble] with the input parameters only.
#'
#' On a successful query, the function returns a [`tibble`][tibble::tibble] with
#' one row by cadastral reference, including the following columns:
#' * `geo.xcen`, `geo.ycen`, `geo.srs`: Input parameters of the query.
#' * `refcat`: Cadastral Reference.
#' * `address`: Address as it is recorded on the Cadastre.
#' * Rest of fields: Check the API Docs.
#'
#' @examplesIf tolower(Sys.info()[["sysname"]]) != "linux"
#' \donttest{
#' catr_ovc_get_rccoor(
#'   lat = 38.6196566583596,
#'   lon = -3.45624183836806,
#'   srs = 4326
#' )
#' }
catr_ovc_get_rccoor <- function(lat, lon, srs = 4326, verbose = FALSE) {
  # Sanity checks
  valid_srs <- CatastRo::catr_srs_values
  valid_srs <- tibble::as_tibble(valid_srs)
  valid_srs <- valid_srs[valid_srs$ovc_service == TRUE, "SRS"]
  valid <- tibble::deframe(valid_srs)
  valid <- as.character(valid)

  if (!as.character(srs) %in% valid) {
    stop(
      "'srs' for OVC should be one of ",
      paste0("'", valid, "'", collapse = ", "),
      ".\n\nSee CatastRo::catr_srs_values"
    )
  }

  srs <- paste0("EPSG:", srs)

  # Prepare query
  ##  Build url
  api_entry <- paste0(
    "http://ovc.catastro.meh.es/ovcservweb/",
    "OVCSWLocalizacionRC/OVCCoordenadas.asmx/Consulta_RCCOOR"
  )

  query <- list(
    SRS = srs,
    Coordenada_X = lon,
    Coordenada_Y = lat
  )

  ## GET
  url <- httr2::url_parse(api_entry)
  url$query <- query
  url <- httr2::url_build(url)

  if (verbose) {
    message("Querying url:\n\t", url)
  }

  api_res <- httr2::request(url)
  api_res <- httr2::req_perform(api_res)

  # Check error on status
  httr2::resp_check_status(api_res)

  # Extract results
  content <- httr2::resp_body_xml(api_res)
  content_list <- xml2::as_list(content)

  # Check API custom error
  err <- content_list[["consulta_coordenadas"]]



  if (("lerr" %in% names(err))) {
    df <- tibble::as_tibble_row(unlist(err["lerr"]))

    message("Error code: ", df[1, 1], ". ", df[1, 2])

    empty <- tibble::tibble(
      a = lat,
      b = lon,
      srs = srs
    )

    names(empty) <- c("geo.xcen", "geo.ycen", "geo.srs")
    return(empty)
  }

  res <- content_list[["consulta_coordenadas"]][["coordenadas"]][["coord"]]

  # Get info of the query
  overall <- tibble::as_tibble_row(unlist(res))

  # Extract helper info
  rc_help <- tibble::tibble(
    refcat = paste0(overall$pc.pc1, overall$pc.pc2),
    address = overall$ldt
  )

  # Join all

  out <- dplyr::bind_cols(
    rc_help,
    overall
  )

  # Numeric
  out["geo.xcen"] <- as.numeric(out[["geo.xcen"]])
  out["geo.ycen"] <- as.numeric(out[["geo.ycen"]])

  return(out)
}
