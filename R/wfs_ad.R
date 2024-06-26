#' WFS INSPIRE: Download addresses
#'
#' @description
#' Get the spatial data of addresses The WFS Service allows to perform
#' several types of queries:
#' - By bounding box: Implemented on `catr_wfs_get_address_bbox()`.
#'   Extract objects included on the bounding box provided. See **Details**.
#'
#' @inheritParams catr_wfs_get_buildings_bbox
#'
#' @seealso [sf::st_bbox()]
#' @family INSPIRE
#' @family WFS
#' @family addresses
#' @family spatial
#'
#' @return A [`sf`][sf::st_sf] object.
#'
#' @references
#'
#' ```{r child = "man/chunks/wfspdf.Rmd"}
#' ```
#'
#' @details
#'
#' When `x` is a numeric vector, make sure that the `srs` matches the
#' coordinate values. Additionally, when the `srs` correspond to a geographic
#' reference system (4326, 4258), the function queries the bounding box on
#' [EPSG:3857](https://epsg.io/3857) - Web Mercator, to overcome
#' a potential bug on the API side.
#'
#' When `x` is a [`sf`][sf::st_sf] object, the value `srs` is ignored. In
#' this case, the bounding box of the [`sf`][sf::st_sf] object would be
#' used for the query (see [sf::st_bbox()]). The query is performed using
#' [EPSG:3857](https://epsg.io/3857) (Web Mercator). The result is provided
#' always in the SRS of the [`sf`][sf::st_sf] object provided as input.
#'
#' # API Limits
#'
#' The API service is limited to a bounding box of 4km2 and a maximum of 5.000
#' elements.
#'
#' @rdname catr_wfs_get_address
#' @export
catr_wfs_get_address_bbox <- function(x, srs, verbose = FALSE) {
  bbox_res <- wfs_bbox(x, srs)

  message_on_limit(bbox_res, 4)

  res <- wfs_api_query(
    entry = "wfsAD.aspx?",
    verbose = verbose,
    # WFS service
    service = "wfs",
    version = "2.0.0",
    request = "getfeature",
    typenames = "AD.ADDRESS",
    # Stored query
    bbox = bbox_res$bbox,
    SRSNAME = bbox_res$incrs
  )

  out <- wfs_results(res, verbose)

  if (!is.null(out)) {
    # Transform back to the desired srs
    out <- sf::st_transform(out, bbox_res$outcrs)
  }
  return(out)
}
#' @description
#' - By street code: Implemented on `catr_wfs_get_address_codvia()`. Extract
#'   objects of specific addresses.
#'
#' @param codvia Cadastral street code.
#' @param del Cadastral office code.
#' @param mun Cadastral municipality code.
#'
#' @rdname catr_wfs_get_address
#' @export
catr_wfs_get_address_codvia <- function(codvia, del, mun, srs = NULL,
                                        verbose = FALSE) {
  res <- wfs_api_query(
    entry = "wfsAD.aspx?",
    verbose = verbose,
    # WFS service
    service = "wfs",
    version = "2.0.0",
    request = "getfeature",
    StoredQuerie_id = "getadbycodvia",
    # Stored query
    codvia = codvia,
    del = del,
    mun = mun,
    SRSNAME = srs
  )

  out <- wfs_results(res, verbose)

  return(out)
}

#' @description
#' - By cadastral reference: Implemented on `catr_wfs_get_address_rc()`. Extract
#'   objects of specific cadastral references
#'
#' @inheritParams catr_wfs_get_buildings_rc
#'
#' @rdname catr_wfs_get_address
#' @export
catr_wfs_get_address_rc <- function(rc, srs = NULL, verbose = FALSE) {
  res <- wfs_api_query(
    entry = "wfsAD.aspx?",
    verbose = verbose,
    # WFS service
    service = "wfs",
    version = "2.0.0",
    request = "getfeature",
    StoredQuerie_id = "GetadByRefcat",
    # Stored query
    REFCAT = rc,
    SRSNAME = srs
  )

  out <- wfs_results(res, verbose)

  return(out)
}
#' @description
#' - By postal codes: Implemented on `catr_wfs_get_address_postalcode()`.
#'   Extract objects of specific cadastral references
#'
#'
#' @param postalcode Postal code.
#'
#' @rdname catr_wfs_get_address
#' @export
#' @examplesIf tolower(Sys.info()[["sysname"]]) != "linux"
#' \donttest{
#' ad <- catr_wfs_get_address_bbox(
#'   c(
#'     233673, 4015968, 233761, 4016008
#'   ),
#'   srs = 25830
#' )
#'
#' library(ggplot2)
#'
#' ggplot(ad) +
#'   geom_sf()
#' }
catr_wfs_get_address_postalcode <- function(postalcode, srs = NULL,
                                            verbose = FALSE) {
  res <- wfs_api_query(
    entry = "wfsAD.aspx?",
    verbose = verbose,
    # WFS service
    service = "wfs",
    version = "2.0.0",
    request = "getfeature",
    StoredQuerie_id = "getadbypostalcode",
    # Stored query
    postalcode = postalcode,
    SRSNAME = srs
  )

  out <- wfs_results(res, verbose)

  return(out)
}
