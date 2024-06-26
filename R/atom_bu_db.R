#' ATOM INSPIRE: Reference database for ATOM buildings
#'
#' @description
#'
#' Create a database containing the urls provided in the INSPIRE ATOM service
#' of the Spanish Cadastre for extracting buildings.
#'
#' - `catr_atom_get_buildings_db_all()` provides a top-level table including
#'    information of all the territorial offices (except Basque Country and
#'    Navarre) listing the municipalities included on each office.
#' - `catr_atom_get_buildings_db_to()` provides a table for the specified
#'    territorial office including information for each of the municipalities
#'    of that office.
#'
#'
#' @source
#' <https://www.catastro.hacienda.gob.es/INSPIRE/buildings/ES.SDGC.BU.atom.xml>
#'
#' @family INSPIRE
#' @family ATOM
#' @family buildings
#' @family databases
#'
#' @inheritParams catr_atom_get_address_db_all
#' @inheritParams catr_set_cache_dir
#'
#'
#' @rdname catr_atom_get_buildings_db
#' @export
#'
#' @return
#' A [`tibble`][tibble::tibble] with the information requested.
#' - `catr_atom_get_buildings_db_all()` includes the following fields:
#'   - `territorial_office`: Territorial office, corresponding to each province
#'      of Spain expect Basque Country and Navarre.
#'   - `url`: ATOM url for the corresponding territorial office.
#'   - `munic`: Name of the municipality.
#'   - `date`: Reference date of the data. Note that **the information of
#'      this service is updated twice a year**.
#' - `catr_atom_get_buildings_db_to()` includes the following fields:
#'   - `munic`: Name of the municipality.
#'   - `url`: url for downloading information of the corresponding municipality.
#'   - `date`: Reference date of the data. Note that **the information of
#'      this service is updated twice a year**.
#'
#' @examples
#' \donttest{
#' catr_atom_get_buildings_db_all()
#' }
catr_atom_get_buildings_db_all <- function(cache = TRUE,
                                           update_cache = FALSE,
                                           cache_dir = NULL,
                                           verbose = FALSE) {
  api_entry <- paste0(
    "https://www.catastro.hacienda.gob.es/INSPIRE/",
    "buildings/ES.SDGC.BU.atom.xml"
  )


  filename <- basename(api_entry)

  path <- catr_hlp_dwnload(
    api_entry, filename, cache_dir,
    verbose, update_cache, cache
  )


  tbl <- catr_read_atom(path, top = TRUE)
  names(tbl) <- c("territorial_office", "url", "munic", "date")

  return(tbl)
}
#' @rdname catr_atom_get_buildings_db
#' @name catr_atom_get_buildings_to
#' @export
catr_atom_get_buildings_db_to <- function(to,
                                          cache = TRUE,
                                          update_cache = FALSE,
                                          cache_dir = NULL,
                                          verbose = FALSE) {
  all <- catr_atom_get_buildings_db_all()
  alldist <- unique(all[, c("territorial_office", "url")])

  # Escape parenthesis
  to <- gsub("\\(|\\)", "", to)
  allto <- gsub("\\(|\\)", "", alldist$territorial_office)

  findto <- grep(to, allto, ignore.case = TRUE)[1]

  if (is.na(findto)) {
    message("No Territorial office found for ", to)
    return(invisible(NA))
  }

  tb <- alldist[findto, ]

  if (verbose) {
    message(
      "Extracting information for ",
      tb$territorial_office
    )
  }

  api_entry <- as.character(tb$url)
  filename <- basename(api_entry)
  path <- catr_hlp_dwnload(
    api_entry, filename, cache_dir,
    verbose, update_cache, cache
  )


  tbl <- catr_read_atom(path, top = FALSE)

  names(tbl) <- c("munic", "url", "date")

  return(tbl)
}
