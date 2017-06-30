#' link_bundles
#'
#' @param bundles the bundle data
#' @param episodes the episode data
#'
#' @return dataframe with new column linked.spell containing spell_number of the spell associated with this bundle
#' @export
#'
link_bundles <- function(bundles, episodes) {
  bundles$linked.spell <- NA
  bundles
}


prev_spell <- function(bundles, episodes) {



  bundles$prev.spell <- apply(bundles, 1, function(x) {
    pt_id <- x["PseudoID"]
    bun_dt <- x["Admission.Datetime"]

    pt_eps <- episodes[episodes$PseudoID == pt_id,]
    pt_eps <- pt_eps[as.POSIXct(pt_eps$CSPAdmissionTime) <= as.POSIXct(bun_dt),]

    pt_eps[which.max(as.POSIXct(pt_eps$CSPAdmissionTime)),"spell_number"]
  })
  bundles
}
