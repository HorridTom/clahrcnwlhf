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


nearest_spells <- function(bundles, episodes) {

  bundles <- bundles[!is.na(bundles$Admission.Datetime),]

  #TODO: refactor these four "apply" calls into one

  bundles$prev.spell <- apply(bundles, 1, function(x) {

    # Extract the patient id and admission datetime for this bundle
    pt_id <- x["PseudoID"]
    bun_dt <- x["Admission.Datetime"]

    # Identify the most recent admission for this patient
    # prior to or on the bundle admission datetime
    pt_eps <- episodes[episodes$PseudoID == pt_id,]
    pt_eps <- pt_eps[as.POSIXct(pt_eps$CSPAdmissionTime) <= as.POSIXct(bun_dt),]
    if(nrow(pt_eps)==0) {prv_adm <- NA} else {
      prv_adm <- pt_eps[which.max(as.POSIXct(pt_eps$CSPAdmissionTime)),"spell_number"]
    }
    prv_adm

  })

  bundles$next.spell <- apply(bundles, 1, function(x) {

    # Extract the patient id and admission datetime for this bundle
    pt_id <- x["PseudoID"]
    bun_dt <- x["Admission.Datetime"]

    # Identify the first subsequent admission for this patient
    # after the bundle admission datetime
    pt_eps <- episodes[episodes$PseudoID == pt_id,]
    pt_eps <- pt_eps[as.POSIXct(pt_eps$CSPAdmissionTime) > as.POSIXct(bun_dt),]
    if(nrow(pt_eps)==0) {nxt_adm <- NA} else {
      nxt_adm <- pt_eps[which.min(as.POSIXct(pt_eps$CSPAdmissionTime)),"spell_number"]
    }
    nxt_adm
  })

  bundles$lag.from.prev.adm <- apply(bundles, 1, function(x) {

    sp_id <- as.numeric(trimws(x["prev.spell"],which = "both"))
    bun_dt <- x["Admission.Datetime"]

    if (is.na(sp_id)) {
      lfpa <- NA
    } else {

      spell.start <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPAdmissionTime"]
      spell.end <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPDischargeTime"]
      lfpa <- as.POSIXct(bun_dt) - as.POSIXct(spell.start)
    }
    lfpa
  })

  bundles$lag.to.next.adm <- apply(bundles, 1, function(x) {

    sp_id <- as.numeric(trimws(x["next.spell"],which = "both"))
    bun_dt <- x["Admission.Datetime"]

    if (is.na(sp_id)) {
      ltna <- NA
    } else {

      spell.start <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPAdmissionTime"]
      spell.end <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPDischargeTime"]
      ltna <- as.POSIXct(spell.start) - as.POSIXct(bun_dt)
    }
    ltna
  })

  bundles
}


bundle_in_spell <- function(bundles, episodes = clahrcnwlhf::emergency_adms) {

  bundles <- nearest_spells(bundles = bundles, episodes = episodes)

  bundles <- bundles[!is.na(bundles$Admission.Datetime),]

  bundles$bundle.in.spell <- apply(bundles, 1, function(x) {
    # Extract the patient id and admission datetime for this bundle
    sp_id <- as.numeric(trimws(x["prev.spell"],which = "both"))
    bun_dt <- x["Admission.Datetime"]

    # If there is no previous spell, return NA
    if (is.na(sp_id)) {
      bis <- NA
    } else {
      # Otherwise, establish whether or not the Admission.Datetime from the bundle
      # lies inside the spell
      spell.start <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPAdmissionTime"]
      spell.end <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPDischargeTime"]
      bis <- (as.POSIXct(spell.start) <= as.POSIXct(bun_dt)) && (as.POSIXct(bun_dt) <= as.POSIXct(spell.end))
    }
    bis
  })

  bundles

}


bundle_spell_lags <- function(bundles, episodes = clahrcnwlhf::emergency_adms) {

  bundles$lag.from.prev.adm <- NA

  bundles$lag.from.prev.adm <- apply(bundles, 1, function(x) {

    sp_id <- as.numeric(trimws(x["prev.spell"],which = "both"))
    bun_dt <- x["Admission.Datetime"]

    if (is.na(sp_id)) {
      lfpa <- NA
    } else {

      spell.start <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPAdmissionTime"]
      spell.end <- episodes[episodes$spell_number == sp_id & episodes$new_spell == TRUE,"CSPDischargeTime"]
      lfpa <- as.POSIXct(bun_dt) - as.POSIXct(spell.start)
    }
    lfpa
  })

  bundles

}



plot_lag_dist <- function(bundles = clahrcnwlhf::bundle_data_clean, episodes = clahrcnwlhf::emergency_adms, prev = TRUE) {


  if (prev) {
    p <- ggplot(bis, aes(lag.from.prev.adm)) + geom_histogram() + facet_wrap(~bundle.in.spell, scales = "free_x")
  } else {
    p <- ggplot(bis, aes(lag.to.next.adm)) + geom_histogram() + facet_wrap(~bundle.in.spell, scales = "free_x")
  }

  p
}
