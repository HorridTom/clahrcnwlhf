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

  #TODO: consider refactoring these two apply calls into one.

  bundles_prv_nxt_spells <- apply(bundles, 1, function(x) {

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

    # Identify the first subsequent admission for this patient
    # after the bundle admission datetime
    pt_eps <- episodes[episodes$PseudoID == pt_id,]
    pt_eps <- pt_eps[as.POSIXct(pt_eps$CSPAdmissionTime) > as.POSIXct(bun_dt),]
    if(nrow(pt_eps)==0) {nxt_adm <- NA} else {
      nxt_adm <- pt_eps[which.min(as.POSIXct(pt_eps$CSPAdmissionTime)),"spell_number"]
    }
    list('prev.spell'=prv_adm, 'next.spell'=nxt_adm)

  })

  bundles <- cbind(bundles, do.call(rbind.data.frame, bundles_prv_nxt_spells))

  bundles_lags <- apply(bundles, 1, function(x) {

    p_sp_id <- as.numeric(trimws(x["prev.spell"],which = "both"))
    n_sp_id <- as.numeric(trimws(x["next.spell"],which = "both"))
    bun_dt <- x["Admission.Datetime"]

    # Get lag from previous admission
    if (is.na(p_sp_id)) {
      lfpa <- NA
    } else {

      spell.start <- episodes[episodes$spell_number == p_sp_id & episodes$new_spell == TRUE,"CSPAdmissionTime"]
      spell.end <- episodes[episodes$spell_number == p_sp_id & episodes$new_spell == TRUE,"CSPDischargeTime"]
      lfpa <- difftime(as.POSIXct(bun_dt), as.POSIXct(spell.start), units = "days")
    }

    # Get lag to next admission
    if (is.na(n_sp_id)) {
      ltna <- NA
    } else {

      spell.start <- episodes[episodes$spell_number == n_sp_id & episodes$new_spell == TRUE,"CSPAdmissionTime"]
      spell.end <- episodes[episodes$spell_number == n_sp_id & episodes$new_spell == TRUE,"CSPDischargeTime"]
      ltna <- difftime(as.POSIXct(spell.start), as.POSIXct(bun_dt), units = "days")
    }

    list('lag.from.prev.adm'=lfpa, 'lag.to.next.adm'=ltna)
    #lfpa
  })

  bundles <- cbind(bundles, do.call(rbind.data.frame, bundles_lags))

  bundles$lag.from.prev.adm <- as.difftime(bundles$lag.from.prev.adm, units = "days")

  bundles$lag.to.next.adm <- as.difftime(bundles$lag.to.next.adm, units = "days")

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


plot_lag_dist <- function(bundles = clahrcnwlhf::bundle_data_clean, episodes = clahrcnwlhf::emergency_adms, bis = NULL, prev = TRUE, cumulative = FALSE) {

  if (is.null(bis)) {
    bis <- clahrcnwlhf::bundle_in_spell(bundles = bundles, episodes = episodes)
  }

  if (prev) {
    p <- ggplot(bis, aes(lag.from.prev.adm)) + facet_wrap(~bundle.in.spell, scales = "free_x")
  } else {
    p <- ggplot(bis, aes(lag.to.next.adm)) + facet_wrap(~bundle.in.spell, scales = "free_x")
  }

  if (cumulative) {
    p <- p + stat_ecdf()
  } else {
    p <- p + geom_histogram()
  }
  p
}


link_diag_table <- function(bis, lag_cutoff = 3) {

  table(bis$bundle.in.spell, bis$lag.to.next.adm <= as.difftime(lag_cutoff, units = "days"), useNA = "always")

}
