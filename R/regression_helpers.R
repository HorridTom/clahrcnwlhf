# Regression helpers

lmer_results_table <- function(model) {

  if(class(model) == "glmmTMB") {

    coefs <- coef(summary(model))$cond
    confints <- confint(model, method = "uniroot")

  } else {

    if(FALSE) {
      coefs <- coef(summary(as(model, "merModLmerTest")))
    } else {
      coefs <- coef(summary(model))
    }
    confints <- confint(model)
  }
  coefs_rownames <- rownames(coefs)
  pretable <- tibble::as_tibble(coefs) %>%
    dplyr::mutate(coeff = coefs_rownames)

  confints_rownames <- rownames(confints)
  confints <- tibble::as_tibble(confints) %>%
    dplyr::mutate(coeff = confints_rownames)

  results_table <- confints %>%
    dplyr::select(coeff, "2.5 %", "97.5 %") %>%
    dplyr::left_join(pretable, by = c("coeff" = "coeff")) %>%
    dplyr::select(coeff, Estimate, ci_low = "2.5 %", ci_high = "97.5 %", everything()) %>%
    dplyr::mutate(Estimate = exp(Estimate),
           ci_low = exp(ci_low),
           ci_high = exp(ci_high))

  return(results_table)
}


plot_its <- function(model,
                     model_data,
                     add_term = TRUE,
                     all_preds = FALSE,
                     distinct_periods = FALSE,
                     average_covariates = TRUE,
                     los_model = FALSE) {

  if(!all_preds) {
    acts_preds <- get_prediction_output(model = model,
                                        model_data = model_data,
                                        add_term = add_term,
                                        distinct_periods = distinct_periods,
                                        average_covariates = average_covariates,
                                        los_model = los_model)
  } else {
    acts_preds_overall <- get_prediction_output(model = model,
                                                model_data = model_data,
                                                add_term = add_term,
                                                distinct_periods = FALSE,
                                                average_covariates = TRUE,
                                                los_model = los_model)

    acts_preds_prepost <- get_prediction_output(model = model,
                                                model_data = model_data,
                                                add_term = add_term,
                                                distinct_periods = TRUE,
                                                average_covariates = TRUE,
                                                los_model = los_model)

    acts_preds_monthly <- get_prediction_output(model = model,
                                                model_data = model_data,
                                                add_term = add_term,
                                                distinct_periods = TRUE,
                                                average_covariates = FALSE,
                                                los_model = los_model)
    #browser()

    acts_preds <- acts_preds_overall %>%
      dplyr::left_join(acts_preds_prepost %>%
                  dplyr::select(id_time,
                         pred_rate_prepost = pred_rate),
                by = c("id_time" = "id_time")) %>%
      dplyr::left_join(acts_preds_monthly %>%
                  dplyr::select(id_time,
                         pred_rate_monthly = pred_rate),
                by = c("id_time" = "id_time"))
  }

  if(los_model) {

    acts_preds <- acts_preds %>%
      mutate(act_rate = exp(act_rate))

  }

  # Plot the chart
  if(!all_preds) {
    plt <- acts_preds %>%
      dplyr::select(id_time, act_rate, pred_rate) %>%
      ggplot2::ggplot(ggplot2::aes(x=id_time)) +
      ggplot2::geom_line(ggplot2::aes(y=pred_rate), colour = "blue") +
      ggplot2::geom_point(ggplot2::aes(y=act_rate), colour = "black")
  } else {
    plt <- acts_preds %>%
      dplyr::select(id_time, act_rate, pred_rate, pred_rate_prepost, pred_rate_monthly) %>%
      ggplot2::ggplot(ggplot2::aes(x=id_time)) +
      ggplot2::geom_line(ggplot2::aes(y=pred_rate), colour = "cornflowerblue") +
      ggplot2::geom_line(ggplot2::aes(y=pred_rate_prepost), colour = "blue") +
      ggplot2::geom_line(ggplot2::aes(y=pred_rate_monthly), colour = "darkslategrey") +
      ggplot2::geom_point(ggplot2::aes(y=act_rate), colour = "black")
  }

  return(plt)
}

get_prediction_output <- function(model,
                                  model_data,
                                  add_term,
                                  distinct_periods,
                                  average_covariates,
                                  los_model) {

  # Create new dataset with averaged values of all covariates
  data_new <- if(distinct_periods) {
    model_data %>%
      group_by(X_x) } else {
        model_data
      }

  if(average_covariates) {
    data_new <- data_new %>%
      mutate(sexM = mean(sexM), non_white1 = mean(non_white1), total_HF = mean(total_HF),
             ln_age = mean(ln_age),
             month_1 = mean(month_1), month_2 = mean(month_2), month_3 = mean(month_3),
             month_4 = mean(month_4),month_5 = mean(month_5), month_6 = mean(month_6),
             month_7 = mean(month_7), month_8 = mean(month_8), month_9 = mean(month_9),
             month_10 = mean(month_10), month_11 = mean(month_11),
             ward_2 = mean(ward_2), ward_3 = mean(ward_3), ward_4 = mean(ward_4),
             ward_5 = mean(ward_5))
  }

  data_new <- data_new %>% ungroup()

  term <- if(add_term) {0.5*(as.data.frame(VarCorr(model))[1, "sdcor"])^2} else {0}

  # Run model predictions on this averaged data
  if(("glmerMod" %in% class(model) |
      "lmerModLmerTest" %in% class(model)) & !los_model) {
    prediction_output <- predict(model,
                                 newdata = data_new,
                                 type = "link",
                                 allow.new.levels = TRUE,
                                 re.form=~0
    ) + term
  } else if(("glmerMod" %in% class(model) |
             "lmerModLmerTest" %in% class(model)) & los_model) {
    prediction_output <- predict(model,
                                 newdata = data_new,
                                 type = "response",
                                 allow.new.levels = TRUE,
                                 re.form=~0
    ) + term
  } else {
    prediction_output <- predict(model,
                                 newdata = data_new,
                                 type = "link"
    )
  }

  # Add the predicted outcomes as a column to the averaged dataset,
  # and summarise this data by month.
  # Create a new column for the actual monthly readmission rate
  acts_preds_raw <- data_new %>%
    cbind(prediction_output)

  outcome <- all.vars(formula(model))[1]

  if(!los_model) {
    acts_preds <- acts_preds_raw %>%
      group_by(id_time) %>%
      summarise(act = sum(.data[[outcome]]),
                total_HF = n(),
                pred = sum(exp(prediction_output))) %>%
      mutate(act_rate = act/total_HF,
             pred_rate = pred/total_HF
      )
  } else {
    acts_preds <- acts_preds_raw %>%
      group_by(id_time) %>%
      summarise(act = sum(.data[[outcome]]),
                total_HF = n(),
                pred = mean(prediction_output)) %>%
      mutate(act_rate = act/total_HF,
             pred_rate = exp(pred))
  }
  return(acts_preds)

}

