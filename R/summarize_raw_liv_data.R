#' Create parametric data summary from LIV sweeps
#'
#' @param df_liv A data frame containing LIV sweep data
#' @param If_vec A numeric vector of bias currents
#' @param Ix_vec A numeric vector of bias currents relative to threshold
#' @param Pop_vec A numeric vector of operating power values
#' @param Ik1 A numeric value for the start of the kink analysis span (mA)
#' @param Ik2 A numeric value for the end of the kink analysis span (mA)
#' @param n1_smooth P-I smooth span
#' @param n2_smooth P-I first-derivative smooth span
#' @param n3_smooth P-I second_derivative smooth span
#'
#' @return A data frame containing parametric data
#' @export
#'
#' @examples
summarize_raw_liv_data = function(df_liv, If_vec, Ix_vec, Pop_vec, Ik1 = 30, Ik2 = 140, n1_smooth = 1, n2_smooth = 1, n3_smooth = 1) {

  # moving average smoothing
  df_liv = df_liv |> dplyr::mutate(power_sm = my_smooth(.data$power, n1_smooth))

  # parameters that don't depend on If or Ix
  df_summary_liv0 = df_liv |>
    dplyr::summarize(
      n1_smooth = n1_smooth,
      n2_smooth = n2_smooth,
      n3_smooth = n3_smooth,
      Ith1d = extract_ith_from_pi(.data$current, .data$power, n1_smooth, n2_smooth)[[1]] / 1e-3,
      Ith2d = extract_ith_from_pi(.data$current, .data$power, n1_smooth, n2_smooth, n3_smooth)[[2]] / 1e-3 ,
      Pth = extract_pf_from_pi(.data$current, .data$power_sm, I0 = .data$Ith1d*1e-3) /1e-3,
      Vth = extract_vf_from_vi(.data$current, .data$voltage, I0 = .data$Ith1d*1e-3),
      Rdth = extract_rs_from_vi(.data$current, .data$voltage, I0 = .data$Ith1d*1e-3),
      Pmax = base::max(.data$power_sm, na.rm = TRUE) / 1e-3,
      Isat = extract_isat_from_pi(.data$current, .data$power) / 1e-3,
      Ik1 = Ik1,
      Ik2 = Ik2,
      df_kink = extract_kink_from_pi(.data$current, .data$power, Istart = Ik1*1e-3, Istop = Ik2*1e-3)
    ) |>
    tidyr::unpack(cols = .data$df_kink) |>
    dplyr::mutate(Ikink = .data$Ikink / 1e-3) |>
    dplyr::ungroup()

  # parameters specified at absolute bias currents in If_vec
  df_summary_liv_If_long = df_liv |>
    dplyr::reframe(
      If_index = seq_along(If_vec),
      If = If_vec / 1e-3,
      Pf = purrr::map_dbl(If_vec, \(x) extract_pf_from_pi(.data$current, .data$power, I0 = x)) / 1e-3,
      SE = purrr::map_dbl(If_vec, \(x) extract_se_from_pi(.data$current, .data$power, I0 = x)),
      Vf = purrr::map_dbl(If_vec, \(x) extract_vf_from_vi(.data$current, .data$voltage, I0 = x)),
      Rs = purrr::map_dbl(If_vec, \(x) extract_rs_from_vi(.data$current, .data$voltage, I0 = x))
    )

  df_summary_liv_If = df_summary_liv_If_long |>
    tidyr::pivot_wider(
      names_from = .data$If_index,
      values_from = c(.data$If, .data$Pf, .data$SE, .data$Vf, .data$Rs),
      names_sep = ""
    )

  # parameters specified at bias currents relative to threshold (Ix_vec)
  df_summary_liv_Ix_long = df_liv |>
    dplyr::reframe(
      Ix_index = seq_along(Ix_vec),
      Ix = Ix_vec / 1e-3,
      Ith1d = extract_ith_from_pi(.data$current, .data$power, n1_smooth, n2_smooth)[[1]] / 1e-3,
      Px = purrr::map_dbl(Ix_vec, \(x) extract_pf_from_pi(.data$current, .data$power, I0 = .data$Ith1d*1e-3 + x)) / 1e-3,
      SEx = purrr::map_dbl(Ix_vec, \(x) extract_se_from_pi(.data$current, .data$power, I0 = .data$Ith1d*1e-3 + x)),
      Vx = purrr::map_dbl(Ix_vec, \(x) extract_vf_from_vi(.data$current, .data$voltage, I0 = .data$Ith1d*1e-3 + x))
    ) |>
    dplyr::select(-"Ith1d") |>
    dplyr::ungroup()

  df_summary_liv_Ix = df_summary_liv_Ix_long |>
    tidyr::pivot_wider(
      names_from = .data$Ix_index,
      values_from = c(.data$Ix, .data$Px, .data$SEx, .data$Vx),
      names_sep = ""
    )

  # Pop parameters
  df_summary_liv_Pop_long = df_liv |>
    dplyr::reframe(
      Pop_index = seq_along(Pop_vec),
      Pop = Pop_vec / 1e-3,
      Iop = purrr::map_dbl(Pop_vec, \(x) extract_iop_from_pi(.data$current, .data$power, Pop = x)) / 1e-3,
      SEop = purrr::map_dbl(.data$Iop * 1e-3, \(x) extract_se_from_pi(.data$current, .data$power, I0 = x)),
      Vop = purrr::map_dbl(.data$Iop * 1e-3, \(x) extract_vf_from_vi(.data$current, .data$voltage, I0 = x))
    )

  df_summary_liv_Pop = df_summary_liv_Pop_long |>
    tidyr::pivot_wider(
      names_from = .data$Pop_index,
      values_from = c(.data$Pop, .data$Iop, .data$SEop, .data$Vop),
      names_sep = ""
    )

  # Join all the parameters
  df_summary_liv = df_summary_liv0 |>
    dplyr::left_join(df_summary_liv_If) |>
    dplyr::left_join(df_summary_liv_Ix) |>
    dplyr::left_join(df_summary_liv_Pop)

  return(df_summary_liv)
}
