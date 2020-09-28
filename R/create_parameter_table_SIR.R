#' Create formatted table with SIR results
#'
#' The point is to automatically create a table with parameter values and their
#' CI calculated by SIR. Returns a list of tibbles of length 2, the first
#' element being the sir_results.csv content, the second element being the
#' formatted table. NB : Residual error parameters are given as variances
#'
#' @param sir_results_path character of length 1. Path to sir_results.csv file
#' @param sig_dig integer of length 1. Number of significant digits to be
#' reported in the table. Defaults to 3
#' @param interval_bounds numeric vector of length 2. Boundaries of the CI.
#' Default to c(0.025, 0.975)
#'
#' @return list of 2 tibbles
#' @export
#'
#' @examples
#' \dontrun{create_parameter_table_SIR("sir_results.csv")}
create_parameter_table_SIR <- function(sir_results_path,
                                       sig_dig = 3,
                                       interval_bounds = c(0.025, 0.975))
{
  library(readr)
  sir_results <- readr::read_csv(sir_results_path,
                          skip = 4) %>%
    dplyr::filter(X1 %in% c("center_estimate",
                     base::paste0(interval_bounds * 100, "%"))) %>%
    tidyr::pivot_longer(-X1) %>%
    tidyr::pivot_wider(name,
                names_from = X1) %>%
    dplyr::rename(
      lower_bound = base::paste0(interval_bounds[1] * 100, "%"),
      upper_bound = base::paste0(interval_bounds[2] * 100, "%")
    ) %>%
    readr::type_convert() %>%
    dplyr::mutate(
      relative_lower_bound = (lower_bound - center_estimate) / center_estimate,
      relative_upper_bound = (upper_bound - center_estimate) / center_estimate
    ) %>%
    dplyr::filter(!is.na(center_estimate)) %>%
    dplyr::rename(Parameter = name)

  report_table <- sir_results %>%
    dplyr::mutate(
      !!base::paste0(
        "Parameter [",
        (interval_bounds[2] - interval_bounds[1]) * 100,
        "%CI]"
      ) :=
        base::paste0(
          base::signif(center_estimate, sig_dig),
          " [",
          base::signif(lower_bound, sig_dig),
          " - ",
          base::signif(upper_bound, sig_dig),
          "]"
        ),
      !!base::paste0(
        "Parameter [",
        (interval_bounds[2] - interval_bounds[1]) * 100,
        "%CI-Relative Values]"
      ) :=
        base::paste0(
          base::signif(center_estimate, sig_dig),
          " [",
          base::signif(relative_lower_bound, sig_dig) * 100,
          "% - ",
          base::signif(relative_upper_bound, sig_dig) * 100,
          "%]"
        )
    ) %>%
    dplyr::select(
      Parameter,
      base::paste0(
        "Parameter [",
        (interval_bounds[2] - interval_bounds[1]) * 100,
        "%CI]"
      ),
      base::paste0(
        "Parameter [",
        (interval_bounds[2] - interval_bounds[1]) * 100,
        "%CI-Relative Values]"
      )
    )

  base::list(sir_results, report_table)
}
