#' Fix parameters and overwrite the model with the updated fixed parameters
#'
#' What the title says. A model file and a character vector of parameters to be
#' fixed has to be provided
#'
#' @param dir_input_files character of length 1. Contains path to directory containing the
#' input model files. Defaults to "".
#' @param input_model_files character vector of length 1 or n equal to the
#' number of input files. Contains name(s) of model(s) to be changed with some
#' fixed parameters
#' @param parameters_to_fix character of length 1 or p equal to the number of
#' parameters to be fixed. Contains the names of the parameters to be fixed.
#' @param init_values_to_fix character of length 1 or p equal to the number of
#' parameters to be fixed. Contains the initial values of the parameters to be
#' fixed.
#'
#' @return New .mod files created
#' @export
#'
#' @examples
#' #' \dontrun{fix_parameters_and_replace_model("../NONMEM/","run1.mod",c("KG","EMAX"))}
fix_parameters_and_replace_model <- function(dir_input_files,
                                               input_model_files,
                                               parameters_to_fix,
                                               init_values_to_fix)
{
  pharmpy <- reticulate::import("pharmpy")
  modeling <- reticulate::import("pharmpy.modeling")
  pharmpy$plugins$nonmem$conf$parameter_names <- "comment"
  fix_parameter <- modeling$fix_parameters
  unfix_parameter <- modeling$unfix_parameters

  single_model_function <- function(dir_input_file,
                                    input_model_file,
                                    parameters_to_fix_single,
                                    init_values_to_fix_single)
  {
    model <- pharmpy$Model(paste0(dir_input_file, input_model_file))
    purrr::walk(parameters_to_fix_single,  ~ fix_parameter(model, .x))
    purrr::walk(parameters_to_fix_single,  ~ usethis::ui_info((base::paste0("Fixed ", .x))))
    purrr::walk2(parameters_to_fix_single,init_values_to_fix_single,
      function(x,y) model$parameters$inits[[as.character(x)]] <- y)
    modeling$update_source(model)
    model$write(paste0(dir_input_file, input_model_file),
                force = T)
    usethis::ui_info(base::paste0("Update of ", input_model_file,
                                  " done"))
  }

  purrr::pwalk(
    list(input_model_files,
    parameters_to_fix,
    init_values_to_fix),
    purrr::possibly(
      ~ single_model_function(
        dir_input_file = dir_input_files,
        input_model_file = ..1,
        parameters_to_fix_single = ..2,
        init_values_to_fix_single = ..3
      ), otherwise = NULL, quiet = F
    )
  )


}
