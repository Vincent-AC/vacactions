#' Unfix parameters and overwrite the model with the updated unfixed parameters
#'
#' What the title says. A model file and a character vector of parameters to be
#' unfixed has to be provided
#'
#' @param dir_input_files character of length 1 or n equal to the
#' number of input files. Contains path to directory containing the
#' input model files. Defaults to "".
#' @param input_model_files character vector of length 1 or n equal to the
#' number of input files. Contains name(s) of model(s) to be changed with some
#' unfixed parameters
#' @param parameters_to_unfix character of length 1 or p equal to the number of
#' parameters to be unfixed. Contains the names of the parameters to be unfixed.
#'
#' @return New .mod files created
#' @export
#'
#' @examples
#' #' \dontrun{unfix_parameters_and_replace_model("../NONMEM/","run1.mod",c("KG","EMAX"))}
unfix_parameters_and_replace_model <- function(dir_input_files,
                                               input_model_files,
                                               parameters_to_unfix)
{
  pharmpy <- reticulate::import("pharmpy")
  modeling <- reticulate::import("pharmpy.modeling")
  pharmpy$plugins$nonmem$conf$parameter_names <- "comment"
  fix_parameter <- modeling$fix_parameters
  unfix_parameter <- modeling$unfix_parameters

  model <- pharmpy$Model(paste0(dir_input_files,input_model_files))
  purrr::walk(parameters_to_unfix,~unfix_parameter(model,.x))
  purrr::walk(parameters_to_unfix,~usethis::ui_info((base::paste0("Unfixed ",.x))))
  modeling$update_source(model)
  model$write(paste0(dir_input_files,input_model_files),
              force = T)
  usethis::ui_info(base::paste0("Update of ", input_model_files,
                                " done"))
}
