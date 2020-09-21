#' Create multiple NONMEM model files with updated initial estimates
#'
#' The point of this function is to enable creation of multiple NONMEM model
#' files, with updated initial estimates when possible. It is possible to copy a
#' single model file multiple times or to create multiple output model files
#' from multiple input model files.
#'
#' @param input_model_files character vector of length 1 or n equal to the
#' number of output files. Contains path(s) to model(s) to be copied with updated
#' parameter initial estimates.
#' @param output_model_files character vector of length n. Contains path(s) to
#' new model files.
#' @param overwrite boolean of length 1. If TRUE and output_model_files refers
#' to an already existing file, the existing file will be overwritten.
#' Defaults to TRUE.
#'
#' @return New .mod files created
#' @export
#'
#' @examples
#' \dontrun{create_updated_modfiles("run1.mod",c("run2.mod","run3.mod"))}
create_updated_modfiles <-
  function(input_model_files,
           output_model_files,
           overwrite = TRUE)
  {
    pharmpy <- reticulate::import("pharmpy")

    update_single_modfile <-
      function(input_model_file,
               output_model_file,
               overwrite)
      {
        model <- pharmpy$Model(input_model_file)

        if (file.exists(gsub(".mod", ".ext", input_model_file)))
          model$update_inits()

        model$write(output_model_file, force = overwrite)
      }

    purrr::walk2(
      input_model_files,
      output_model_files,
      update_single_modfile(overwrite = overwrite)
    )
  }
