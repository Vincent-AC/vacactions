#' Create multiple NONMEM model files with updated initial estimates
#'
#' The point of this function is to enable creation of multiple NONMEM model
#' files, with updated initial estimates when possible. It is possible to copy a
#' single model file multiple times or to create multiple output model files
#' from multiple input model files.
#'
#' @param dir_input_files character. Contains path to directory containing the
#' input model files. Defaults to "".
#' @param input_model_files character vector of length 1 or n equal to the
#' number of output files. Contains name(s) of model(s) to be copied with updated
#' parameter initial estimates.
#' @param dir_output_files character. Contains path to directory containing the
#' output model files. Defaults to "".
#' @param output_model_files character vector of length n. Contains name(s) of
#' new model file(s).
#' @param overwrite boolean of length 1. If TRUE and output_model_files refers
#' to an already existing file, the existing file will be overwritten.
#' Defaults to TRUE.
#' @param update_psn_based_on boolean of length 1. If TRUE the PsN generated
#' comment "Based on:" will be updated or created in the first line of the code
#' if absent. Defaults to TRUE.
#'
#'
#' @return New .mod files created
#' @export
#'
#' @examples
#' \dontrun{create_updated_modfiles("../NONMEM/"
#'                                  "run1.mod",
#'                                  "../NONMEM",
#'                                c("run2.mod","run3.mod"))}
create_updated_modfiles <-
  function(dir_input_files = "",
           input_model_files,
           dir_output_files = "",
           output_model_files,
           overwrite = TRUE,
           update_psn_based_on = TRUE)
  {
    pharmpy <- reticulate::import("pharmpy")

    update_single_modfile <-
      function(dir_input_file,
               input_model_file,
               dir_output_file,
               output_model_file,
               overwrite,
               update_psn_based_on)
      {
        usethis::ui_info(base::paste0("Creating ",output_model_file," from ",input_model_file))

        model <-
          pharmpy$Model(base::paste0(dir_input_file, input_model_file))

        if (file.exists(base::paste0(dir_input_file, gsub(".mod", ".ext", input_model_file)))) {
          model$update_inits()
        }

        model$write(base::paste0(dir_output_file, output_model_file), force = overwrite)

        if (update_psn_based_on == TRUE)
        {
          input_run_n <-
            base::gsub("[[:alpha:]]|[[:punct:]]", "", input_model_file) %>% as.numeric()
          output_model_lines <-
            readLines(base::paste0(dir_output_file, output_model_file),
                      warn = F)

          if (base::length(base::grep("Based on:", output_model_lines)) == 0) {
            #If there is no "Based on:" Create it as first line

            output_model_lines_updated <- base::append(output_model_lines,
                                                       base::paste0(";; 1. Based on: ", input_run_n),
                                                       after = 0)
            base::writeLines(
              output_model_lines_updated,
              base::paste0(dir_output_file, output_model_file)
            )
          } else {
            #If there is one or multiple "Based on:" modify them all

            output_model_lines_updated <- output_model_lines

            output_model_lines_updated[base::grep("Based on:", output_model_lines_updated)] <-
              base::paste0(";; 1. Based on: ", input_run_n)

            base::writeLines(
              output_model_lines_updated,
              base::paste0(dir_output_file, output_model_file)
            )
          }
        }

      }


    purrr::walk2(
      input_model_files,
      output_model_files,
      purrr::possibly(~update_single_modfile(
        dir_input_file = dir_input_files,
        input_model_file = .x,
        dir_output_file = dir_output_files,
        output_model_file = .y,
        overwrite = overwrite,
        update_psn_based_on = update_psn_based_on
      ), otherwise = NULL, quiet = F
      )
    )
  }
