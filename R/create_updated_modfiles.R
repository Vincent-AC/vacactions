create_updated_modfiles <- function(input_model_files, output_model_files)
{
  pharmpy <- reticulate::import("pharmpy")
  model <- pharmpy$Model(input_model_files)
  if (file.exists(gsub(".mod",".ext",input_model_files))) model$update_inits()
  model$write(output_model_files)
}
