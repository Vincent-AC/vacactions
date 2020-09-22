context("1 input model to 2 output models")

test_that("Parameters are correctly updated", {
  create_updated_modfiles(dir_input_files = "input_models/",
             input_model_files = "run1.mod",
             dir_output_files = "output_models/",
             output_model_files = c("run3.mod","run4.mod"),
             overwrite = TRUE,
             update_psn_based_on = TRUE)

  pharmpy <- reticulate::import("pharmpy")

  input_model <- pharmpy$Model("input_models/run1.mod")
  input_model$update_inits()
  input_updated_params <- input_model$parameters

  run3 <- pharmpy$Model("output_models/run3.mod")
  run3_parameters <-  run3$parameters

  run4 <- pharmpy$Model("output_models/run4.mod")
  run4_parameters <-  run4$parameters

  expect_equal(run3_parameters,input_updated_params)
  expect_equal(run4_parameters,input_updated_params)

  base::file.remove(base::paste0("output_models/",(base::list.files("output_models/"))))

})

test_that("Based on is created as first line", {
  create_updated_modfiles(dir_input_files = "input_models/",
                          input_model_files = "run1.mod",
                          dir_output_files = "output_models/",
                          output_model_files = c("run3.mod","run4.mod"),
                          overwrite = TRUE,
                          update_psn_based_on = TRUE)

  expect_equal(base::readLines("output_models/run3.mod")[1],
               ";; 1. Based on: 1")
  expect_equal(base::readLines("output_models/run4.mod")[1],
               ";; 1. Based on: 1")

  base::file.remove(base::paste0("output_models/",(base::list.files("output_models/"))))
})

test_that("Table names are correctly updated", {
  create_updated_modfiles(dir_input_files = "input_models/",
                          input_model_files = "run1.mod",
                          dir_output_files = "output_models/",
                          output_model_files = c("run3.mod","run4.mod"),
                          overwrite = TRUE,
                          update_psn_based_on = TRUE)

  expect_true(grepl("sdtab3",base::readLines("output_models/run3.mod")[33]))
  expect_true(grepl("patab3",base::readLines("output_models/run3.mod")[34]))
  expect_true(grepl("sdtab4",base::readLines("output_models/run4.mod")[33]))
  expect_true(grepl("patab4",base::readLines("output_models/run4.mod")[34]))

  base::file.remove(base::paste0("output_models/",(base::list.files("output_models/"))))
})

context("2 input models to 2 output models")

test_that("Parameters are correctly updated", {
  create_updated_modfiles(dir_input_files = "input_models/",
                          input_model_files = c("run1.mod","run2.mod"),
                          dir_output_files = "output_models/",
                          output_model_files = c("run5.mod","run6.mod"),
                          overwrite = TRUE,
                          update_psn_based_on = TRUE)

  pharmpy <- reticulate::import("pharmpy")

  run1 <- pharmpy$Model("input_models/run1.mod")
  run1$update_inits()
  run1_updated_params <- run1$parameters

  run2 <- pharmpy$Model("input_models/run2.mod")
  run2_parameters <-  run2$parameters

  run5 <- pharmpy$Model("output_models/run5.mod")
  run5_parameters <-  run5$parameters

  run6 <- pharmpy$Model("output_models/run6.mod")
  run6_parameters <-  run6$parameters

  expect_equal(run5_parameters,run1_updated_params)
  expect_equal(run6_parameters,run2_parameters)

  base::file.remove(base::paste0("output_models/",(base::list.files("output_models/"))))

})

test_that("Based on is created as first line", {
  create_updated_modfiles(dir_input_files = "input_models/",
                          input_model_files = c("run1.mod","run2.mod"),
                          dir_output_files = "output_models/",
                          output_model_files = c("run5.mod","run6.mod"),
                          overwrite = TRUE,
                          update_psn_based_on = TRUE)

  expect_equal(base::readLines("output_models/run5.mod")[1],
               ";; 1. Based on: 1")

  base::file.remove(base::paste0("output_models/",(base::list.files("output_models/"))))
})

test_that("Based on comments are updated", {
  create_updated_modfiles(dir_input_files = "input_models/",
                          input_model_files = c("run1.mod","run2.mod"),
                          dir_output_files = "output_models/",
                          output_model_files = c("run5.mod","run6.mod"),
                          overwrite = TRUE,
                          update_psn_based_on = TRUE)

  expect_equal(base::readLines("output_models/run6.mod")[1],
               ";; 1. Based on: 2")
  expect_equal(base::readLines("output_models/run6.mod")[5],
               ";; 1. Based on: 2")

  base::file.remove(base::paste0("output_models/",(base::list.files("output_models/"))))
})

test_that("Table names are correctly updated", {
  create_updated_modfiles(dir_input_files = "input_models/",
                          input_model_files = c("run1.mod","run2.mod"),
                          dir_output_files = "output_models/",
                          output_model_files = c("run5.mod","run6.mod"),
                          overwrite = TRUE,
                          update_psn_based_on = TRUE)

  expect_true(grepl("sdtab5",base::readLines("output_models/run5.mod")[33]))
  expect_true(grepl("patab5",base::readLines("output_models/run5.mod")[34]))
  expect_true(grepl("sdtab6",base::readLines("output_models/run6.mod")[34]))
  expect_true(grepl("patab6",base::readLines("output_models/run6.mod")[35]))

  base::file.remove(base::paste0("output_models/",(base::list.files("output_models/"))))
})

