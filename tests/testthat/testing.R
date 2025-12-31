library(testthat)
library(varselect)

test_that("Beginner User: One-line classification works on Iris", {
  # Setup data
  data <- iris
  data$is_versicolor <- as.factor(ifelse(data$Species == "versicolor", "yes", "no"))
  data$Species <- NULL

  # Run pipeline
  pipeline <- varselect(data, "is_versicolor") + fullselection() + train()

  # Assertions
  expect_s4_class(pipeline, "varselectPipeline")
  expect_true(!is.null(pipeline@model))
  expect_true(length(pipeline@final_vars) > 0)
  expect_equal(pipeline@target_type, "binary")
})

test_that("Advanced User: Custom regression pipeline works", {
  # Run pipeline
  pipeline <- varselect(iris, "Sepal.Length") +
    validate(
      missing_threshold = 0.1,
      variance_threshold = 0.05
    ) +
    significance(alpha = 0.1, correction = "bonferroni") +
    refine(vif_threshold = 5) +
    selection(
      metrics = c("rmse", "aic"),
      weights = c(0.7, 0.3),
      top = 2
    ) +
    prediction(cv_folds = 5) +
    train()

  # Assertions
  expect_s4_class(pipeline, "varselectPipeline")
  expect_equal(pipeline@target_type, "numeric")


  expect_true(length(pipeline@final_vars) >= 1)

  expect_true("RMSE" %in% names(pipeline@performance))
  expect_true(any(grepl("Refined", pipeline@steps)))
  expect_true(any(grepl("Trained", pipeline@steps)))
})
