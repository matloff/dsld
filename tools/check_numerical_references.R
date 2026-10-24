# Independent numerical references across seeds, group counts, and contrasts.
# Run after installing the candidate: Rscript tools/check_numerical_references.R
suppressPackageStartupMessages(library(dsld))
assert_close <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected),
                             tolerance = 1e-7)))
}
settings <- 0L
for (seed in seq_len(10L)) {
  for (group_count in c(2L, 3L, 5L)) {
    set.seed(seed + 100L * group_count)
    frame <- data.frame(x = rnorm(200), z = rnorm(200),
      group = factor(rep(letters[seq_len(group_count)], length.out = 200)))
    frame$y <- 1 + 2 * frame$x - .7 * frame$z + as.integer(frame$group) / 3 + rnorm(200)
    binary <- frame
    binary$y <- factor(rbinom(200, 1, plogis(.2 + .4 * frame$x - .2 * frame$z)))
    for (contrast in list(contr.treatment(group_count), contr.sum(group_count))) {
      contrasts(frame$group) <- contrasts(binary$group) <- contrast
      for (interactions in c(FALSE, TRUE)) {
        points <- frame[c(1, 17, 46, 103), c("x", "z", "group")]
        # stats::predict uses the fitted contrasts; a redundant attribute on
        # new data otherwise emits its standard "contrasts dropped" warning.
        attr(points$group, "contrasts") <- NULL
        use_sandwich <- seed %% 2L == 0L
        linear <- dsldLinear(frame, "y", "group", interactions, points, use_sandwich)
        logistic <- dsldLogit(binary, "y", "group", points, interactions, "1")
        actual_linear <- predict(linear, points)
        actual_logistic <- predict(logistic, points)
        groups <- if (interactions) levels(frame$group) else NA_character_
        for (group in groups) {
          rows <- if (interactions) frame$group == group else rep(TRUE, nrow(frame))
          formula <- if (interactions) y ~ x + z else y ~ x + z + group
          reference_linear <- lm(formula, frame[rows, ])
          binary_data <- binary[rows, ]
          binary_data$y <- as.integer(binary_data$y == "1")
          reference_logistic <- glm(formula, binary_data, family = binomial())
          actual_rows <- if (interactions) actual_linear$level == group else rep(TRUE, nrow(points))
          assert_close(actual_linear$prediction[actual_rows], predict(reference_linear, points))
          if (use_sandwich) {
            design <- model.matrix(delete.response(terms(reference_linear)), points,
                                    contrasts.arg = reference_linear$contrasts)
            expected_se <- sqrt(diag(design %*% sandwich::sandwich(reference_linear) %*% t(design)))
          } else expected_se <- predict(reference_linear, points, se.fit = TRUE)$se.fit
          assert_close(actual_linear$standardError[actual_rows], expected_se)
          reference <- predict(reference_logistic, points, type = "response", se.fit = TRUE)
          assert_close(actual_logistic$prediction[actual_rows], reference$fit)
          assert_close(actual_logistic$standardError[actual_rows], reference$se.fit)
        }
        settings <- settings + 1L
      }
    }
  }
}
cat("PASS:", settings, "settings; linear/logistic predictions and standard errors match independent references.\n")

# Reconstruct CV independently, including uneven folds, training-only scaling,
# an unpenalized intercept, sample-weighted MAE, and fold-mean correlations.
for (seed in seq_len(12L)) {
  set.seed(1000L + seed)
  frame <- data.frame(x = rnorm(83), z = rnorm(83),
                       group = factor(rep(letters[1:3], length.out = 83)))
  frame$y <- 3 + 1.7 * frame$x + .4 * frame$z + rnorm(83)
  set.seed(seed)
  permutation <- sample.int(nrow(frame))
  folds <- split(permutation, rep(seq_len(5L), length.out = nrow(frame)))
  expected <- lapply(c(0, 4), function(penalty) {
    absolute_errors <- numeric(nrow(frame))
    correlations <- matrix(NA_real_, length(folds), 3L)
    for (fold in seq_along(folds)) {
      held_out <- folds[[fold]]
      training <- frame[-held_out, ]
      center <- colMeans(training[c("x", "z")])
      scaling <- vapply(training[c("x", "z")], sd, numeric(1))
      design <- cbind(1, scale(training[c("x", "z")], center, scaling))
      coefficients <- solve(crossprod(design) + diag(c(0, penalty, 0)),
                            crossprod(design, training$y))
      test_design <- cbind(1, scale(frame[held_out, c("x", "z")], center, scaling))
      predictions <- as.numeric(test_design %*% coefficients)
      absolute_errors[held_out] <- abs(predictions - frame$y[held_out])
      correlations[fold, ] <- vapply(letters[1:3], function(group) {
        cor(predictions, as.integer(frame$group[held_out] == group))
      }, numeric(1))
    }
    c(mean(absolute_errors), colMeans(correlations))
  })
  set.seed(seed)
  actual <- dsldFairUtils(frame, "y", "group", "dsldQeFairRidgeLin",
                          deweightPars = list(x = c(0, 4)), k_folds = 5)
  for (candidate in seq_along(expected)) {
    assert_close(unlist(actual[candidate, c("testAcc", paste0("group==", letters[1:3]))]),
                 expected[[candidate]])
  }
}
cat("PASS: 24 CV candidates across 12 seeds match independent penalized normal equations and aggregation.\n")
