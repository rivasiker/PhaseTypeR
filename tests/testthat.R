library(testthat)
library(phasty)

test_that(
  'test of the reward function', {
    expect_equal(1, 1)
    expect_error(disc_reward())
    }
  )

test_check("phasty")
#' subint_mat <- matrix(c(0.4, 0, 0,
#'                       0.24, 0.4, 0,
#'                       0.12, 0.2, 0.5), ncol = 3)
#' init_probs <- c(0.9, 0.1, 0)
#' reward <- c(1,0,4)
#'
#' disc_reward(init_probs = init_probs,
#'             subint_mat = subint_mat, reward = reward)
