library(testthat)
library(phasty)


test_that(
  'test of the reward function', {

    # empty function return an error
    expect_error(disc_reward(reward = c(1,0,1)), 'Wrong')

    # sum of init_probs should be one
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                             init_probs = c(0, 0, 0),
                             reward = c(1,0,1)))

    # not the same length (subint_mat & init_probs)
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                             init_probs = c(1, 0),
                             reward = c(1,0,1)))

    # not the same length (subint_mat & reward)

    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                             init_probs = c(1, 0, 1),
                             reward = c(1,0)))

    # not the good dimensions (square matrix)
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0), ncol = 2),
                             init_probs = c(1, 0),
                             reward = c(1,0,1)))

    #
  }
  )

#test_check("phasty")
#' subint_mat <- matrix(c(0.4, 0, 0,
#'                       0.24, 0.4, 0,
#'                       0.12, 0.2, 0.5), ncol = 3)
#' init_probs <- c(0.9, 0.1, 0)
#' reward <- c(1,0,4)
#'
#' disc_reward(init_probs = init_probs,
#'             subint_mat = subint_mat, reward = reward)
