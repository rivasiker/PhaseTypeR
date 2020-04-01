library(testthat)
library(phasty)


test_that(
  'test of all the error and warnings in discrete phase-type function', {

    # [E] negative reward
    expect_error(cont_reward(subint_mat = matrix(c(-1.5, 0, 0,
                                                   1.5, -1, 0,
                                                   0, 1, -0.5), ncol = 3),
                             init_probs = c(1, 0, 0),
                             reward = c(-1, 0, 4)),
                 'The reward vector should only contains non-negative values')

    # [E] Too large reward vector
    expect_error(cont_reward(subint_mat = matrix(c(-1.5, 0, 0,
                                                   1.5, -1, 0,
                                                   0, 1, -0.5), ncol = 3),
                             init_probs = c(1, 0, 0),
                             reward = c(1, 0, 0, 4)),
                 'The reward vector has wrong dimensions')

    # [E] Too small reward vector
    expect_error(cont_reward(subint_mat = matrix(c(-1.5, 0, 0,
                                                   1.5, -1, 0,
                                                   0, 1, -0.5), ncol = 3),
                             init_probs = c(1, 0, 0),
                             reward = c(1, 4)),
                 'The reward vector has wrong dimensions')
  })

