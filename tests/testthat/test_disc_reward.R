library(testthat)
library(phasty)


test_that(
  'test of all the error and warnings in discrete phase-type function', {

    # [W] No init_probs provided
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                             init_probs = NULL,
                             reward = c(1, 0, 4)),
                 'The reward vector should only contains integer')

    # [E] negative reward
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                             init_probs = c(1, 0, 0),
                             reward = c(-1, 0, 4)),
                   'The reward vector should only contains non-negative values')

    # [E] Too large reward vector
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                  0.24, 0.4, 0,
                                                  0.12, 0.2, 0.5), ncol = 3),
                                   init_probs = c(1, 0, 0),
                                   reward = c(1, 0, 0, 4)),
                   'The reward vector has wrong dimensions')

    # [E] Too small reward vector
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                                   init_probs = c(1, 0, 0),
                                   reward = c(1, 4)),
                   'The reward vector has wrong dimensions')

    # [E] Decimal reward
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                                   init_probs = c(1, 0, 0),
                                   reward = c(1.5, 0, 4)),
                 'The reward vector should only contains integer')

    # [E] no reward provided
    expect_error(disc_reward(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                             init_probs = c(1, 0, 0),
                             reward = NULL),
                 '')
  })

