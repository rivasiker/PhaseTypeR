context('Continuous reward tranformation')

library(testthat)
library(PhaseTypeR)

cont_phase_type <- PH(subint_mat = matrix(c(-1.5, 0, 0,
                                                    1.5, -1, 0,
                                                    0, 1, -0.5), ncol = 3),
                              init_probs = c(1, 0, 0))

test_that(
  'test of all the error and warnings in the continuous phase-type reward function', {

    # [E] negative reward
    expect_error(reward_phase_type(
      cont_phase_type,
      reward = c(-1, 0, 4)), 'negative')

    # [E] Too large reward vector
    expect_error(reward_phase_type(
      cont_phase_type,
      reward = c(1, 0, 0, 4)), 'dimensions')

    # [E] Too small reward vector
    expect_error(reward_phase_type(
      cont_phase_type,
      reward = c(1, 0)), 'dimensions')

    # [E] NULL reward provided
    expect_error(reward_phase_type(
      cont_phase_type,
      reward = NULL), 'valid')

    # [E] No reward provided
    expect_error(reward_phase_type(
      cont_phase_type), 'missing')

    # [E] No phase-type provided
    expect_error(reward_phase_type(reward = c(1, 0, 0)), 'missing')


  })

