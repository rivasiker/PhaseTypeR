context('Discrete reward tranformation')

library(testthat)
local_edition(3)
library(PhaseTypeR)


disc_phase_type <- DPH(subint_mat = matrix(c(0.4, 0, 0,
                                                    0.24, 0.4, 0,
                                                    0.12, 0.2, 0.5), ncol = 3),
                              init_probs = c(1, 0, 0))

test_that(
  'test of all the error and warnings in discrete phase-type function', {

    # [E] negative reward
    expect_error(reward_phase_type(
      disc_phase_type,
      reward = c(-1, 0, 4)), 'negative')

    # [E] Too large reward vector
    expect_error(reward_phase_type(
      disc_phase_type,
      reward = c(1, 0, 0, 4)))

    # [E] Too small reward vector
    expect_error(reward_phase_type(
      disc_phase_type,
      reward = c(1, 0)), 'dimensions')

    # [E] Decimal reward
    expect_error(reward_phase_type(
      disc_phase_type,
      reward = c(1.5, 0, 4)), 'integers')

    # [E] NULL reward provided
    expect_error(reward_phase_type(
      disc_phase_type,
      reward = NULL), 'valid reward')

    # [E] No reward provided
    expect_error(reward_phase_type(
      disc_phase_type), 'missing')

    # [E] No phase-type provided
    expect_error(reward_phase_type(reward = c(1, 0, 0)), 'missing')

    # [E] Reward is not a unidimensional matrix
    expect_error(reward_phase_type(disc_phase_type,
                                   reward = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)),
                                   'Wrong dimensions')

    # [E] Sum of rows equal to 1
    expect_error(reward_phase_type(disc_phase_type,
                      reward = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)),
    'equal to 1')

    # [E] No negative values or values more than 1
    expect_error(reward_phase_type(disc_phase_type,
                                   reward = matrix(c(-0.1, 0.2, 0.3, 1.1, 0.8, 0.7), nrow = 3)),
                 'should only contain')



  })


test_that(
  'test that the function runs properly', {
    expect_snapshot(reward_phase_type(disc_phase_type, reward = c(1, 0, 2)))
    expect_snapshot(reward_phase_type(disc_phase_type, reward = matrix(c(1, 0, 2), nrow = 1)))
    expect_snapshot(reward_phase_type(disc_phase_type,
                                      reward = matrix(c(0.1, 0.2, 0.3, 0.9, 0.8, 0.7), nrow = 3)))
  }
)




