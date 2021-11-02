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
                                   'a matrix with 1 row')

  })


test_that(
  'test that the function runs properly', {
    expect_snapshot(reward_phase_type(disc_phase_type, reward = c(1, 0, 2)))
    expect_snapshot(reward_phase_type(disc_phase_type, reward = matrix(c(1, 0, 2), nrow = 1)))
  }
)




