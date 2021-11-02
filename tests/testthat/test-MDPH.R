context('The multivariate discrete phase-type distribution')

library(testthat)
library(PhaseTypeR)


subintensity_matrix <- matrix(c(0.4, 0.24, 0.12,
                                0,   0.4,  0.2,
                                0,   0,    0.5),
                              ncol = 3,
                              byrow = TRUE)
reward_matrix <- matrix(sample(seq(0, 10), 6), nrow = 3, ncol = 2)
initial_probabilities = c(1, 0, 0)
mult_disc_phase_type <- MDPH(subintensity_matrix,
                            initial_probabilities,
                            reward_matrix)




test_that(
  'test of all the error and warnings in the check_phase_type function with MDPH', {

    # [W] No init_probs provided
    expect_warning(MDPH(subintensity_matrix, reward_mat = reward_matrix),
                   'automatically')

    # [W] Defect of 1
    expect_warning(MDPH(subintensity_matrix,
                       c(0, 0, 0),
                       reward_matrix),
                   'defect')

    # [E] No subint_mat provided
    expect_error(MDPH(reward_mat = reward_matrix, init_probs = c(1, 0, 0)),
                 'valid')

    # [E] subint_mat not a matrix
    expect_error(MDPH(subint_mat = 'a',
                     reward_mat = reward_matrix,
                     init_probs = c(1, 0, 0)),
                 'should be a numeric matrix')

    # [E] init_probs not a numeric matrix or vector
    expect_error(MDPH(subint_mat = subintensity_matrix,
                     init_probs = 'a'),
                 'must be a matrix')
    expect_error(MDPH(subint_mat = subintensity_matrix,
                      init_probs = matrix('a')),
                 'must be a matrix')

    # [E] Negative values in the init
    expect_error(MDPH(subintensity_matrix,
                     c(-1, 2, 0),
                     reward_matrix),
                 'initial probability')

    # [E] Greater than 1 values in the init
    expect_error(MDPH(subintensity_matrix,
                     c(1, 2, 0),
                     reward_matrix),
                 'sum of the initial probabilities')

    # [E] Sum of init greater than 1
    expect_error(MDPH(subintensity_matrix,
                     c(0.5, 0.5, 0.5),
                     reward_matrix),
                 'sum of the initial probabilities')

    # [E] Not the good dimensions (square matrix)
    expect_error(MDPH(subint_mat = matrix(c(-1.5, 0, 0,
                                           1.5, -1, 0), ncol = 2),
                     initial_probabilities,
                     reward_matrix),
                 'square')

    # [E] Not the good dimensions (subint_mat and init_probs)
    expect_error(MDPH(subintensity_matrix,
                     c(1, 0),
                     reward_matrix),
                 'size')

  })


test_that(
  'test of all the error and warnings in the check_reward function with MDPH', {

    # [E] No reward matrix provided
    expect_error(
      MDPH(
        subintensity_matrix,
        initial_probabilities
      ),
      'reward matrix'
    )

    # [E] Non-negative reward vector values
    expect_error(
      MDPH(
        subintensity_matrix,
        initial_probabilities,
        reward_mat = matrix(c(-4.4, 2.0, 8.8, 4.3, 1.8, 2.3), nrow = 3, ncol = 2)
      ),
      'non-negative'
    )

    # [E] Number of states in reward matrix not matching
    expect_error(
      MDPH(
        subintensity_matrix,
        initial_probabilities,
        reward_mat = matrix(c(4.4, 2.0, 8.8, 4.3), nrow = 2, ncol = 2)
      ),
      'number of states'
    )


  })



test_that(
  'test of all the error and warnings in the MDPH function', {

    # [E] rowsums of over 1
    expect_error(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0.5,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = c(1, 0, 0),
                     reward_mat = reward_matrix),
                 'rowsums')

    # [E] Values over 1
    expect_error(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           3,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = c(1, 0, 0),
                     reward_mat = reward_matrix),
                 'values between 0 and 1')

    # [E] Negative values
    expect_error(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           -0.2,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = c(1, 0, 0),
                     reward_mat = reward_matrix),
                 'non-negative')

    # [E] Negative values
    expect_error(MDPH(subint_mat = subintensity_matrix,
                      init_probs = c(1, 0, 0),
                      reward_mat = matrix(c(sample(seq(0, 10), 5), 0.5), nrow = 3, ncol = 2)),
                 'should only contain integer')

  }
)


