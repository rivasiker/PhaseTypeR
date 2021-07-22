context('The multivariate continuous phase-type distribution')

library(testthat)
library(PhaseTypeR)

subintensity_matrix <- matrix(c(-3,  2,  0,
                                 0, -2,  1,
                                 0,  0, -1),
                              nrow = 3,
                              byrow = TRUE)
reward_matrix = matrix(c(4.4, 2.0, 8.8, 4.3, 1.8, 2.3), nrow = 3, ncol = 2)
initial_probabilities = c(1, 0, 0)
mult_cont_phase_type <- MPH(subintensity_matrix,
                            initial_probabilities,
                            reward_matrix)




test_that(
  'test of all the error and warnings in the check_phase_type function with MPH', {

    # [W] No init_probs provided
    expect_warning(MPH(subintensity_matrix, reward_mat = reward_matrix),
                   'automatically')

    # [W] Defect of 1
    expect_warning(MPH(subintensity_matrix,
                       c(0, 0, 0),
                       reward_matrix),
                   'defect')

    # [E] No subint_mat provided
    expect_error(MPH(reward_mat = reward_matrix, init_probs = c(1, 0, 0)),
                 'valid')

    # [E] subint_mat not a matrix
    expect_error(MPH(subint_mat = 'a',
                      reward_mat = reward_matrix,
                      init_probs = c(1, 0, 0)),
                 'should be a numeric matrix')
    expect_error(MPH(subint_mat = matrix('a'),
                     reward_mat = reward_matrix,
                     init_probs = c(1, 0, 0)),
                 'should be a numeric matrix')

    # [E] init_probs not a numeric matrix or vector
    expect_error(MPH(subint_mat = subintensity_matrix,
                      reward_mat = reward_matrix,
                      init_probs = 'a'),
                 'must be a matrix')
    expect_error(MPH(subint_mat = subintensity_matrix,
                     reward_mat = reward_matrix,
                     init_probs = matrix('a')),
                 'must be a matrix')

    # [E] Negative values in the init
    expect_error(MPH(subintensity_matrix,
                     c(-1, 2, 0),
                     reward_matrix),
                 'initial probability')

    # [E] Greater than 1 values in the init
    expect_error(MPH(subintensity_matrix,
                     c(1, 2, 0),
                     reward_matrix),
                 'sum of the initial probabilities')

    # [E] Sum of init greater than 1
    expect_error(MPH(subintensity_matrix,
                     c(0.5, 0.5, 0.5),
                     reward_matrix),
                 'sum of the initial probabilities')

    # [E] Not the good dimensions (square matrix)
    expect_error(MPH(subint_mat = matrix(c(-1.5, 0, 0,
                                           1.5, -1, 0), ncol = 2),
                     initial_probabilities,
                     reward_matrix),
                 'square')

    # [E] Not the good dimensions (subint_mat and init_probs)
    expect_error(MPH(subintensity_matrix,
                     c(1, 0),
                     reward_matrix),
                 'size')

  })


test_that(
  'test of all the error and warnings in the check_reward function with MPH', {

    # [E] No reward matrix provided
    expect_error(
      MPH(
        subintensity_matrix,
        initial_probabilities
      ),
      'reward matrix'
    )

    # [E] Non-negative reward vector values
    expect_error(
      MPH(
        subintensity_matrix,
        initial_probabilities,
        reward_mat = matrix(c(-4.4, 2.0, 8.8, 4.3, 1.8, 2.3), nrow = 3, ncol = 2)
      ),
      'non-negative'
    )

    # [E] Number of states in reward matrix not matching
    expect_error(
      MPH(
        subintensity_matrix,
        initial_probabilities,
        reward_mat = matrix(c(4.4, 2.0, 8.8, 4.3), nrow = 2, ncol = 2)
      ),
      'number of states'
    )


  })

test_that(
  'test of all the error and warnings in the MPH function', {

    # [E] zero value in the subint diagonal
    expect_error(MPH(subint_mat = matrix(c(0, 0, 0,
                                          0, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    initial_probabilities,
                    reward_matrix),
                 'negative')

    # [E] Sum of rows subint_mat greater than 0
    expect_error(MPH(subint_mat = matrix(c(-1.5, 0, 0,
                                          2, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    initial_probabilities,
                    reward_matrix),
                 'non-positive')





  }
)
