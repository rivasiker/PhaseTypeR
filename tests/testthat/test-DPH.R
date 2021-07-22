context('The discrete phase-type distribution')

library(testthat)
library(PhaseTypeR)


test_that(
  'test of all the error and warnings in the check_phase_type function with DPH', {

    # [W] No init_probs provided
    expect_warning(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                             0,   0.4,  0.2,
                                             0,   0,    0.5),
                                           ncol = 3,
                                           byrow = TRUE)),
                   'initial')

    # [W] Defect of 1
    expect_warning(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                             0,   0.4,  0.2,
                                             0,   0,    0.5),
                                           ncol = 3,
                                           byrow = TRUE),
                    init_probs = c(0, 0, 0)),
                   'defect')

    # [E] No subint_mat provided
    expect_error(DPH(init_probs = c(1, 0, 0)),
                 'valid')

    # [E] subint_mat not a matrix
    expect_error(DPH(subint_mat = 'a',
                     init_probs = c(1, 0, 0)),
                 'should be a numeric matrix')
    expect_error(DPH(subint_mat = matrix('a'),
                     init_probs = c(1, 0, 0)),
                 'should be a numeric matrix')

    # [E] init_probs not a numeric matrix or vector
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = 'a'),
                 'must be a matrix')
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = matrix('a')),
                 'must be a matrix')

    # [E] Negative values in the init
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                 init_probs = c(-1, 2, 0)),
                 'probability')

    # [E] Greater than 1 values in the init
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                 init_probs = c(2, 0, 0)),
                 'sum of the initial probabilities')

    # [E] Sum of init greater than 1
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                 init_probs = c(0.5, 0.5, 0.5)),
                 'sum of the initial probabilities')

    # [E] Not the good dimensions (square matrix)
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0,   0.4,  0.2),
                                        ncol = 3,
                                        byrow = TRUE),
                     init_probs = c(1, 0, 0)),
                 'square')

    # [E] Not the good dimensions (subint_mat and init_probs)
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = c(1, 0)),
                 'size')

  })



test_that(
  'test of all the error and warnings in the DPH function', {

    # [E] rowsums of over 1
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           0.5,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = c(1, 0, 0)),
                 'rowsums')

    # [E] Values over 1
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           3,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = c(1, 0, 0)),
                 'values between 0 and 1')

    # [E] Negative values
    expect_error(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                           -0.2,   0.4,  0.2,
                                           0,   0,    0.5),
                                         ncol = 3,
                                         byrow = TRUE),
                     init_probs = c(1, 0, 0)),
                 'non-negative')

  }
)
