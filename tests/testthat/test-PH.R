context('The continuous phase-type distribution')

library(testthat)
local_edition(3)
library(PhaseTypeR)

test_that(
  'test of all the error and warnings in the check_phase_type function with PH', {

    # [W] No init_probs provided
    expect_warning(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                            1.5, -1, 0,
                                            0, 1, -0.5), ncol = 3)),
                   'automatically')

    # [W] Defect of 1
    expect_warning(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                            1.5, -1, 0,
                                            0, 1, -0.5), ncol = 3),
                      init_probs = c(0, 0, 0)),
                   'defect')

    # [E] No subint_mat provided
    expect_error(PH(init_probs = c(1, 0, 0)),
                 'valid')

    # [E] subint_mat not a matrix
    expect_error(PH(subint_mat = 'a',
                    init_probs = c(1, 0, 0)),
                 'should be a numeric matrix')
    expect_error(PH(subint_mat = matrix('a'),
                    init_probs = c(1, 0, 0)),
                 'should be a numeric matrix')

    # [E] init_probs not a numeric matrix or vector
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          1.5, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                     init_probs = 'a'),
                 'must be a matrix')
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          1.5, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = matrix('a')),
                 'must be a matrix')

    # [E] Negative values in the init
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          1.5, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = c(-1, 2, 0)),
                 'initial probability')

    # [E] Greater than 1 values in the init
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          1.5, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = c(2, 0, 0)),
                 'sum of the initial probabilities')

    # [E] Sum of init greater than 1
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          1.5, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = c(0.5, 0.5, 0.5)),
                 'sum of the initial probabilities')

    # [E] Not the good dimensions (square matrix)
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          1.5, -1, 0), ncol = 2),
                    init_probs = c(1, 0)),
                 'square')

    # [E] Not the good dimensions (subint_mat and init_probs)
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          1.5, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = c(1, 0)),
                 'size')


  })



test_that(
  'test of all the error and warnings in the PH function', {

    # [E] zero value in the subint diagonal
    expect_error(PH(subint_mat = matrix(c(0, 0, 0,
                                          0, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = c(1, 0, 0)),
                 'negative')

    # [E] Sum of rows subint_mat greater than 0
    expect_error(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                          2, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = c(1, 0, 0)),
                 'non-positive')

  }
)




