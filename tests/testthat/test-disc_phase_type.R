library(testthat)
library(phasty)

test_that(
  'test of all the error and warnings in continuous phase-type function', {

    # [W] No init_probs provided
    expect_warning(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                    1.5, -1, 0,
                                                    0, 1, -0.5), ncol = 3)))

    # [W] Defect of 1
    expect_warning(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                    1.5, -1, 0,
                                                    0, 1, -0.5), ncol = 3),
                              init_probs = c(0, 0, 0)))

    # [E] No subint_mat provided
    expect_error(phase_type(init_probs = c(1, 0, 0)))

    # [E] Negative values in the init
    expect_error(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                  1.5, -1, 0,
                                                  0, 1, -0.5), ncol = 3),
                            init_probs = c(-1, 2, 0)))

    # [E] Greater than 1 values in the init
    expect_error(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                  1.5, -1, 0,
                                                  0, 1, -0.5), ncol = 3),
                            init_probs = c(2, 0, 0)))

    # [E] Sum of init greater than 1
    expect_error(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                  1.5, -1, 0,
                                                  0, 1, -0.5), ncol = 3),
                            init_probs = c(0.5, 0.5, 0.5)))

    # [E] Not the good dimensions (square matrix)
    expect_error(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                  1.5, -1, 0), ncol = 2),
                            init_probs = c(1, 0)))

    # [E] Not the good dimensions (subint_mat and init_probs)
    expect_error(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                  1.5, -1, 0,
                                                  0, 1, -0.5), ncol = 3),
                            init_probs = c(1, 0)))

    # [E] zero value in the subint diagonal
    expect_error(phase_type(subint_mat = matrix(c(0, 0, 0,
                                                  0, -1, 0,
                                                  0, 1, -0.5), ncol = 3),
                            init_probs = c(1, 0, 0)))

    # [E] Sum of rows subint_mat greater than 0
    expect_error(phase_type(subint_mat = matrix(c(-1.5, 0, 0,
                                                  2, -1, 0,
                                                  0, 1, -0.5), ncol = 3),
                            init_probs = c(1, 0, 0)))
  })
