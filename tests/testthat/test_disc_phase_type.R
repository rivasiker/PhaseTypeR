library(testthat)
library(phasty)


test_that(
  'test of all the error and warnings in discrete phase-type function', {

    # [W] No init_probs provided
    expect_warning(disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                         0.24, 0.4, 0,
                                                         0.12, 0.2, 0.5), ncol = 3)),
                   'The initial probability vector is automatically generated.')

    # [W] Defect of 1
    expect_warning(disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0,
                                                   0.12, 0.2, 0.5), ncol = 3),
                             init_probs = c(0, 0, 0)),
                   'defect')

    # [E] No subint_mat provided
    expect_error(disc_phase_type(init_probs = c(1, 0, 0)),
                 'Unable to construct')

    # [E] Negative values in the init
    expect_error(disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                       0.24, 0.4, 0,
                                                       0.12, 0.2, 0.5), ncol = 3),
                                 init_probs = c(-1, 2, 0)),
                 'Each initial probability should be between 0 and 1')

    # [E] Greater than 1 values in the init
    expect_error(disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                       0.24, 0.4, 0,
                                                       0.12, 0.2, 0.5), ncol = 3),
                                 init_probs = c(2, 0, 0)),
                 'The total initial')

    # [E] Sum of init greater than 1
    expect_error(disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                       0.24, 0.4, 0,
                                                       0.12, 0.2, 0.5), ncol = 3),
                                 init_probs = c(0.5, 0.5, 0.5)),
                 'The total initial')

    # [E] Not the good dimensions (square matrix)
    expect_error(disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                   0.24, 0.4, 0), ncol = 2),
                             init_probs = c(1, 0)),
                 'square')

    # [E] Not the good dimensions (subint_mat and init_probs)
    expect_error(disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                       0.24, 0.4, 0,
                                                       0.12, 0.2, 0.5), ncol = 3),
                                 init_probs = c(1, 0)),
                 'match the size')

    # [E] Negative values in the subint
    expect_error(disc_phase_type(subint_mat = matrix(c(-0.4, 0, 0,
                                                       0.24, 0.4, 0,
                                                       0.12, 0.2, 0.5), ncol = 3),
                                 init_probs = c(1, 0, 0)),
                 'non-negative')

    # [E] Sum of rows subint_mat greater than 1
    expect_error(disc_phase_type(subint_mat = matrix(c(2, 0, 0,
                                                       0.24, 0.4, 0,
                                                       0.12, 0.2, 0.5), ncol = 3),
                                 init_probs = c(1, 0, 0)),
                 'rowsums')
  })

test_that(
  'Test of the mean and variance for discrete phase-type.',{
    test1 = disc_phase_type(subint_mat = matrix(c(0.4, 0, 0,
                                                  0.24, 0.4, 0,
                                                  0.12, 0.2, 0.5), ncol = 3))
    expect_equal(mean(test1))

})
