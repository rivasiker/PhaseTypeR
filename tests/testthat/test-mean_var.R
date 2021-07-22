context('Calculating the mean and the (co)variance')

library(testthat)
library(PhaseTypeR)


test_that(
  'test the mean function for all phase-type distributions', {

    expect_equal(mean(PH(subint_mat = matrix(c(-2, 0, 0,
                                          1, -1, 0,
                                          0, 1, -0.5), ncol = 3),
                    init_probs = c(1, 0, 0))),
                 2)



    expect_equal(mean(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                              0,   0.4,  0.2,
                              0,   0,    0.5),
                            ncol = 3,
                            byrow = TRUE),
        init_probs = c(1, 0, 0))),
        3)

    expect_equal(mean(MPH(subint_mat = matrix(c(-2, 0, 0,
                                                1, -1, 0,
                                                0, 1, -0.5), ncol = 3),
                          reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0))),
                 c(4.5, 10.5))


    expect_equal(mean(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                0,   0.4,  0.2,
                                                0,   0,    0.5),
                                              ncol = 3,
                                              byrow = TRUE),
                           reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0))),
                 c(5, 14))

  })




test_that(
  'test the var function for all phase-type distributions', {
    # [E] Not the good dimensions (subint_mat and init_probs)
    expect_equal(var(PH(subint_mat = matrix(c(-2, 0, 0,
                                               1, -1, 0,
                                               0, 1, -0.5), ncol = 3),
                         init_probs = c(1, 0, 0))),
                 5)

    expect_equal(var(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                0,   0.4,  0.2,
                                                0,   0,    0.5),
                                              ncol = 3,
                                              byrow = TRUE),
                          init_probs = c(1, 0, 0))),
                 34/9)

    expect_equal(var(MPH(subint_mat = matrix(c(-2, 0, 0,
                                                1, -1, 0,
                                                0, 1, -0.5), ncol = 3),
                          reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0))),
                 matrix(c(36.25, 76, 76, 160.75), nrow = 2))

    expect_equal(var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                 0,   0.4,  0.2,
                                                 0,   0,    0.5),
                                               ncol = 3,
                                               byrow = TRUE),
                           reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                           init_probs = c(1, 0, 0))),
                 matrix(c(626/9, 1652/9, 1652/9, 4442/9), nrow = 2))
  })
