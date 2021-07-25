context('Summary function')

library(testthat)
local_edition(3)
library(PhaseTypeR)


test_that(
  'test all outputs of the phase-type summaries', {

    # PH
    expect_snapshot(summary(PH(subint_mat = matrix(c(-1.5, 0, 0,
                                             1.5, -1, 0,
                                             0, 1, -0.5), ncol = 3),
                       init_probs = c(1, 0, 0))))

    # DPH
    expect_snapshot(summary(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                      0,   0.4,  0.2,
                                                      0,   0,    0.5),
                                                    ncol = 3,
                                                    byrow = TRUE),
                               init_probs = c(1, 0, 0))))

    # MPH
    expect_snapshot(summary(MPH(subint_mat = matrix(c(-3,  2,  0,
                                         0, -2,  1,
                                         0,  0, -1),
                                       nrow = 3,
                                       byrow = TRUE),
                               reward_mat =  matrix(c(4.4, 2.0, 8.8, 4.3, 1.8, 2.3), nrow = 3, ncol = 2),
                               init_probs = c(1, 0, 0))))

    # MDPH
    expect_snapshot(summary(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                     0,   0.4,  0.2,
                                                     0,   0,    0.5),
                                                   ncol = 3,
                                                   byrow = TRUE),
                               reward_mat = matrix(c(4, 2, 8, 4, 1, 2), nrow = 3, ncol = 2),
                               init_probs = c(1, 0, 0))))


  })
