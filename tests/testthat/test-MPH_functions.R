context('The multivariate continuous phase-type distribution functions')

library(testthat)
local_edition(3)
library(PhaseTypeR)

cont_phase_type <- matrix(c(-3, 0, 1,
                            2, -3, 1,
                            1, 1, -2), ncol = 3)
R <- matrix(c(0, 1, 1,  2,
                 2, 1, 5,  2,
                 0, 1, 10, 2), nrow = 3, ncol=4, byrow=TRUE)
Y <- MPH(cont_phase_type, reward_mat = R, init_probs = c(1, 0, 0))


test_that(
  'test all outputs of the phase-type functions', {

    expect_snapshot_value(dMPH(0:20, Y), style = 'serialize')
    expect_error(dMPH(0:20, 'a'), "of class 'mult_")
    expect_snapshot_value(pMPH(0:15, Y), style = 'serialize')
    expect_error(pMPH(0:15, 'a'), "of class 'mult_")
    expect_snapshot_value(qMPH(seq(0.1, 0.9, length.out = 20), Y), style = 'serialize')
    expect_error(qMPH(seq(0, 1, length.out = 20), 'a'), "of class 'mult_")
    expect_length(rMPH(6, Y), 24)
    expect_length(rMPH(1:6, Y), 24)
    expect_type(rMPH(6, Y), "double")
    expect_error(rMPH(3, 'a'), "of class 'mult_")
    expect_type(rFullMPH(Y), 'list')
    expect_length(rFullMPH(Y), ncol(R)+2)
    expect_error(rFullMPH('a'), "of class 'mult_")


  })
