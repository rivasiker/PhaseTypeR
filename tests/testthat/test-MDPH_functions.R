context('The multivariate discrete phase-type distribution functions')

library(testthat)
local_edition(3)
library(PhaseTypeR)

disc_phase_type <- matrix(c(0.4, 0, 0.2,
                            0.5, 0.3, 0.2,
                            0, 0.7, 0.2), ncol = 3)
R <- matrix(c(0, 1, 1,
              2, 1, 5,
              0, 1, 10,
              1, 2, 3), nrow = 3)
Y <- MDPH(disc_phase_type, reward_mat = R, init_probs = c(1, 0, 0))


test_that(
  'test all outputs of the phase-type functions', {

    expect_snapshot_value(dMDPH(0:20, Y), style = 'serialize')
    expect_error(dMDPH(0:20, 'a'), "of class 'mult_")
    expect_snapshot_value(pMDPH(0:15, Y), style = 'serialize')
    expect_error(pMDPH(0:15, 'a'), "of class 'mult_")
    expect_snapshot_value(qMDPH(seq(0.2, 0.9, length.out = 10), Y), style = 'serialize')
    expect_error(qMDPH(seq(0, 1, length.out = 20), 'a'), "of class 'mult_")
    expect_length(rMDPH(6, Y), ncol(R)*6)
    expect_length(rMDPH(1:6, Y), ncol(R)*6)
    expect_type(rMDPH(6, Y), "double")
    expect_error(rMDPH(3, 'a'), "of class 'mult_")
    expect_type(rFullMDPH(Y), 'list')
    expect_length(rFullMDPH(Y), ncol(R)+2)
    expect_error(rFullMDPH('a'), "of class 'mult_")


  })
