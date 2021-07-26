context('The continuous phase-type distribution functions')

library(testthat)
local_edition(3)
library(PhaseTypeR)

subint <- matrix(c(0.4, 0, 0.2,
                            0.5, 0.3, 0.2,
                            0, 0.7, 0.2), ncol = 3)
disc_phase_type <- DPH(subint, c(1, 0, 0))


test_that(
  'test all outputs of the phase-type functions', {

    expect_snapshot_value(dDPH(0:20, disc_phase_type), style = 'serialize')
    expect_error(dDPH(0:20, 'a'), "of class 'disc_")
    expect_snapshot_value(pDPH(0:15, disc_phase_type), style = 'serialize')
    expect_error(pDPH(0:15, 'a'), "of class 'disc_")
    expect_snapshot_value(qDPH(seq(0, 1, length.out = 20), disc_phase_type), style = 'serialize')
    expect_error(qDPH(seq(0, 1, length.out = 20), 'a'), "of class 'disc_")
    expect_error(rDPH(3, 'a'), "of class 'disc_")


  })
