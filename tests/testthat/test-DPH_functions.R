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
    expect_error(dDPH(c(0.3, 0.4, 0.5), disc_phase_type), "contain integers")
    expect_error(dDPH(0:20, 'a'), "of class 'disc_")
    expect_snapshot_value(pDPH(0:15, disc_phase_type), style = 'serialize')
    expect_error(pDPH(0:15, 'a'), "of class 'disc_")
    expect_snapshot_value(qDPH(seq(0, 1, length.out = 20), disc_phase_type), style = 'serialize')
    expect_error(qDPH(seq(0, 1, length.out = 20), 'a'), "of class 'disc_")
    expect_length(rDPH(10, disc_phase_type), 10)
    expect_length(rDPH(1:10, disc_phase_type), 10)
    expect_type(rDPH(10, disc_phase_type), "double")
    expect_error(rDPH(3, 'a'), "of class 'disc_")
    expect_type(rFullDPH(disc_phase_type), 'list')
    expect_error(rFullDPH('a'), "of class 'disc_")


  })
