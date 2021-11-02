context('The continuous phase-type distribution functions')

library(testthat)
local_edition(3)
library(PhaseTypeR)

subintensity_matrix <- matrix(c(-3,  2,  0,
                                0, -2,  1,
                                0,  0, -1),
                              nrow = 3,
                              byrow = TRUE)
initial_probabilities = c(1, 0, 0)
cont_phase_type <- PH(subintensity_matrix, initial_probabilities)


test_that(
  'test all outputs of the phase-type functions', {

    expect_snapshot_value(dPH(0:20, cont_phase_type), style = 'serialize')
    expect_error(dPH(0:20, 'a'), "of class 'cont_")
    expect_snapshot_value(pPH(0:15, cont_phase_type), style = 'serialize')
    expect_error(pPH(0:15, 'a'), "of class 'cont_")
    expect_snapshot_value(qPH(seq(0, 1, length.out = 20), cont_phase_type), style = 'serialize')
    expect_error(qPH(seq(0, 1, length.out = 20), 'a'), "of class 'cont_")
    expect_length(rPH(10, cont_phase_type), 10)
    expect_length(rPH(1:10, cont_phase_type), 10)
    expect_type(rPH(10, cont_phase_type), "double")
    expect_error(rPH(3, 'a'), "of class 'cont_")
    expect_type(rFullPH(cont_phase_type), 'list')
    expect_error(rFullPH('a'), "of class 'cont_")


  })
