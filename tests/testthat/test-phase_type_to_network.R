context('Phase-type distributions to networks')

library(testthat)
local_edition(3)
library(PhaseTypeR)

cont_phase_type <- PH(subint_mat = matrix(c(-1.5, 0, 0,
                                            1.5, -1, 0,
                                            0, 1, -0.5), ncol = 3),
                      init_probs = c(1, 0, 0))

test_that(
  'networks in the continuous phase-type distribution', {
    expect_snapshot(phase_type_to_network(cont_phase_type)[1:5])
    expect_snapshot(phase_type_to_network(cont_phase_type, 3)[1:4])
    expect_snapshot(phase_type_to_network(cont_phase_type, 0)[1:5])
    expect_error(phase_type_to_network(cont_phase_type, 'a'),
                 'time or NULL')
  }
)



disc_phase_type <- DPH(matrix(c(0, 0.2, 0.8,
                                0.5, 0.5, 0,
                                0, 0, 0.4), ncol = 3, byrow = T),
                       c(0.7, 0.3, 0))


test_that(
  'networks in the discrete phase-type distribution', {
    expect_snapshot(phase_type_to_network(disc_phase_type)[1:5])
    expect_warning(phase_type_to_network(disc_phase_type, 3)[1:5],
                   'Argument t not used')
  }
)

test_that(
  'there is an error when not using the right phase-type object', {
    expect_error(phase_type_to_network('a'), 'Please provide a ')
  }
)
