context('Calculating the mean and the (co)variance')

library(testthat)
local_edition(3)
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
    expect_equal(mean(MPH(subint_mat = matrix(c(-2, 0, 0,
                                                1, -1, 0,
                                                0, 1, -0.5), ncol = 3),
                          reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0)),
                      v = 1),
                 4.5)
    expect_equal(mean(MPH(subint_mat = matrix(c(-2, 0, 0,
                                                1, -1, 0,
                                                0, 1, -0.5), ncol = 3),
                          reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0)),
                      v = c(2, 1)),
                 c(10.5, 4.5))
    expect_error(mean(MPH(subint_mat = matrix(c(-2, 0, 0,
                                                1, -1, 0,
                                                0, 1, -0.5), ncol = 3),
                          reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0)),
                      v = 'a'),
                 'integer')
    expect_error(mean(MPH(subint_mat = matrix(c(-2, 0, 0,
                                                1, -1, 0,
                                                0, 1, -0.5), ncol = 3),
                          reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0)),
                      v = 1.4),
                 'integer')



    expect_equal(mean(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                0,   0.4,  0.2,
                                                0,   0,    0.5),
                                              ncol = 3,
                                              byrow = TRUE),
                           reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0))),
                 c(5, 14))
    expect_equal(mean(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                 0,   0.4,  0.2,
                                                 0,   0,    0.5),
                                               ncol = 3,
                                               byrow = TRUE),
                           reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                           init_probs = c(1, 0, 0)),
                      v = 1),
                 5)
    expect_equal(mean(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                 0,   0.4,  0.2,
                                                 0,   0,    0.5),
                                               ncol = 3,
                                               byrow = TRUE),
                           reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                           init_probs = c(1, 0, 0)),
                      v = c(2, 1)),
                 c(14, 5))
    expect_error(mean(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                 0,   0.4,  0.2,
                                                 0,   0,    0.5),
                                               ncol = 3,
                                               byrow = TRUE),
                           reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                           init_probs = c(1, 0, 0)),
                      v = 'a'),
                 'integer')
    expect_error(mean(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                 0,   0.4,  0.2,
                                                 0,   0,    0.5),
                                               ncol = 3,
                                               byrow = TRUE),
                           reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                           init_probs = c(1, 0, 0)),
                      v = 1.4),
                 'integer')

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

    expect_equal(var(MPH(subint_mat = matrix(c(-2, 0, 0,
                                               1, -1, 0,
                                               0, 1, -0.5), ncol = 3),
                         reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                         init_probs = c(1, 0, 0)),
                     v = 2),
                 160.75)

    expect_equal(var(MPH(subint_mat = matrix(c(-2, 0, 0,
                                               1, -1, 0,
                                               0, 1, -0.5), ncol = 3),
                         reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                         init_probs = c(1, 0, 0)),
                     v = c(1, 2)),
                 76)

    expect_equal(
      diag(
          var(
            MPH(
              subint_mat =
                matrix(
                  c(-2, 0, 0,
                    1, -1, 0,
                    0, 1, -0.5),
                  ncol = 3),
              reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
              init_probs = c(1, 0, 0)),
          )
        ),
      c(
        var(
          reward_phase_type(
            PH(
              subint_mat =
                matrix(
                  c(-2, 0, 0,
                    1, -1, 0,
                    0, 1, -0.5),
                  ncol = 3),
              init_probs = c(1, 0, 0)),
            c(1, 2, 3)
          )
        ),
        var(
          reward_phase_type(
            PH(
              subint_mat =
                matrix(
                  c(-2, 0, 0,
                    1, -1, 0,
                    0, 1, -0.5),
                  ncol = 3),
              init_probs = c(1, 0, 0)),
            c(4, 5, 6)
          )
        )
      ))

    expect_error(var(MPH(subint_mat = matrix(c(-2, 0, 0,
                                               1, -1, 0,
                                               0, 1, -0.5), ncol = 3),
                         reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                         init_probs = c(1, 0, 0)),
                     v = c(1, 2, 1)),
                 'right indices')

    expect_snapshot(var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                   0,   0.4,  0.2,
                                                   0,   0,    0.5),
                                                 ncol = 3,
                                                 byrow = TRUE),
                             reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                             init_probs = c(1, 0, 0))))

    expect_snapshot(var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                   0,   0.4,  0.2,
                                                   0,   0,    0.5),
                                                 ncol = 3,
                                                 byrow = TRUE),
                             reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                             init_probs = c(1, 0, 0)),
                        v = 2))

    expect_snapshot(var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                   0,   0.4,  0.2,
                                                   0,   0,    0.5),
                                                 ncol = 3,
                                                 byrow = TRUE),
                             reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                             init_probs = c(1, 0, 0)),
                        v = c(1,2)))

    expect_equal(
      diag(
        var(
          MDPH(
            subint_mat =
              matrix(
                c(0.4, 0.24, 0.12,
                  0,   0.4,  0.2,
                  0,   0,    0.5),
                ncol = 3, byrow = TRUE),
            reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
            init_probs = c(1, 0, 0)))
        ),
      c(
        var(
          reward_phase_type(
            DPH(
              subint_mat =
                matrix(
                  c(0.4, 0.24, 0.12,
                    0,   0.4,  0.2,
                    0,   0,    0.5),
                  ncol = 3, byrow = TRUE),
              init_probs = c(1, 0, 0)),
            c(1, 2, 3)
            )
          ),
        var(
          reward_phase_type(
            DPH(
              subint_mat =
                matrix(
                  c(0.4, 0.24, 0.12,
                    0,   0.4,  0.2,
                    0,   0,    0.5),
                  ncol = 3, byrow = TRUE),
              init_probs = c(1, 0, 0)),
            c(4, 5, 6)
          )
        )
      )
      )


    expect_error(var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12,
                                                0,   0.4,  0.2,
                                                0,   0,    0.5),
                                              ncol = 3,
                                              byrow = TRUE),
                          reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
                          init_probs = c(1, 0, 0)),
                     v = c(1, 2, 1)),
                 'right indices')

    expect_equal(var(c(1, 2, 3, 4, 5)), 2.5)


  })









