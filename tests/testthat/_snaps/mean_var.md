# test the var function for all phase-type distributions

    Code
      var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12, 0, 0.4, 0.2, 0, 0, 0.5), ncol = 3,
      byrow = TRUE), reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
      init_probs = c(1, 0, 0)))
    Output
               [,1]      [,2]
      [1,] 19.55556  43.55556
      [2,] 43.55556 101.55556

---

    Code
      var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12, 0, 0.4, 0.2, 0, 0, 0.5), ncol = 3,
      byrow = TRUE), reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
      init_probs = c(1, 0, 0)), v = 2)
    Output
      [1] 101.5556

---

    Code
      var(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12, 0, 0.4, 0.2, 0, 0, 0.5), ncol = 3,
      byrow = TRUE), reward_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2),
      init_probs = c(1, 0, 0)), v = c(1, 2))
    Output
      [1] 43.55556

