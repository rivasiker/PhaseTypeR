# test that the function runs properly

    Code
      reward_phase_type(disc_phase_type, reward = c(1, 0, 2))
    Output
      $subint_mat
           [,1] [,2] [,3]
      [1,]  0.4  0.2    0
      [2,]  0.0  0.0    1
      [3,]  0.0  0.5    0
      
      $init_probs
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      $defect
      [1] 0
      
      attr(,"class")
      [1] "disc_phase_type"

---

    Code
      reward_phase_type(disc_phase_type, reward = matrix(c(1, 0, 2), nrow = 1))
    Output
      $subint_mat
           [,1] [,2] [,3]
      [1,]  0.4  0.2    0
      [2,]  0.0  0.0    1
      [3,]  0.0  0.5    0
      
      $init_probs
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      $defect
      [1] 0
      
      attr(,"class")
      [1] "disc_phase_type"

