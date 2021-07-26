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
      reward_phase_type(disc_phase_type, reward = matrix(c(0.1, 0.2, 0.3, 0.9, 0.8,
        0.7), nrow = 3))
    Output
      $subint_mat
            [,1]      [,2]      [,3]
      [1,] 0.375 0.2173913 0.1118926
      [2,] 0.000 0.3478261 0.1790281
      [3,] 0.000 0.0000000 0.4117647
      
      $init_probs
             [,1]       [,2]       [,3]
      [1,] 0.9375 0.02173913 0.01118926
      
      $defect
      [1] 0.02957161
      
      attr(,"class")
      [1] "disc_phase_type"

