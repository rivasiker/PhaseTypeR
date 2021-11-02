# test that the function runs properly

    Code
      reward_phase_type(cont_phase_type, reward = matrix(c(1, 3, 2), nrow = 1))
    Output
      $subint_mat
           [,1]       [,2]       [,3]
      [1,] -1.5  1.5000000  0.0000000
      [2,]  0.0 -0.3333333  0.3333333
      [3,]  0.0  0.0000000 -0.2500000
      
      $init_probs
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      $defect
      [1] 0
      
      attr(,"class")
      [1] "cont_phase_type"

---

    Code
      reward_phase_type(cont_phase_type, reward = c(1, 0, 2))
    Output
      $subint_mat
           [,1]  [,2]
      [1,] -1.5  1.50
      [2,]  0.0 -0.25
      
      $init_probs
           [,1] [,2]
      [1,]    1    0
      
      $defect
      [1] 0
      
      attr(,"class")
      [1] "cont_phase_type"

---

    Code
      reward_phase_type(cont_phase_type, reward = c(1, 3, 2))
    Output
      $subint_mat
           [,1]       [,2]       [,3]
      [1,] -1.5  1.5000000  0.0000000
      [2,]  0.0 -0.3333333  0.3333333
      [3,]  0.0  0.0000000 -0.2500000
      
      $init_probs
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      $defect
      [1] 0
      
      attr(,"class")
      [1] "cont_phase_type"

---

    Code
      reward_phase_type(cont_phase_type, reward = c(1, 0, 0))
    Output
      $subint_mat
           [,1]
      [1,] -1.5
      
      $init_probs
           [,1]
      [1,]    1
      
      $defect
      [1] 0
      
      attr(,"class")
      [1] "cont_phase_type"

