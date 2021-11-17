# test all outputs of the phase-type summaries

    Code
      summary(PH(subint_mat = matrix(c(-1.5, 0, 0, 1.5, -1, 0, 0, 1, -0.5), ncol = 3),
      init_probs = c(1, 0, 0)))
    Output
      
      Subintensity matrix:
           [,1] [,2] [,3]
      [1,] -1.5  1.5  0.0
      [2,]  0.0 -1.0  1.0
      [3,]  0.0  0.0 -0.5
      
      Initial probabilities:
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      Defect:
      [1] 0
      
      Mean: 3.666667
      
      Variance: 5.444444
      

---

    Code
      summary(DPH(subint_mat = matrix(c(0.4, 0.24, 0.12, 0, 0.4, 0.2, 0, 0, 0.5),
      ncol = 3, byrow = TRUE), init_probs = c(1, 0, 0)))
    Output
      
      Subintensity matrix:
           [,1] [,2] [,3]
      [1,]  0.4 0.24 0.12
      [2,]  0.0 0.40 0.20
      [3,]  0.0 0.00 0.50
      
      Initial probabilities:
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      Defect:
      [1] 0
      
      Mean: 3
      
      Variance: 3.777778
      

---

    Code
      summary(MPH(subint_mat = matrix(c(-3, 2, 0, 0, -2, 1, 0, 0, -1), nrow = 3,
      byrow = TRUE), reward_mat = matrix(c(4.4, 2, 8.8, 4.3, 1.8, 2.3), nrow = 3,
      ncol = 2), init_probs = c(1, 0, 0)))
    Output
      
      Subintensity matrix:
           [,1] [,2] [,3]
      [1,]   -3    2    0
      [2,]    0   -2    1
      [3,]    0    0   -1
      
      Reward matrix:
           [,1] [,2]
      [1,]  4.4  4.3
      [2,]  2.0  1.8
      [3,]  8.8  2.3
      
      Initial probabilities:
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      Defect:
      [1] 0
      
      Means:
      [1] 5.066667 2.800000
      
      Variance-covariance matrix:
               [,1]      [,2]
      [1,] 48.01778 15.282222
      [2,] 15.28222  6.173333
      
      

---

    Code
      summary(MDPH(subint_mat = matrix(c(0.4, 0.24, 0.12, 0, 0.4, 0.2, 0, 0, 0.5),
      ncol = 3, byrow = TRUE), reward_mat = matrix(c(4, 2, 8, 4, 1, 2), nrow = 3,
      ncol = 2), init_probs = c(1, 0, 0)))
    Output
      
      Subintensity matrix:
           [,1] [,2] [,3]
      [1,]  0.4 0.24 0.12
      [2,]  0.0 0.40 0.20
      [3,]  0.0 0.00 0.50
      
      Reward matrix:
           [,1] [,2]
      [1,]    4    4
      [2,]    2    1
      [3,]    8    2
      
      Initial probabilities:
           [,1] [,2] [,3]
      [1,]    1    0    0
      
      Defect:
      [1] 0
      
      Means:
      [1] 13.333333  8.666667
      
      Variance-covariance matrix:
               [,1]     [,2]
      [1,] 477.3333 276.0000
      [2,] 276.0000 175.3333
      
      

