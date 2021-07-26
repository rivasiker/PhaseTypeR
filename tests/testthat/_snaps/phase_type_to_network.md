# networks in the continuous phase-type distribution

    Code
      phase_type_to_network(cont_phase_type)[1:5]
    Output
      5 x 5 sparse Matrix of class "dgCMatrix"
         V0   V1   V2   V3  V4
      V0  .  1.0  .    .   .  
      V1  . -1.5  1.5  .   .  
      V2  .  .   -1.0  1.0 .  
      V3  .  .    .   -0.5 0.5
      V4  .  .    .    .   .  

---

    Code
      phase_type_to_network(cont_phase_type, 3)[1:4]
    Output
      4 x 4 sparse Matrix of class "dgCMatrix"
            V1    V2    V3    V4
      V1 0.011 0.116 0.404 0.469
      V2 .     0.050 0.347 0.604
      V3 .     .     0.223 0.777
      V4 .     .     .     1.000

---

    Code
      phase_type_to_network(cont_phase_type, 0)[1:5]
    Output
      5 x 5 sparse Matrix of class "dgCMatrix"
         V0 V1 V2 V3 V4
      V0  .  1  .  .  .
      V1  .  1  .  .  .
      V2  .  .  1  .  .
      V3  .  .  .  1  .
      V4  .  .  .  .  1

# networks in the discrete phase-type distribution

    Code
      phase_type_to_network(disc_phase_type)[1:5]
    Output
      5 x 5 sparse Matrix of class "dgCMatrix"
         V0  V1  V2  V3  V4
      V0  . 0.7 0.3 .   .  
      V1  . .   0.2 0.8 .  
      V2  . 0.5 0.5 .   .  
      V3  . .   .   0.4 0.6
      V4  . .   .   .   1.0

