#' Construction of DPH-representation
#'
#'
#' \code{DPHrep} computes the representation of of a integer-linear-combination of the Site Frequency as a discrete phase-type distribution.
#' The construction is described in Section 7.1 of Hobolth (2020).
#'
#' @param bM Subtransition probabilities un the underlying discrete Markov chain (cf. Figure 6).
#' @param bA Statespace of the underlying block-counting process
#' @param ba Vector of integer coeffcients
#'
#' @return List consisting of
#' bMt: The constructed matrix subtransition probabilities.
#' sizes_of_blocks: The sizes of the constructed blocks.
#'
#' @examples
#' ba = c(1,2,3)
#' ph_bcp = block_counting_process(4)
#' subintensity_matrix = ph_bcp$subint_mat
#' bA = ph_bcp$reward_mat
#' ph = phase_type(subintensity_matrix)
#' ph_rew_obj = reward_phase_type(ph, rowSums(rew_mat))
#' bS = ph_rew_obj$subint_mat
#' bM = solve(diag(dim(bS)[1])-(2/theta)*bS)
#' DPHrep(ba,bM)
#'
#'
#' @export
DPHrep <- function(bM,bA,ba) {
  m = nrow(bA) #this is p in the paper
  sizes_of_blocks = rep(0,m) #obtain the sizes of the blocks using formula XX
  for(i in 1:m) {
    sizes_of_blocks[i]=max(ba*(bA[i,] > 0))
  }

  bMt = matrix(0,sum(sizes_of_blocks),sum(sizes_of_blocks)) #bold-Mtilde
  for(i in 1:m) {
    for(j in 1:m) {
      if(i <= j) { #off-diagonal blocks
        bmvec = rep(0,sizes_of_blocks[j])
        # The bottom row is calculated using formula DD
        for(k in 1:sizes_of_blocks[j]) {
          bmvec[sizes_of_blocks[j]-k+1]=sum(bA[j,]*(ba == k))
        }
        bmvec = bM[i,j]*bmvec/sum(bmvec)
        cur_i = sum(sizes_of_blocks[1:i])
        if(j == 1) {
          cur_j = 1
        }
        else {
          cur_j = sum(sizes_of_blocks[1:(j-1)]) + 1
        }
        bMt[cur_i,cur_j:(cur_j+sizes_of_blocks[j]-1)] = bmvec
      }
      # The diagonal-blocks are treated as a separate case
      if((i == j) && sizes_of_blocks[i] > 1) {
        size_of_current_block = sizes_of_blocks[i]
        cur_i = sum(sizes_of_blocks[1:i]) - size_of_current_block + 1
        cur_j = sum(sizes_of_blocks[1:j]) - size_of_current_block + 2
        bMt[cur_i:(cur_i + size_of_current_block - 2),cur_j:(cur_j +   size_of_current_block - 2)] = diag(size_of_current_block-1) #add identity-matrix of appropriate size
      }
    }
  }
  return(list(bMt,sizes_of_blocks))
}
