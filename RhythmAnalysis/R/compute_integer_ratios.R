# Function to calcualte integer ratios in interval sequences 
# calculating either ratios for all pairs of intervals in the sequence or (for long sequences) for a random 
# subset of pairs 

# author: Lara S. Burchardt 
# Assistance with code design and implementation provided by ChatGPT (GPT-4-turbo), OpenAI, August 2025.

compute_integer_ratios <- function(ioi_vec, method = "all", n_pairs = 20, seed = 123) {
  n <- length(ioi_vec)
  if (n < 2) return(NULL)
  
  all_pairs <- expand.grid(i = 1:n, j = 1:n)
  all_pairs <- all_pairs[all_pairs$i != all_pairs$j, ]
  
  if (method == "random") {
    set.seed(seed)
    
    # Define adjacent pairs
    adj_pairs <- data.frame(i = 1:(n - 1), j = 2:n)
    adj_pairs$adjacent <- TRUE
    
    # Identify non-adjacent pairs
    adj_labels <- paste0(adj_pairs$i, "-", adj_pairs$j)
    all_labels <- paste0(all_pairs$i, "-", all_pairs$j)
    non_adj_pairs <- all_pairs[!(all_labels %in% adj_labels), ]
    non_adj_pairs$adjacent <- FALSE
    
    # Sample 50:50
    n_adj <- min(ceiling(n_pairs / 2), nrow(adj_pairs))
    n_non_adj <- min(n_pairs - n_adj, nrow(non_adj_pairs))
    
    sampled_adj <- adj_pairs[sample(nrow(adj_pairs), n_adj), ]
    sampled_non_adj <- non_adj_pairs[sample(nrow(non_adj_pairs), n_non_adj), ]
    
    sampled_pairs <- rbind(sampled_adj, sampled_non_adj)
  } else {
    all_pairs$adjacent <- abs(all_pairs$i - all_pairs$j) == 1
    sampled_pairs <- all_pairs
  }
  
  # Compute the ratios
  sampled_pairs$ratio <- ioi_vec[sampled_pairs$i] / 
    (ioi_vec[sampled_pairs$i] + ioi_vec[sampled_pairs$j])
  
  return(sampled_pairs)
}
