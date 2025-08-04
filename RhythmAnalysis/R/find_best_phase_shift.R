# function for finding best phase shift of ioi beat
# minimizing deviations

# author: Lara S. Burchardt with help of ChatGPT
library(ggplot2)

find_best_phase_shift <- function(data_ugof, beat_period, step_size = 0.01) {
  
  # Extend phase shifts to include negative values (e.g., one full beat before zero)
  phase_shifts <- seq(-beat_period, beat_period, by = step_size)
  
  min_data <- min(data_ugof)
  max_data <- max(data_ugof)
  
  # Initialize vector to store RMSD per shift
  rmsd_values <- numeric(length(phase_shifts))
  
  for (i in seq_along(phase_shifts)) {
    
    shift <- phase_shifts[i]
    
    # Start from earliest needed point and make sure the sequence covers the entire data range
    theo_seq <- seq(from = min_data - beat_period, to = max_data + beat_period, by = beat_period) + shift
    
    # For each observation, find the smallest absolute deviation to the theoretical points
    deviations <- sapply(data_ugof, function(obs) {
      min(abs(obs - theo_seq))
    })
    
    # Root-mean-square deviation (RMSD)
    rmsd_values[i] <- sqrt(mean(deviations^2))
  }
  
  # Find optimal shift
  best_shift_idx <- which.min(rmsd_values)
  best_shift <- phase_shifts[best_shift_idx]
  
  # Generate final theoretical time sequence
  theotime_seq <- data.frame(time = seq(from = min_data - beat_period, to = max_data + beat_period, by = beat_period) + best_shift)
  
  # combine all rmsd values with the corresponding phase shifts for plot in server function 
  rmsd_data <- data.frame(phase_shift = phase_shifts, rmsd = rmsd_values)
  
  # Generate plot
  rmsd_plot <- ggplot(data.frame(phase_shift = phase_shifts, rmsd = rmsd_values),
                      aes(x = phase_shift, y = rmsd)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(aes(x = best_shift, y = rmsd_values[best_shift_idx]), color = "red", size = 2) +
    labs(
      title = "Phase Shift Optimization",
      x = "Phase Shift (s)",
      y = "RMS Deviation"
    ) +
    theme_minimal()
  
  return(list(
    best_shift = best_shift,
    theotime_seq = theotime_seq,
    rmsd_plot = rmsd_plot,
    rmsd_data = rmsd_data  
  ))
}
