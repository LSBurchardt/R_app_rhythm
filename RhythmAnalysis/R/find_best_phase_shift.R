# function for finding best phase shift of ioi beat
# minimizing deviations

# author: Lara S. Burchardt with help of ChatGPT

library(ggplot2)

find_best_phase_shift <- function(data_ugof, beat_period, step_size = 0.01) {
  
  # Create shifts (from 0 to one full beat cycle)
  phase_shifts <- seq(0, beat_period, by = step_size)
  maxoriginal <- max(data_ugof)
  
  # Initialize vector to store RMSD per shift
  rmsd_values <- numeric(length(phase_shifts))
  
  for (i in seq_along(phase_shifts)) {
    
    shift <- phase_shifts[i]
    # make sure we start shifting the phase such, that all data points are included
    min_obs <- min(data_ugof)
    max_obs <- max(data_ugof)
    
    # Start beat sequence slightly before the earliest observation
    start_time <- shift + floor((min_obs - shift) / beat_period) * beat_period
    end_time   <- shift + ceiling((max_obs - shift) / beat_period) * beat_period
    
    theo_seq <- seq(from = start_time, to = end_time, by = beat_period)
    
    # For each observation, find the closest theoretical point
    deviations <- sapply(data_ugof, function(obs) {
      min(abs(obs - theo_seq))  # raw deviation
    })
    
    # Root-mean-square deviation (RMSD)
    rmsd_values[i] <- sqrt(mean(deviations^2))
  }
  
  # Find optimal shift
  best_shift_idx <- which.min(rmsd_values)
  best_shift <- phase_shifts[best_shift_idx]
  
  # Generate final theoretical time sequence
  theotime_seq <- data.frame(time = seq(from = best_shift, to = maxoriginal + beat_period, by = beat_period))
  
  # Generate plot
  #rmsd_plot <- ggplot(data.frame(phase_shift = phase_shifts, rmsd = rmsd_values),
  #                    aes(x = phase_shift, y = rmsd)) +
  #  geom_line(color = "steelblue", linewidth = 1) +
  #  geom_point(aes(x = best_shift, y = rmsd_values[best_shift_idx]), color = "red", size = 2) +
  #  labs(
  #    title = "Phase Shift Optimization",
  #    x = "Phase Shift (s)",
  #    y = "RMS Deviation"
  #  ) +
  #  theme_minimal()
  
  return(list(
    best_shift = best_shift,
    theotime_seq = theotime_seq,
    #rmsd_plot = rmsd_plot,
    rmsd_data = data.frame(phase_shift = phase_shifts,
                           rmsd = rmsd_values)
  ))
}
