# is beat precision symmetrical?
# 08.04.2024, Lara S. Burchardt 

# original ugof calculations from server.R -----

#data_ugof <- data$X1
data_ugof <- data$X1
beat_ioi <- 10 # in Hz
#beat_ioi <- results_rhythm[a,2]
#beat_fft <- results_rhythm[a,5]

# calculate goodness-of-fit for IOI analysis and Fourier analysis

maxoriginal <- max(data_ugof)
timesteps <- 1000/round(beat_ioi, digits = 1)
count <- 0
theotime_value <- 0
theotime_seq <- data.frame()

while (theotime_value < maxoriginal){
  
  count <- count + 1;
  theotime_value <- count * timesteps /1000;
  theotime_seq[count,1] <- theotime_value;
  
}

# match original sequence to theoretical timeseries and calculate actual deviation
x <- length(data_ugof)
min_value <- c(1:x)
ugof_value_beat <- c()

for (n in 1:x){
  
  min_value[n] <- min(abs(data_ugof[n]- theotime_seq))
  
}

# calculate uGof

maxdev <- timesteps/2/1000;

ugof_value_beat <- min_value/maxdev;
# next line is new:
ugof_value_beat <- as.data.frame(ugof_value_beat)

m_ugof_beat_1 <- median(ugof_value_beat[2:length(ugof_value_beat)])

# adjustments ----



# Define the beat parameters
beat_frequency <- 10  # in Hz
beat_length <- 1 / beat_frequency  # in seconds
delta_max <- beat_length / 2  # in seconds

seq_duration <- 1 # in seconds

# Calculate the number of timesteps
timesteps <- 1000 / round(beat_frequency, digits = 1)

# Generate the first time series (beat)
#beat <- seq(0, beat_length, by = 1/timesteps)  # Assuming a time resolution of 1/timesteps
beat <- seq(0, seq_duration, by = beat_length)

# Initialize vectors to store beat precision and lags
beat_precision <- numeric()
lags <- numeric()

# Number of iterations
num_iterations <- 1000

# Maximum lag (double the beat length)
max_lag <- 0.2

# Loop through different lags
for (i in 1:num_iterations) {
  # Calculate lag
  lag <- max_lag / num_iterations * (i - 1)
  
  # Generate the second time series with lag
  beat_second <- seq(lag, lag + beat_length, by = 1/timesteps)
  
  # Calculate deviations between elements of the first time series and the closest element in the second time series
  deviations <- abs(outer(beat, beat_second, "-"))
  
  # Scale deviations to compute beat precision
  scaled_deviations <- deviations / (beat_length / 2 / 1000)  # Assuming timesteps are in milliseconds
  
  # Calculate mean scaled deviation for each element in the first time series
  mean_scaled_deviations <- apply(scaled_deviations, 1, mean)
  
  # Store beat precision and lag
  beat_precision <- c(beat_precision, mean_scaled_deviations)
  lags <- c(lags, lag)
}

# Create a data frame to store beat precision and lags
beat_df <- data.frame(lag = rep(lags, each = length(beat)), beat_precision = beat_precision)

# Plot beat precision as a function of lag
plot(beat_df$lag, beat_df$beat_precision, type = "l", xlab = "Lag (seconds)", ylab = "Beat Precision")



# version 2 -----


# Define the beat parameters
beat_frequency <- 10  # in Hz
beat_length <- 1 / beat_frequency  # in seconds
delta_max <- beat_length / 2  # in seconds


# Generate the first time series (beat)
#beat <- seq(0, beat_length, by = 0.01)  # Assuming a time resolution of 10 ms
beat <- seq(0, seq_duration, by = beat_length)


# Initialize vectors to store beat precision and lags
beat_precision <- numeric()
lags <- numeric()

# Loop through different lags
num_iterations <- 1000

# Maximum lag (double the beat length)
max_lag <- 1

# Loop through different lags
for (i in 1:num_iterations) {
  # Calculate lag
  lag <- max_lag / num_iterations * (i - 1)
  # Generate the second time series with lag
  beat_second <- seq(lag, lag + beat_length, by = 0.01)
  
  # Calculate deviations between elements of the first time series and the closest element in the second time series
  deviations <- abs(outer(beat, beat_second, "-"))
  
  # Calculate beat precision
  beat_prec <- delta_max / mean(deviations)
  
  # Store beat precision and lag
  beat_precision <- c(beat_precision, beat_prec)
  lags <- c(lags, lag)
}

# Plot beat precision as a function of lag
plot(lags, beat_precision, type = "l", xlab = "Lag (seconds)", ylab = "Beat Precision")

plot(beat_precision, lags, type = "l", xlab = "Beat Precision", ylab = "Lag (seconds)" )

plot(deviations)
# version 3 ----

# Define the beat parameters
beat_frequency <- 10  # in Hz
beat_length <- 1 / beat_frequency  # in seconds

# Number of events in the beat sequence
num_events <- 100

# Number of iterations
num_iterations <- 1000

# Maximum lag (half the beat length)
#max_lag <- beat_length / 2
max_lag <- beat_length * 2

# Generate the beat sequence
beat <- seq(0, beat_length * (num_events - 1), by = beat_length)

# Initialize vectors to store beat precision and lags
beat_precision <- numeric()
lags <- seq(0, max_lag, length.out = num_iterations)

# Loop through different lags
for (lag in lags) {
  # Calculate the lagged beat sequence
  beat_second <- beat + lag
  
  # Calculate deviations between each event of beat_second and the closest event in beat
  deviations <- abs(outer(beat, beat_second, "-"))
  
  # Calculate the mean deviation
  mean_deviation <- mean(apply(deviations, 2, min))
  
  # Scale deviations to compute beat precision
  scaled_deviation <- mean_deviation / (beat_length / 2)  # Scaling to range from 0 to 1
  
  # Store beat precision
  beat_precision <- c(beat_precision, scaled_deviation)
}

# Create a data frame to store beat precision and lags
beat_df <- data.frame(lag = lags, beat_precision = beat_precision)

# Plot beat precision as a function of lag
plot(beat_df$lag, beat_df$beat_precision, type = "l", xlab = "Lag (seconds)", ylab = "Beat Precision")

