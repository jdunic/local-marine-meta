fl_combined <- readr::read_csv("Data_outputs/fl_combined.csv")


# Effect size (Richness)
ggplot(fl_combined, aes(x = yi.SppR.ROM)) +
  geom_histogram() + 
  xlab('Log Ratio') + 
  ylab('Count')

# Effect size (Shannon diveristy)
ggplot(fl_combined, aes(x = yi.Shan.ROM)) +
  geom_histogram() + 
  xlab('Log Ratio') + 
  ylab('Count')

ggplot(fl_combined, aes(x = mean_imps)) +
  geom_histogram() + 
  xlab('Human cumulative impacts') + 
  ylab('Count')

ggplot(fl_combined, aes(x = mean_invs)) +
  geom_histogram() + 
  xlab('Invasive potential') + 
  ylab('Count')

ggplot(fl_combined, aes(x = mean_nuts)) +
  geom_histogram() + 
  xlab('Nutrients') + 
  ylab('Count')

# Study duration specific data
ggplot(fl_combined, aes(x = mean_lin_change)) +
  geom_histogram() + 
  xlab('Linear temperature change (per decade)') + 
  ylab('Count')

# Study duration specific data
ggplot(fl_combined, aes(x = mean_vocc)) +
  geom_histogram() + 
  xlab('Climate Velocity') + 
  ylab('Count')

# Single duration: 1935 - 
ggplot(fl_combined, aes(x = single_lin_change)) +
  geom_histogram() + 
  xlab('Linear temperature change (per decade)') + 
  ylab('Count')

# Study duration specific data
ggplot(fl_combined, aes(x = mean_vocc)) +
  geom_histogram() + 
  xlab('Climate Velocity') + 
  ylab('Count')