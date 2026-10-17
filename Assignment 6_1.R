
# ============================================================
# BIOLOG PLATE DATA ANALYSIS
# Tasks:
# 1. Clean data into tidy (long) format
# 2. Classify samples as Soil or Water
# 3. Plot dilution == 0.1
# 4. Calculate means of 3 replicates
# 5. Create an animated GIF of absorbance over time
# ============================================================


# 1. INSTALL AND LOAD PACKAGES -------------------------------

packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "gganimate",
  "gifski"
)

# Install packages that are not already installed.
installed <- rownames(installed.packages())

for (package in packages) {
  if (!package %in% installed) {
    install.packages(package)
  }
}

# Load packages.
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gganimate)
library(gifski)


# 2. READ THE CSV FILE ---------------------------------------

# Set your working directory to the folder containing the CSV,
# or replace this with the full path to your file.

input_file <- "BioLog_Plate_Data(1).csv"

raw_data <- read_csv(
  input_file,
  show_col_types = FALSE
)

# Check the original data.
print(head(raw_data))
print(dim(raw_data))


# 3. CLEAN THE DATA ------------------------------------------

# Convert column names to consistent R-friendly names.
# For example, "Sample ID" becomes "Sample_ID".

clean_data <- raw_data %>%
  rename(
    Sample_ID = `Sample ID`
  ) %>%
  mutate(
    Sample_ID = str_trim(Sample_ID),
    Substrate = str_trim(Substrate),
    Rep = as.integer(Rep),
    Dilution = as.numeric(Dilution)
  ) %>%
  # Remove rows without essential identifiers.
  filter(
    !is.na(Sample_ID),
    !is.na(Substrate),
    !is.na(Rep),
    !is.na(Dilution)
  ) %>%
  # Classify each sample.
  mutate(
    Sample_Type = case_when(
      str_detect(
        str_to_lower(Sample_ID), "soil"
      ) ~ "Soil",
      
      str_detect(
        str_to_lower(Sample_ID),
        "creek|water"
      ) ~ "Water",
      
      TRUE ~ "Unknown"
    ),
    Sample_Type = factor(
      Sample_Type,
      levels = c("Soil", "Water", "Unknown")
    )
  )

# Confirm that the sample classification worked.
print(
  clean_data %>%
    distinct(Sample_ID, Sample_Type)
)


# 4. CONVERT TO TIDY (LONG) FORMAT ---------------------------

# Each row will represent one measurement for one substrate,
# sample, replicate, dilution, and measurement time.

tidy_data <- clean_data %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  ) %>%
  mutate(
    # Convert Hr_24 into numeric time 24, for example.
    Time = as.numeric(str_remove(Time, "^Hr_")),
    Time = factor(
      Time,
      levels = c(24, 48, 144),
      labels = c("24 hours", "48 hours", "144 hours")
    )
  ) %>%
  arrange(
    Sample_ID, Dilution, Substrate, Rep, Time
  )

# Inspect the tidy data.
print(head(tidy_data))

# Save all cleaned measurements.
write_csv(
  tidy_data,
  "BioLog_Tidy_Data.csv"
)


# 5. FILTER TO DILUTION 0.1 ----------------------------------

data_01 <- tidy_data %>%
  filter(
    abs(Dilution - 0.1) < 1e-8
  )

# This keeps all samples, substrates, replicates, and time
# points at dilution 0.1 only.

print(
  data_01 %>%
    count(Sample_ID, Time)
)


# 6. CALCULATE MEAN ABSORBANCE -------------------------------

# Average the three replicates within each combination of:
# sample group + sample type + dilution + substrate + time.
#
# na.rm = TRUE allows a mean to be calculated when some
# replicates are missing. If all three are missing, the
# mean remains NA.

mean_data <- data_01 %>%
  group_by(
    Sample_ID,
    Sample_Type,
    Dilution,
    Substrate,
    Time
  ) %>%
  summarise(
    Mean_Absorbance = if (all(is.na(Absorbance))) {
      NA_real_
    } else {
      mean(Absorbance, na.rm = TRUE)
    },
    Replicate_Count = sum(!is.na(Absorbance)),
    .groups = "drop"
  )

# Save the means for later analysis.
write_csv(
  mean_data,
  "BioLog_Mean_Absorbance_Dilution_0.1.csv"
)


# 7. CREATE A STATIC PLOT ------------------------------------

# Plot mean absorbance by substrate for dilution 0.1.
# Separate panels show the three measurement times.
# Color identifies the original sample group.

static_plot <- ggplot(
  mean_data,
  aes(
    x = Substrate,
    y = Mean_Absorbance,
    fill = Sample_ID
  )
) +
  geom_col(
    position = position_dodge(width = 0.85),
    width = 0.8,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ Time,
    ncol = 1
  ) +
  labs(
    title = "BIOLOG Plate Absorbance at Dilution 0.1",
    subtitle = "Mean absorbance across available replicates",
    x = "Substrate",
    y = "Mean absorbance",
    fill = "Sample group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 65,
      hjust = 1,
      vjust = 1,
      size = 7
    ),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Display the plot.
print(static_plot)

# Save the static plot as a high-resolution PNG.
ggsave(
  filename = "BioLog_Dilution_0.1_Static.png",
  plot = static_plot,
  width = 16,
  height = 12,
  units = "in",
  dpi = 300
)


# 8. CREATE THE ANIMATED PLOT -------------------------------

# Use the same means as the static plot.
# Each frame represents one measurement time.
# The plot updates as time progresses from 24 to 48 to
# 144 hours.

animated_plot <- ggplot(
  mean_data,
  aes(
    x = Substrate,
    y = Mean_Absorbance,
    fill = Sample_ID
  )
) +
  geom_col(
    position = position_dodge(width = 0.85),
    width = 0.8,
    na.rm = TRUE
  ) +
  labs(
    title = "BIOLOG Plate Absorbance: {closest_state}",
    subtitle = "Dilution = 0.1 | Mean of three replicates",
    x = "Substrate",
    y = "Mean absorbance",
    fill = "Sample group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 65,
      hjust = 1,
      vjust = 1,
      size = 7
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  # Advance through the measurement times in order.
  transition_states(
    Time,
    transition_length = 1,
    state_length = 2,
    wrap = FALSE
  ) +
  # Smooth the transition between frames.
  ease_aes("linear")

# Render and save the animation.
animated_gif <- animate(
  animated_plot,
  nframes = 90,
  fps = 10,
  width = 1400,
  height = 900,
  renderer = gifski_renderer()
)

anim_save(
  "BioLog_Dilution_0.1_Animation.gif",
  animation = animated_gif
)


# 9. REPORT THE OUTPUTS --------------------------------------

cat("\nAnalysis complete!\n")
cat("Files created:\n")
cat("- BioLog_Tidy_Data.csv\n")
cat("- BioLog_Mean_Absorbance_Dilution_0.1.csv\n")
cat("- BioLog_Dilution_0.1_Static.png\n")
cat("- BioLog_Dilution_0.1_Animation.gif\n")
