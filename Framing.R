library(tidyverse)
setwd("~/Documents/Hyannis")

# Read in Data, filter taken pitches
new_data <- read_csv("cape724.csv")
taken <- new_data %>% filter(PitchCall == "StrikeCalled" | PitchCall == "BallCalled")

# Model likelihood that a taken pitch is called a strike
library(mgcv)
strike_mod <- gam(PitchCall == "StrikeCalled" ~ s(PlateLocSide, PlateLocHeight), family = binomial, data = taken)
saveRDS(strike_mod, file = "strike_mod.rds")

# Predict whether each pitch will be called a strike
csv_hats <- strike_mod %>%
  augment(type.predict = "response", newdata = taken)

# Calculate Strikes Called Above Average and Framing Runs for each Catcher
catchers <- csv_hats %>% mutate(Strike = ifelse(PitchCall == "StrikeCalled", 1, 0)) %>% 
  mutate(StrikeVsAVG = Strike - `.fitted`) %>% select(Catcher, PlateLocSide, PlateLocHeight, PitchCall, Strike, StrikeVsAVG) %>% 
  group_by(Catcher) %>% summarize(StrikesAboveAVG = round(sum(StrikeVsAVG, na.rm = TRUE), 3), Pitches = n()) %>% arrange(-StrikesAboveAVG) %>%
  mutate(FramingRuns = round(0.153597 * StrikesAboveAVG, 2))
# Constant for value of called strike comes from Run Value Calculations

write_csv(catchers, "framing.csv")
