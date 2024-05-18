library(tidyverse)
library(gt)
library(DT)
library(xgboost)
library(caret)
library(Boruta)
library(pROC)
library(Metrics)
library(factoextra)
set.seed(2004)
options(scipen = 999)

## Read in data ----------
TM_2022 <- read_csv("TM_2022_All.csv") %>%
  filter(Level == "D1")
TM_2023 <- read_csv("TM_D1_Complete.csv") %>%
  filter(Level == "D1")
TM_2023 <- TM_2023[,1:167]

D1_Data <- rbind(TM_2022, TM_2023)
rm(TM_2022, TM_2023)

unique(D1_Data$TaggedPitchType)

filtered_data <- D1_Data %>%
  filter(TaggedPitchType %in% c("Fastball", "ChangeUp", "Curveball", "Slider", "Sinker", "Cutter", "Splitter"))
rm(D1_Data)

Fastball_Data <- filtered_data %>%
  group_by(Pitcher, PitcherThrows, PitcherTeam, Date) %>%
  filter(TaggedPitchType %in% c("Fastball", "Sinker")) %>%
  summarise(
    Fastball_Velo = mean(RelSpeed, na.rm = T),
    Fastball_HB = mean(HorzBreak, na.rm = T),
    Fastball_IVB = mean(InducedVertBreak, na.rm = T)
  )

filtered_data <- filtered_data %>%
  inner_join(Fastball_Data, by = c("Pitcher", "PitcherThrows", "PitcherTeam", "Date"))

unique(filtered_data$TaggedPitchType)
unique(filtered_data$PitchCall)

filtered_data <- filtered_data %>%
  mutate(TaggedPitchType = if_else(TaggedPitchType == "Splitter", "ChangeUp", TaggedPitchType)) %>%
  filter(TaggedPitchType %in% c("Fastball", "Curveball", "ChangeUp", "Cutter", "Sinker", "Slider")) %>%
  mutate(
    is_Swing = if_else(PitchCall %in% c("InPlay", "FoulBall", "StrikeSwinging"),1,0),
    is_Whiff = if_else(PitchCall == "StrikeSwinging", 1, ifelse(PitchCall %in% c("InPlay", "FoulBall"), 0, NA)),
    is_Foul = if_else(PitchCall %in% c("FoulBall", "FoulBallFieldable", "FoulBallNotFieldable"), 1, 0),
    is_BBE = if_else(PitchCall == "InPlay", 1, 0),
    is_CalledStrike = if_else(PitchCall == "StrikeCalled", 1, 0),
    is_Groundball = if_else(is_BBE == 1 & Angle < 10, 1, 0),
    is_Linedrive = if_else(is_BBE == 1 & between(Angle, 10, 25), 1, 0),
    is_Flyball = if_else(is_BBE == 1 & between(Angle, 25, 50), 1, 0),
    is_Popup = if_else(is_BBE == 1 & Angle > 50, 1, 0),
    is_HardHit = if_else(is_BBE == 1 & ExitSpeed >= 95, 1, 0),
    is_SweetSpot = if_else(is_BBE == 1 & between(Angle, 8, 32), 1, 0),
    is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), 1, 0),
    BatterLoR = if_else(BatterSide == "Right", 1, 0),
    PitcherLoR = if_else(PitcherThrows == "Right",1,0),
    PlatoonAdv = if_else(BatterLoR == PitcherLoR, 1, 0),
    HB_adjusted = if_else(PitcherThrows == "Right", HorzBreak, -HorzBreak),
    RelSide_adjusted = if_else(PitcherThrows == "Right", RelSide, -RelSide),
    FB_Velo_Diff = if_else(TaggedPitchType %in% c("Curveball", "ChangeUp", "Slider", "Cutter"), RelSpeed - Fastball_Velo, NA),
    FB_HB_Diff = if_else(TaggedPitchType %in% c("Curveball", "ChangeUp", "Slider", "Cutter"), HorzBreak - Fastball_HB, NA),
    FB_IVB_Diff = if_else(TaggedPitchType %in% c("Curveball", "ChangeUp", "Slider", "Cutter"), InducedVertBreak - Fastball_IVB, NA),
  )

filtered_data <- setup_woba_c(filtered_data)
filtered_data <- setup_pa_event_c(filtered_data)
filtered_data <- add_run_values_c(filtered_data)
  
Model_Data_Ready <- filtered_data %>%
  select(
    Date, Pitcher, PitcherTeam, Inning, Balls, Strikes, PitchCall, Outs, PitcherThrows, Batter, BatterSide, BatterTeam, TaggedPitchType,
    RelSpeed, PlateLocHeight, PlateLocSide, HB_adjusted, InducedVertBreak, SpinRate, SpinAxis, RelSide_adjusted, 
    RelHeight, Extension, FB_Velo_Diff, FB_HB_Diff, FB_IVB_Diff, Type, woba_denom,
    woba_value, Event, Count, new_Count, Run_Value, is_Swing, is_Whiff, is_Foul, is_BBE, is_CalledStrike, is_Groundball, is_Linedrive, is_Flyball, is_Popup, is_HardHit, 
    is_SweetSpot, BatterLoR, PitcherLoR, PlatoonAdv,
    ExitSpeed, Angle, PlayResult, gameID, Catcher
  ) %>%
  filter(
    !is.na(RelSpeed),
    !is.na(InducedVertBreak),
    !is.na(HB_adjusted),
    !is.na(RelHeight),
    !is.na(RelSide_adjusted),
    !is.na(Extension),
    !is.na(SpinAxis)
  )

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Fastballs and Sinkers - fastballs
Fastball_Data_Boruta <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("Fastball", "Sinker")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinRate, SpinAxis, RelHeight, RelSide_adjusted, Extension, PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv, 
    is_Swing
  ) %>%
  drop_na()

Fastball_Boruta <- Boruta(
  as.factor(is_Swing) ~ RelSpeed + InducedVertBreak + HB_adjusted + SpinRate + SpinAxis + RelHeight + RelSide_adjusted + Extension +
                      PlateLocHeight + PlateLocSide + Balls + Strikes + PlatoonAdv,
  data = sample_n(Fastball_Data_Boruta, 30000)
)  

Fastball_Feature_Importance <- as.data.frame(attStats(Fastball_Boruta))
Fastball_Data <- Fastball_Feature_Importance %>% 
  select(meanImp, decision)
Fastball_Features <- unlist(rownames(Fastball_Data)) 
Fastball_Graph_Data <- cbind(Fastball_Data, Fastball_Features)

ggplot(Fastball_Graph_Data, aes(reorder(Fastball_Features, meanImp), meanImp, color = decision)) +
  geom_point(stat = "identity") +
  labs(
    title = "Fastball Variable Importance Plot (xSwing)",
    subtitle = "Trained on 2022 and 2023 D1 Trackman Data",
    x = "Variable",
    y = "Average Importance"
  )

# Sliders, Curveballs, and Cutters - breaking ball
BreakingBall_Data_Boruta <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("Slider", "Curveball", "Cutter")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinRate, SpinAxis, RelHeight, RelSide_adjusted, Extension, 
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv, 
    FB_Velo_Diff, FB_HB_Diff, FB_IVB_Diff,
    is_Swing
  ) %>%
  drop_na()

BreakingBall_Boruta <- Boruta(
  as.factor(is_Swing) ~ RelSpeed + InducedVertBreak + HB_adjusted + SpinRate + SpinAxis + RelHeight + RelSide_adjusted + Extension +
                      PlateLocHeight + PlateLocSide + Balls + Strikes + PlatoonAdv,
                      FB_Velo_Diff + FB_IVB_Diff + FB_HB_Diff, 
  data = sample_n(BreakingBall_Data_Boruta, 30000)
)

BreakingBall_Feature_Importance <- data.frame(attStats(BreakingBall_Boruta))
Fastball_Data <- feature_importanceBB %>% 
  select(meanImp, decision)
BreakingBall_Features <- unlist(rownames(Fastball_Data)) 
BreakingBall_Graph_Data <- cbind(Fastball_Data, BreakingBall_Features)

ggplot(BreakingBall_Graph_Data, aes(reorder(BreakingBall_Features, meanImp), meanImp, color = decision)) +
  geom_point(stat = "identity") +
  labs(
    title = "Breaking Ball Variable Importance Plot (xSwing)",
    subtitle = "Trained on 2022 and 2023 D1 Trackman Data",
    x = "Variable",
    y = "Average Importance"
  )

# Changeups and splitters - Off-speed 
Offspeed_Boruta_Data <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("ChangeUp")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinRate, SpinAxis, RelHeight, RelSide_adjusted, Extension, 
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv, 
    FB_Velo_Diff, FB_HB_Diff, FB_IVB_Diff,
    is_Swing
  ) %>%
  drop_na()


Offspeed_Boruta <- Boruta(
  as.factor(is_Swing) ~ RelSpeed + InducedVertBreak + HB_adjusted + SpinRate + SpinAxis + RelHeight + RelSide_adjusted + Extension +
                      PlateLocHeight + PlateLocSide + Balls + Strikes + PlatoonAdv,
                      FB_Velo_Diff + FB_IVB_Diff + FB_HB_Diff, 
  data = sample_n(Offspeed_Boruta_Data, 30000)
)

Offspeed_Feature_Importance <- data.frame(attStats(Offspeed_Boruta))
Offspeed_Data <- Offspeed_Feature_Importance %>% 
  select(meanImp, decision)
Ofspeed_Features <- unlist(rownames(Offspeed_Data)) 
Offspeed_Graph_Data <- cbind(Offspeed_Data, Offspeed_Features)

ggplot(Offspeed_Graph_Data, aes(reorder(Ofspeed_Features, meanImp), meanImp, color = decision)) +
  geom_point(stat = "identity") +
  labs(
    title = "Off-speed Boruta Importance Plot (xSwing)",
    subtitle = "Trained on 2022 and 2023 D1 Trackman Data",
    x = "Variable",
    y = "Average Importance"
  )


## Modeling

Fastball <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("Fastball", "Sinker")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinAxis, SpinRate,
    RelHeight, RelSide_adjusted, Extension,
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv,
    is_Swing
  )
BreakingBall <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("Slider", "Curveball", "Cutter")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinAxis, SpinRate,
    FB_Velo_Diff, FB_IVB_Diff, FB_HB_Diff,
    RelHeight, RelSide_adjusted, Extension,
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv,
    is_Swing
  )
Offspeed <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("ChangeUp")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinAxis, SpinRate,
    FB_Velo_Diff, FB_IVB_Diff, FB_HB_Diff,
    RelHeight, RelSide_adjusted, Extension,
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv,
    is_Swing
  )

unique(Fastball$is_Swing)
unique(BreakingBall$is_Swing)
unique(Offspeed$is_Swing)

# Fastball xSwing Model
Fastball_Partition <- createDataPartition(Fastball$is_Swing, p = .75, list = FALSE, times = 1)
Fastball_Train <- Fastball[trainIndexFF,]
Fastball_Test <- Fastball[-trainIndexFF,]
Fastball_Train_x <- data.matrix(Fastball_Train[, -14])
Fastball_Train_y <- as.numeric(unlist(Fastball_Train[,14]))

Fastball_Train_d <- xgb.DMatrix(data = Fastball_Train_x, label = Fastball_Train_y)
Fastball_Model <- xgboost(
  data = Fastball_Train_d, max.depth = 3, eta = 0.1, nrounds = 300, eval_metric = "auc", objective = "binary:logistic"
)
Fastball_Test_x <- data.matrix(Fastball_Test[, -14])
Fastball_Test_y <- as.numeric(unlist(Fastball_Test[,14]))

xSwing_Pred <- predict(Fastball_Model, Fastball_Test_x)
Fastball_wPred <- cbind(Fastball_Test, xSwing_Pred)

auc_value <- auc(Fastball_wPred$is_Swing, Fastball_wPred$xSwing_Pred)
Fastball_ROC <- roc(Fastball_wPred$is_Swing, Fastball_wPreds$xSwing_Pred)
ggroc(Fastball_ROC) +
  labs(
    title = "Fastball xSwing ROC Curve with Area Under Curve Value",
    subtitle = paste("AUC:", auc_value),
    x = "Specificity",
    y = "Sensitivity"
  )

## Sliders, Curves, Cutters xSwing Model
BreakingBall_Partition <- createDataPartition(BreakingBall$is_Swing, p = .75, list = FALSE, times = 1)
BreakingBall_Train <- BreakingBall[BreakingBall_Partition,]
BreakingBall_Test <- BreakingBall[-BreakingBall_Partition,]
BreakingBall_Train_x <- data.matrix(BreakingBall_Train[, -17])
BreakingBall_Train_y <- as.numeric(unlist(BreakingBall_Train[,17]))

BreakingBall_Train_d <- xgb.DMatrix(data = BreakingBall_Train_x, label = BreakingBall_Train_y)
BreakingBall_Model <- xgboost(
  data = BreakingBall_Train_d, max.depth = 3, eta = 0.1, nrounds = 300, eval_metric = "auc", objective="binary:logistic"
)
BreakingBall_Test_x <- data.matrix(BreakingBall_Test[, -17])
BreakingBall_Test_y <- as.numeric(unlist(BreakingBall_Test[,17]))

xSwing_Pred <- predict(BreakingBall_Model, BreakingBall_Test_x)
BreakingBall_wPred <- cbind(BreakingBall_Test, xSwing_Pred)

auc_value <- auc(BreakingBall_wPred$is_Swing, BreakingBall_wPred$xSwing_Pred)
BreakingBall_ROC <- roc(BreakingBall_wPred$is_Swing, BreakingBall_wPred$xSwing_Pred)
ggroc(BreakingBall_ROC) +
  labs(
    title = "Breaking Ball xSwing ROC Curve with Area Under Curve Value",
    subtitle = paste("AUC:", auc_value),
    x = "Specificity",
    y = "Sensitivity"
  )

# Changeups and Splitters xSwing Model
Offspeed_Partition <- createDataPartition(Offspeed$is_Swing, p = .75, list = FALSE, times = 1)
Offspeed_Train <- Offspeed[Offspeed_Partition,]
Offspeed_Test <- Offspeed[-Offspeed_Partition,]
Offspeed_Train_x <- data.matrix(Offspeed_Train[, -17])
Offspeed_Train_y <- as.numeric(unlist(Offspeed_Train[,17]))

Offspeed_Train_d <- xgb.DMatrix(data = Offspeed_Train_x, label = Offspeed_Train_y)
Offspeed_Model <- xgboost(
  data = Offspeed_Train_d, max.depth = 4, eta = 0.1, nrounds = 300, eval_metric = "auc", objective = "binary:logistic"
)
Offspeed_Test_x <- data.matrix(Offspeed_Test[, -17])
Offspeed_Test_y <- as.numeric(unlist(Offspeed_Test[,17]))

xSwing_Pred <- predict(Offspeed_Model, Offspeed_Test_x)
Offspeed_wPred <- cbind(Offspeed_Test, xSwing_Pred)

auc_value <- auc(Offspeed_wPred$is_Swing, Offspeed_wPred$xSwing_Pred)
Offspeed_ROC <- roc(Offspeed_wPred$is_Swing, Offspeed_wPred$xSwing_Pred)
ggroc(Offspeed_ROC) +
  labs(
    title = "Offspeed xSwing ROC Curve with Area Under Curve Value",
    subtitle = paste("AUC:", auc_value),
    x = "Specificity",
    y = "Sensitivity"
  )

# Save the Models
saveRDS(Fastball_Model, "xSwing_Fastball_Final.rda")
saveRDS(BreakingBall_Model, "xSwing_Breaking_Final.rda")
saveRDS(Offspeed_Model, "xSwing_Offspeed_Final.rda")

# Applying model to rest of 2022/2023 data

#Fastball
Fastball <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("Fastball", "Sinker")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinAxis, SpinRate,
    RelHeight, RelSide_adjusted, Extension,
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv,
    is_Swing
  )

Fastball_sans_Swing <- Fastball %>%
  select(-is_Swing)

Fastball_Matrix <- as.matrix(Fastball_sans_Swing)
xSwing_Pred <- predict(Fastball_Model, Fastball_Matrix)
Fastball <- cbind(Fastball, xSwing_Pred)

#Breaking Ball
BreakingBall <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("Slider", "Curveball", "Cutter")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinAxis, SpinRate,
    FB_Velo_Diff, FB_IVB_Diff, FB_HB_Diff,
    RelHeight, RelSide_adjusted, Extension,
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv,
    is_Swing
  )

BreakingBall_sans_Swing <- BreakingBall %>%
  select(-is_Swing)

BreakingBall_Matrix <- as.matrix(BreakingBall_sans_Swing)
xSwing_Pred <- predict(BreakingBall_Model, BreakingBall_Matrix)
BreakingBall <- cbind(BreakingBall, xSwing_Pred)

#Offspeed
Offspeed <- Model_Data_Ready %>%
  filter(TaggedPitchType %in% c("ChangeUp")) %>%
  select(
    RelSpeed, InducedVertBreak, HB_adjusted, SpinAxis, SpinRate,
    FB_Velo_Diff, FB_IVB_Diff, FB_HB_Diff,
    RelHeight, RelSide_adjusted, Extension,
    PlateLocHeight, PlateLocSide, Balls, Strikes, PlatoonAdv,
    is_Swing
  )

Offspeed_sans_Swing <- Offspeed %>%
  select(-swing)

Offspeed_Matrix <- as.matrix(Offspeed_sans_Swing)
xSwing_Pred <- predict(Offspeed_Model, Offspeed_Matrix)
Offspeed <- cbind(Offspeed, xSwing_Pred)

## Combine all
Fastball <- Fastball %>%
  select(
    RelSpeed, HB_adjusted, InducedVertBreak, SpinRate, SpinAxis, RelSide_adjusted, 
    RelHeight, Extension, PlateLocHeight, PlateLocSide, PlatoonAdv, Balls, Strikes, is_Swing, xSwing_Pred
  )
BreakingBall <- BreakingBall %>%
  select(
    RelSpeed, HB_adjusted, InducedVertBreak, SpinRate, SpinAxis, RelSide_adjusted, 
    RelHeight, Extension, PlateLocHeight, PlateLocSide, PlatoonAdv, Balls, Strikes, is_Swing, xSwing_Pred
  )
Offspeed <- Offspeed %>%
  select(
    RelSpeed, HB_adjusted, InducedVertBreak, SpinRate, SpinAxis, RelSide_adjusted, 
    RelHeight, Extension, PlateLocHeight, PlateLocSide, PlatoonAdv, Balls, Strikes, is_Swing, xSwing_Pred
  )

xSwing <- rbind(Fastball, BreakingBall, Offspeed)
rm(Fastball, BreakingBall, Offspeed)

xSwing_Combined <- inner_join(xSwing, Model_Data_Ready)
rm(xSwing)

# Save xSwing CSV
write_csv(xSwing_Combined, "xSwing_CSV_Final.csv")


## Some EDA -------------------------------------------------------------------------------------------------------------------------

# Highest Swing-xSwing diff pitches for hitters
xSwing_Combined %>%
  filter(BatterTeam == "CAL_FUL") %>%
  mutate(
    xSwingDiff = is_Swing - xSwing_Pred,
   `xSwing%` = round(100*xSwing_Pred,2),
    Count = paste0(Balls,"-",Strikes)
  ) %>%
  select(
    Date, Pitcher, PitcherTeam, Batter, BatterTeam, Inning, Count, PitchCall, is_Swing, `xSwing%`, xSwingDiff
  ) %>%
  arrange(desc(xSwingDiff)) %>%
  head(10)

# Highest Swing-xSwing diff pitches for pitchers
xSwing_Combined %>%
  filter(PitcherTeam == "CAL_FUL") %>%
  mutate(
    xSwingDiff = is_Swing - xSwing_Pred,
   `xSwing%` = round(100*xSwing_Pred,2),
    Count = paste0(Balls,"-",Strikes)
  ) %>%
  select(
    Date, Pitcher, PitcherTeam, Batter, BatterTeam, Inning, Count, PitchCall, is_Swing, `xSwing%`, xSwingDiff
  ) %>%
  arrange(desc(xSwingDiff)) %>%
  head(10)

# highest swing diff at the player level instead of the pitch level
xSwing_Combined %>%
  filter(
    PitcherTeam == "CAL_FUL",
    str_sub(xSwing_Combined$Date,1,4)==2023
  ) %>%
  group_by(Pitcher) %>%
  summarise(
    Pitches = n(),
    `Swing%` = round(100*sum(swing)/Pitches,2),
    `xSwing%` = round(100*sum(xSwing_Pred)/Pitches,2)
  ) %>%
  mutate(SwingDiff = `Swing%` - `xSwing%`) %>%
  filter(Pitches >= 100) %>%
  arrange(desc(SwingDiff))

# xChase% -----------

# Cal State Fullerton
xSwing_Combined %>%
  filter(
    BatterTeam == "CAL_FUL",
    str_sub(xSwing_Combined$Date,1,4)==2023
  ) %>%
  mutate(is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85),T,F)) %>%
  filter(is_Zone == F) %>%
  group_by(Batter) %>%
  summarise(
    `OOZ-Pitches` = n(),
    `Chase%` = (sum(is_Swing)/`OOZ-Pitches`),
    `xChase%` = (sum(xSwing_Pred)/`OOZ-Pitches`)
  ) %>%
  mutate(ChaseDiff = (`Chase%` - `xChase%`)) %>%
  filter(`OOZ-Pitches` >= 15) %>%
  arrange((ChaseDiff)) %>%
  gt() %>%
  data_color(
    `Chase%`,
    method = "quantile",
    quantiles = 14,
    palette = c("red", "white", "blue")
  ) %>%
  data_color(
    ChaseDiff,
    method = "quantile",
    quantiles = 14,
    palette = c("red", "white", "blue")
  ) %>%
  fmt_percent(`Chase%`:ChaseDiff, decimals = 1) %>%
  tab_header(
    title = "Biggest Overperformers in xChase%",
    subtitle = "CSUF Hitters 2023"
  )

# LSU
xSwing_Combined %>%
  filter(
    BatterTeam == "LSU_TIG",
    str_sub(xSwing_Combined$Date,1,4)==2023
  ) %>%
  mutate(is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85),T,F)) %>%
  filter(is_Zone == F) %>%
  group_by(Batter) %>%
  summarise(
    `OOZ-Pitches` = n(),
    `Chase%` = (sum(swing)/`OOZ-Pitches`),
    `xChase%` = (sum(pred)/`OOZ-Pitches`)
  ) %>%
  mutate(ChaseDiff = (`Chase%` - `xChase%`)) %>%
  filter(`OOZ-Pitches` >= 100) %>%
  arrange((ChaseDiff)) %>%
  gt() %>%
  data_color(
    `Chase%`,
    method = "quantile",
    quantiles = 14,
    palette = c("red", "white", "blue")
  ) %>%
  data_color(
    ChaseDiff,
    method = "quantile",
    quantiles = 14,
    palette = c("red", "white", "blue")
  ) %>%
  fmt_percent(`Chase%`:ChaseDiff, decimals = 1) %>%
  tab_header(
    title = "Biggest Overperformers in xChase%",
    subtitle = "LSU Hitters 2023"
  )

# UCSB
xSwing_Combined %>%
  filter(
    BatterTeam == "SAN_GAU",
    str_sub(xSwing_Combined$Date,1,4)==2023
  ) %>%
  mutate(is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85),T,F)) %>%
  filter(is_Zone == F) %>%
  group_by(Batter) %>%
  summarise(
    `OOZ-Pitches` = n(),
    `Chase%` = (sum(swing)/`OOZ-Pitches`),
    `xChase%` = (sum(pred)/`OOZ-Pitches`)
  ) %>%
  mutate(ChaseDiff = (`Chase%` - `xChase%`)) %>%
  filter(`OOZ-Pitches` >= 100) %>%
  arrange((ChaseDiff)) %>%
  gt() %>%
  data_color(
    `Chase%`,
    method = "quantile",
    quantiles = 14,
    palette = c("red", "white", "blue")
  ) %>%
  data_color(
    ChaseDiff,
    method = "quantile",
    quantiles = 14,
    palette = c("red", "white", "blue")
  ) %>%
  fmt_percent(`Chase%`:ChaseDiff, decimals = 1) %>%
  tab_header(
    title = "Biggest Overperformers in xChase%",
    subtitle = "UCSB Hitters 2023"
  )

# Top Performers Overall
xSwing_Combined %>%
  filter(str_sub(xSwing_Combined$Date,1,4)==2023) %>%
  mutate(is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85),T,F)) %>%
  filter(is_Zone == F) %>%
  group_by(Batter, BatterTeam) %>%
  summarise(
    `OOZ-Pitches` = n(),
    `Chase%` = (sum(swing)/`OOZ-Pitches`),
    `xChase%` = (sum(pred)/`OOZ-Pitches`)
  ) %>%
  mutate(ChaseDiff = (`Chase%` - `xChase%`)) %>%
  filter(`OOZ-Pitches` >= 200) %>%
  arrange((ChaseDiff)) %>%
  head(15) %>%
  as_tibble() %>%
  gt() %>%
  fmt_percent(`Chase%`:ChaseDiff, decimals = 1) %>%
  tab_header(title = "Biggest Overperformers in xChase%",
             subtitle = "Overall D1 2023")


## xSwing in the heart of the zone vs true swing
xSwing_Combined %>%
  filter(str_sub(xSwing_Combined$Date,1,4)==2023) %>%
  mutate(is_Heart = if_else(between(PlateLocHeight, 1.83, 3.17) & between(PlateLocSide, -0.56, 0.56),T,F)) %>%
  filter(is_Heart == T) %>%
  group_by(Batter, BatterTeam) %>%
  summarise(
    HeartPitches = n(),
    `Heart-Swing%` = mean(is_Swing, na.rm = T),
    `Heart-xSwing%` = mean(xSwing_Pred, na.rm = T)
  ) %>%
  mutate(SwingDiff = `Heart-Swing%` - `Heart-xSwing%`) %>%
  filter(HeartPitches >= 150) %>%
  arrange(desc(SwingDiff)) %>%
  head(15) %>%
  as_tibble() %>%
  gt() %>%
  fmt_percent(`Heart-Swing%`:SwingDiff, decimals = 1) %>%
  tab_header(
    title = "Most Aggresive Hitters in the Heart Zone",
    subtitle = "Overall D1 2023"
  )
