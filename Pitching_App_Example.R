
## load in libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(gt)
library(ggtext)
library(ggdensity)
library(ggrepel)


## Load in example data -------
Example_Pitching_App_Data <- read_csv("Example_Pitching_App_Data.csv")

## Functions -----
Get_Overall_Pitch_Stats_2023 <- function(df, pitcher, date1, date2) {
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2)
  
  player_stats <- df %>%
    filter(
      PitchCall != "Undefined",
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    )%>%
    mutate(
      TopZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 2.9, 3.75), T, F),
      BotZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 1.35, 2.2), T, F),
      is_BBE = if_else(PitchCall == "InPlay", T, F),
      is_BBE_EV = if_else(PitchCall == "InPlay" & TaggedHitType != "Bunt", T, F),
      is_Swing = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "StrikeSwinging","InPlay"), T, F),
      is_Whiff = if_else(PitchCall == "StrikeSwinging", T, F),
      is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Strike = if_else(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay", "StrikeCalled"), T, F),
      is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85), T, F),
      is_SweetSpot = if_else(PitchCall == "InPlay" & between(Angle, 8, 32), T, F),
      is_HardHit = if_else(PitchCall == "InPlay" & ExitSpeed >= 95, T, F),
      is_CalledStrike = if_else(PitchCall == "StrikeCalled", T, F),
      is_Ball = if_else(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch"), T, F),
      is_Foul = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "FoulBallFieldable"), T, F),
      is_Hit = if_else(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), T, F),
      is_GB = if_else(Angle < 10, T, F),
      is_FB = if_else(between(Angle, 25, 50), T, F),
      is_Single = if_else(PlayResult == "Single", T, F),
      is_Double = if_else(PlayResult == "Double", T, F),
      is_Triple = if_else(PlayResult == "Triple", T, F),
      is_HR = if_else(PlayResult == "HomeRun", T, F),
      is_Error = if_else(PlayResult == "Error", T, F),
      is_Sac = if_else(PlayResult == "Sacrifice", T, F),
      is_Dos = if_else(PitchCall == "HitByPitch", T, F),
      is_Strikeout = if_else(Strikes == 2 & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Walk = if_else(Balls == 3 & PitchCall %in% c("BallCalled", "BallinDirt"), T, F),
      xwOBA_weights = xwOBACON_value + (is_Walk * 0.7) + (is_Dos * 0.7),
      is_AB = is_BBE + is_Strikeout - is_Sac,
      is_PA = is_BBE + is_Strikeout + is_Walk + is_Dos
    ) %>%
    group_by(Pitcher) %>%
    summarise(
      Pitches = n(),
      `Strike%` = round(mean(is_Strike, na.rm = T),5),
      `FPS%` = round(mean(is_Strike[PitchofPA == 1]),5),
      `CSW%` = round((mean(is_CSW, na.rm = T)),5),
      `Put-Away%` = round(mean(is_CSW[Strikes == 2], na.rm = T),5),
      `Whiff%` = round(mean(is_Whiff[is_Swing == TRUE], na.rm = T),5),
      `Zone%` = round(mean(is_Zone, na.rm = T),5),
      `Swing%` = round(mean(is_Swing, na.rm = T),5),
      `Chase%` = round(mean(is_Swing[is_Zone == FALSE], na.rm = TRUE),5),
      `Z-Swing%` = round(mean(is_Swing[is_Zone == TRUE], na.rm = T),5),
      `Z-Whiff%` = round(mean(is_Whiff[is_Zone == TRUE & is_Swing == TRUE]),5),
      BBE = sum(is_BBE, na.rm = T),
      wOBA = round(mean(woba_value[is_PA == TRUE], na.rm = TRUE), 3),
      wOBACON = round(mean(woba_value[is_BBE == TRUE], na.rm = TRUE), 3),
      xwOBACON = round(mean(xwOBACON_value[is_BBE == TRUE], na.rm = T),3),
      xwOBA = round(mean(xwOBA_weights[is_PA == TRUE], na.rm = TRUE), 3),
      `RV/100` = round(mean(Run_Value, na.rm = T) * 100, 2),
    ) %>%
    ## select(TaggedPitchType, Pitches, xwOBA, `CSW%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `Whiff%`, `Z-Swing%`, BBE, xwOBACON) %>%
    select(Pitches, `Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `RV/100`, BBE, wOBACON, wOBA, xwOBACON, xwOBA)
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    #gt(caption = paste(name_first,name_last,"Pitch Stats")) %>%
    gt() %>%
    cols_label(
      Pitches = md("**Pitches**"),
      `Strike%` = md("**Strike%**"),
      `FPS%` = md("**FPS%**"),
      `Put-Away%` = md("**Put-Away%**"),
      `CSW%` = md("**CSW%**"),
      `Whiff%` = md("**Whiff%**"),
      `Zone%` = md("**Zone%**"),
      `Swing%` = md("**Swing%**"),
      `Chase%` = md("**Chase%**"),
      `Z-Whiff%` = md("**Z-Whiff%**"),
      `RV/100` = md("**RV/100**"),
      BBE = md("**BBE**"),
      wOBACON = md("**wOBACON**"),
      wOBA = md("**wOBA**"),
      xwOBACON = md("**xwOBACON**"),
      xwOBA = md("**xwOBA**")
    ) %>%
    cols_align("center") %>%
    tab_spanner(
      label = md("**Pitch Level**"),
      columns = c(`Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `RV/100`)
    ) %>%
    fmt_percent(
      `Strike%`:`Z-Whiff%`,
      decimals = 1
    ) %>%
    tab_spanner(
      label = md("**PA Level**"),
      columns = c(wOBACON, wOBA, xwOBACON, xwOBA)
    ) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:12px;padding-bottom:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:12px;padding-right:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_header(title = paste(name_first,name_last,"Overall Pitch Stats"))
  #tab_footnote(footnote = "Pitches Tracked by Trackman")
  
  return(stats_table)
  
}

Get_Pitch_Stats_2023 <- function(df, pitcher, date1, date2) {
  
  sc_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2)
  
  player_stats <- df %>%
    filter(
      PitchCall != "Undefined",
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    mutate(
      TopZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 2.9, 3.75), T, F),
      BotZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 1.35, 2.2), T, F),
      is_BBE = if_else(PitchCall == "InPlay", T, F),
      is_BBE_EV = if_else(PitchCall == "InPlay" & TaggedHitType != "Bunt", T, F),
      is_Swing = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "StrikeSwinging","InPlay"), T, F),
      is_Whiff = if_else(PitchCall == "StrikeSwinging", T, F),
      is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Strike = if_else(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay", "StrikeCalled"), T, F),
      is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85), T, F),
      is_SweetSpot = if_else(PitchCall == "InPlay" & between(Angle, 8, 32), T, F),
      is_HardHit = if_else(PitchCall == "InPlay" & ExitSpeed >= 95, T, F),
      is_CalledStrike = if_else(PitchCall == "StrikeCalled", T, F),
      is_Ball = if_else(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch"), T, F),
      is_Foul = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "FoulBallFieldable"), T, F),
      is_Hit = if_else(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), T, F),
      is_GB = if_else(Angle < 10, T, F),
      is_FB = if_else(between(Angle, 25, 50), T, F),
      is_Single = if_else(PlayResult == "Single", T, F),
      is_Double = if_else(PlayResult == "Double", T, F),
      is_Triple = if_else(PlayResult == "Triple", T, F),
      is_HR = if_else(PlayResult == "HomeRun", T, F),
      is_Error = if_else(PlayResult == "Error", T, F),
      is_Sac = if_else(PlayResult == "Sacrifice", T, F),
      is_Dos = if_else(PitchCall == "HitByPitch", T, F),
      is_Strikeout = if_else(Strikes == 2 & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Walk = if_else(Balls == 3 & PitchCall %in% c("BallCalled", "BallinDirt"), T, F),
      xwOBA_weights = xwOBACON_value + (is_Walk * 0.7) + (is_Dos * 0.7),
      is_AB = is_BBE + is_Strikeout - is_Sac,
      is_PA = is_BBE + is_Strikeout + is_Walk + is_Dos
    ) %>%
    mutate(
      pitch_type = factor(
        TaggedPitchType,
        levels = c(
          "Fastball", "Sinker", "Cutter", "Slider", "Curveball", "ChangeUp"
        )
      )
    ) %>%
    group_by(pitch_type) %>%
    summarise(
      Pitches = n(),
      `FPS%` = round(mean(is_Strike[PitchofPA == 1]),5),
      `CSW%` = round((mean(is_CSW, na.rm = T)),5),
      `Put-Away%` = round(mean(is_CSW[Strikes == 2], na.rm = T),5),
      `Whiff%` = round(mean(is_Whiff[is_Swing == TRUE], na.rm = T),5),
      `Strike%` = round(mean(is_Strike, na.rm = T),5),
      `Zone%` = round(mean(is_Zone, na.rm = T),5),
      `Swing%` = round(mean(is_Swing, na.rm = T),5),
      `Chase%` = round(mean(is_Swing[is_Zone == FALSE], na.rm = TRUE),5),
      `Z-Swing%` = round(mean(is_Swing[is_Zone == TRUE], na.rm = T),5),
      `Z-Whiff%` = round(mean(is_Whiff[is_Zone == TRUE & is_Swing == TRUE]),5),
      BBE = sum(is_BBE),
      wOBA = round(mean(woba_value[is_PA == TRUE], na.rm = TRUE), 3),
      wOBACON = round(mean(woba_value[is_BBE == TRUE], na.rm = TRUE), 3),
      xwOBACON = round(mean(xwOBACON_value[is_BBE == TRUE], na.rm = T),3),
      xwOBA = round(mean(xwOBA_weights[is_PA == TRUE], na.rm = TRUE), 3),
      `RV/100` = round(mean(Run_Value, na.rm = T) * 100, 2),
    ) %>%
    ungroup() %>%
    mutate(`Mix` = scales::percent(Pitches / sum(Pitches), accuracy = 0.1)) %>%
    ## select(TaggedPitchType, Pitches, xwOBA, `CSW%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `Whiff%`, `Z-Swing%`, BBE, xwOBACON) %>%
    select(pitch_type, Pitches, `Mix`, `Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, 
           `Z-Swing%`, `Z-Whiff%`, `RV/100`, BBE, wOBACON, wOBA, xwOBACON, xwOBA) %>%
    arrange(desc(Pitches))
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    #gt(caption = paste(name_first,name_last,"Pitch Stats")) %>%
    gt() %>%
    data_color(
      pitch_type,
      method = "factor",
      palette = sc_colors
    ) %>%
    cols_label(
      pitch_type = md("**Pitch Type**"),
      Pitches = md("**Pitches**"),
      `Mix` = md("**Mix**"),
      `FPS%` = md("**FPS%**"),
      `Put-Away%` = md("**Put-Away%**"),
      `CSW%` = md("**CSW%**"),
      `Whiff%` = md("**Whiff%**"),
      `Strike%` = md("**Strike%**"),
      `Zone%` = md("**Zone%**"),
      `Swing%` = md("**Swing%**"),
      `Chase%` = md("**Chase%**"),
      `Z-Swing%` = md("**Z-Swing%**"),
      `Z-Whiff%` = md("**Z-Whiff%**"),
      `RV/100` = md("**RV/100**"),
      BBE = md("**BBE**"),
      wOBACON = md("**wOBACON**"),
      wOBA = md("**wOBA**"),
      xwOBACON = md("**xwOBACON**"),
      xwOBA = md("**xwOBA**")
    ) %>%
    cols_align("center") %>%
    fmt_percent(
      `Strike%`:`Z-Whiff%`,
      decimals = 1
    ) %>%
    tab_spanner(
      label = md("**Pitch Level**"),
      columns = c(`Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, 
                  `Z-Swing%`, `Z-Whiff%`, `RV/100`)
    ) %>%
    tab_spanner(
      label = md("**PA Level**"),
      columns = c(wOBACON, wOBA, xwOBACON, xwOBA)
    ) %>%
    tab_spanner(
      label = md("**Pitch Usage**"),
      columns = c(Pitches, Mix)
    ) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:12px;padding-bottom:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:12px;padding-right:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_header(title = paste(name_first,name_last,"Stats by Pitch Type"))
  #tab_footnote(footnote = "Pitches Tracked by Trackman")
  
  return(stats_table)
  
}

Get_Pitch_Locations_2023 <- function(df, pitcher, date1, date2) {
  
  add_zone_c <- function(Color = "red"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    geom_path(aes(.data$x, .data$y), data=kZone,
              lwd=1, col=Color, inherit.aes = FALSE)
  }
  
  sc_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  is_pitch_colors <- c(
    "1" = "#000000",
    "0" = "#000000"
  )
  
  player_stats <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           TaggedPitchType != "Other", TaggedPitchType != "Undefined") %>%
    mutate(is_pitch = if_else(!is.na(TaggedPitchType), 1, 0),
           is_pitch = as.character(is_pitch))
  
  player_name <- df %>%
    filter(Pitcher == pitcher)
  
  player_name <- player_name %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  plot <- ggplot(player_stats, aes(PlateLocSide, PlateLocHeight)) +
    geom_density_2d_filled(contour_var = "ndensity", alpha = 0.90) +
    scale_fill_brewer(palette = "Spectral", direction = -1) +
    add_zone_c(Color = "black") +
    ylim(0,4.5) +
    xlim(-1.7,1.7) +
    coord_fixed() +
    theme_classic() +
    labs(
      title = paste(name_first,name_last,"Pitch Contours"),
      x = "Pitcher's Perspective",
      y = "",
      fill = ""
    ) +
    facet_wrap(~TaggedPitchType, ncol = 6) +
    guides(
      color = "none"
    ) +
    theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "black", size = 14,
                                       hjust = 0.5, vjust = 0.8, angle = 0),
          text=element_text(size=14),
          legend.position = "none",
          panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(colour = "black", size = 12, face = "bold")) +
    geom_segment(aes(x = -0.85, y = 0.15, xend = 0.85, yend = 0.15), linewidth = 1, color = "black") + 
    geom_segment(aes(x = -0.85, y = 0.3, xend = -0.85, yend = 0.15), linewidth = 1, color = "black") + 
    geom_segment(aes(x = -0.85, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
    geom_segment(aes(x = 0, y = 0.5, xend = 0.85, yend = 0.3), linewidth = 1, color = "black") + 
    geom_segment(aes(x = 0.85, y = 0.3, xend = 0.85, yend = 0.15), linewidth = 1, color = "black")
  
  
  #plot <- ggplot(player_stats, aes(PlateLocSide, PlateLocHeight)) +
  #  geom_hdr(
  #    aes(
  #      fill = after_stat(probs),
  #      color = is_pitch
  #    ),
  #    alpha = 0.7
  #  ) +
  #  scale_color_manual(values = is_pitch_colors) +
  #  scale_fill_brewer(palette = "Blues",
  #                    direction = 1) +
  #  add_zone_c(Color = "black") +
  #  ylim(0,4.5) +
  #  xlim(-1.7,1.7) +
  #  coord_fixed() +
  #  theme_classic() +
  #  labs(
  #    title = paste(name_first,name_last,"Pitch Contours"),
  #    x = "Pitcher's Perspective",
  #    y = "",
  #    fill = ""
  #  ) +
  #  facet_wrap(~TaggedPitchType, ncol = 6) +
  #  guides(
  #    color = "none"
  #  ) +
  #  theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
  #                                  hjust = 0.5, vjust = 0.8, angle = 0),
  #        plot.subtitle = element_text(colour = "black", size = 14,
  #                                     hjust = 0.5, vjust = 0.8, angle = 0),
  #        text=element_text(size=14),
  #        legend.position = "bottom",
  #        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
  #        axis.title = element_text(face = "bold"),
  #        strip.text = element_text(colour = "black", size = 12, face = "bold")) +
  #  geom_segment(aes(x = -0.85, y = 0.15, xend = 0.85, yend = 0.15), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = -0.85, y = 0.3, xend = -0.85, yend = 0.15), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = -0.85, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = 0, y = 0.5, xend = 0.85, yend = 0.3), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = 0.85, y = 0.3, xend = 0.85, yend = 0.15), linewidth = 1, color = "black")
  
  
  
  
  
  #plot <- ggplot(player_stats, aes(PlateLocSide, PlateLocHeight, color = TaggedPitchType)) +
  #  geom_point(size = 5, alpha =0.8) +
  #  scale_color_manual(values = sc_colors) +
  #  add_zone_c(Color = "black") +
  #  ylim(0,4.5) +
  #  xlim(-2,2) +
  #  coord_fixed() +
  #  theme_classic() +
  #  labs(
  #    title = paste(name_first,name_last,"Pitch Locations"),
  #    x = "Pitcher's Perspective",
  #    y = "",
  #    fill = "",
  #    color = ""
  #  ) +
  #  facet_wrap(~TaggedPitchType, ncol = 6) +
  #  guides(
  #    color = "none"
  #  ) +
  #  theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
  #                                  hjust = 0.5, vjust = 0.8, angle = 0),
  #        plot.subtitle = element_text(colour = "black", size = 14,
  #                                     hjust = 0.5, vjust = 0.8, angle = 0),
  #        text=element_text(size=14),
  #        legend.position = "bottom",
  #        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
  #        axis.title = element_text(face = "bold"),
  #        strip.text = element_text(colour = "black", size = 12, face = "bold")) +
  #  geom_segment(aes(x = -0.85, y = 0.15, xend = 0.85, yend = 0.15), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = -0.85, y = 0.3, xend = -0.85, yend = 0.15), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = -0.85, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = 0, y = 0.5, xend = 0.85, yend = 0.3), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = 0.85, y = 0.3, xend = 0.85, yend = 0.15), linewidth = 1, color = "black")
  
  
  
  
  
  return(plot)
  
  
}

Get_AttackZone_Stats_2023 <- function(df, pitcher, date1, date2) {
  
  pitchzone_colors <- c(
    "Heart" = "#F8766D",
    "Shadow" = "#00BA38",
    "Chase" = "#619CFF",
    "Waste" = "gray50"
  )
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2)
  
  player_stats <- df %>%
    filter(
      PitchCall != "Undefined",
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    mutate(
      TopZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 2.9, 3.75), T, F),
      BotZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 1.35, 2.2), T, F),
      is_BBE = if_else(PitchCall == "InPlay", T, F),
      is_BBE_EV = if_else(PitchCall == "InPlay" & TaggedHitType != "Bunt", T, F),
      is_Swing = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "StrikeSwinging","InPlay"), T, F),
      is_Whiff = if_else(PitchCall == "StrikeSwinging", T, F),
      is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Strike = if_else(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay", "StrikeCalled"), T, F),
      is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85), T, F),
      is_SweetSpot = if_else(PitchCall == "InPlay" & between(Angle, 8, 32), T, F),
      is_HardHit = if_else(PitchCall == "InPlay" & ExitSpeed >= 95, T, F),
      is_CalledStrike = if_else(PitchCall == "StrikeCalled", T, F),
      is_Ball = if_else(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch"), T, F),
      is_Foul = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "FoulBallFieldable"), T, F),
      is_Hit = if_else(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), T, F),
      is_GB = if_else(Angle < 10, T, F),
      is_FB = if_else(between(Angle, 25, 50), T, F),
      is_Single = if_else(PlayResult == "Single", T, F),
      is_Double = if_else(PlayResult == "Double", T, F),
      is_Triple = if_else(PlayResult == "Triple", T, F),
      is_HR = if_else(PlayResult == "HomeRun", T, F),
      is_Error = if_else(PlayResult == "Error", T, F),
      is_Sac = if_else(PlayResult == "Sacrifice", T, F),
      is_Dos = if_else(PitchCall == "HitByPitch", T, F),
      is_Strikeout = if_else(Strikes == 2 & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Walk = if_else(Balls == 3 & PitchCall %in% c("BallCalled", "BallinDirt"), T, F),
      xwOBA_weights = xwOBACON_value + (is_Walk * 0.7) + (is_Dos * 0.7),
      is_AB = is_BBE + is_Strikeout - is_Sac,
      is_PA = is_BBE + is_Strikeout + is_Walk + is_Dos,
      pitchzone = factor(
        pitchzone,
        levels = c(
          "Heart", "Shadow", "Chase", "Waste"
        )
      )
    ) %>%
    group_by(pitchzone) %>%
    summarise(
      Pitches = n(),
      `Swing%` = round(mean(is_Swing, na.rm = T),5),
      `CSW%` = round((mean(is_CSW, na.rm = T)),5),
      `Put-Away%` = round(mean(is_CSW[Strikes == 2], na.rm = T),5),
      `Whiff%` = round(mean(is_Whiff[is_Swing == TRUE], na.rm = T),5),
      BBE = sum(is_BBE),
      xwOBACON = round(mean(xwOBACON_value[is_BBE == TRUE], na.rm = T),3),
      xwOBA = round(mean(xwOBA_weights[is_PA == TRUE], na.rm = TRUE), 3),
      `RV/100` = round(mean(Run_Value, na.rm = T) * 100, 2),
      RV = round(sum(Run_Value, na.rm = T),2)
    ) %>%
    ungroup() %>%
    mutate(`Mix` = scales::percent(Pitches / sum(Pitches), accuracy = 0.1)) %>%
    ## select(TaggedPitchType, Pitches, xwOBA, `CSW%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `Whiff%`, `Z-Swing%`, BBE, xwOBACON) %>%
    select(pitchzone, Pitches, BBE, `Mix`, `Swing%`, `CSW%`, `Whiff%`, `Put-Away%`, `RV/100`, RV, xwOBACON, xwOBA) %>%
    arrange(desc(Pitches))
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    #gt(caption = paste(name_first,name_last,"Pitch Stats")) %>%
    gt() %>%
    data_color(
      pitchzone,
      method = "factor",
      palette = pitchzone_colors
    ) %>%
    cols_label(
      pitchzone = md("**Attack Zone**"),
      Pitches = md("**Pitches**"),
      `Mix` = md("**Mix**"),
      `Put-Away%` = md("**Put-Away%**"),
      `CSW%` = md("**CSW%**"),
      `Whiff%` = md("**Whiff%**"),
      `Swing%` = md("**Swing%**"),
      `RV/100` = md("**RV/100**"),
      BBE = md("**BBE**"),
      xwOBACON = md("**xwOBACON**"),
      xwOBA = md("**xwOBA**")
    ) %>%
    cols_align("center") %>%
    fmt_percent(
      `Swing%`:`Put-Away%`,
      decimals = 1
    ) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:12px;padding-bottom:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:12px;padding-right:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_header(title = paste(name_first,name_last,"Stats by Attack Zone"))
  #tab_footnote(footnote = "Pitches Tracked by Trackman")
  
  return(stats_table)
  
}

Get_Pitch_Percentages_2023 <- function(df, pitcher, date1, date2) {
  
  player_stats <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           TaggedPitchType != "Other", TaggedPitchType != "Undefined")
  
  player_name <- df %>%
    filter(Pitcher == pitcher)
  
  player_name <- player_name %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  player_stats <- player_stats %>%
    mutate(Count = paste(Balls, Strikes, sep = "-"),
           PitchClass = ifelse(TaggedPitchType %in% c("Fastball","Sinker"), "Fastball",
                               ifelse(TaggedPitchType %in% c("Slider", "Curveball", "Slurve", "Cutter"), 
                                      "Breaking Ball",
                                      ifelse(TaggedPitchType %in% c("ChangeUp", "Splitter"), "Off-speed", "Other"))),
           CountType = ifelse(Count %in%
                                c("1-0", "2-0", "3-0",
                                  "2-1", "3-1", "3-2"), "Behind",
                              ifelse(Count %in%
                                       c("0-1", "0-2", "1-2", "2-2"),
                                     "Ahead", "Neutral")))
  
  player_stats %>%
    group_by(Count) %>%
    summarise(total_pitches = n()) -> test
  
  player_stats %>%
    group_by(Count, PitchClass) %>%
    summarise(pitches = n()) -> test_pt
  
  test_total <- left_join(test, test_pt, by = "Count")
  
  test_total %>%
    group_by(Count, PitchClass) %>%
    summarise(pcts = round(100*(pitches / total_pitches),1)) -> count_pcts
  
  plot <- ggplot(count_pcts, aes(x="", y=pcts, fill=PitchClass)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("Breaking Ball" = '#EEE716', "Fastball" = '#D22D49', 
                                 "Off-speed" = '#1DBE3A')) +
    facet_wrap(~Count, ncol = 6) +
    theme_void() +
    labs(title = paste(name_first,name_last,"Pitch% by Count"),
         fill = "Pitch Class",
         #subtitle = paste("2023 Season")
    ) +
    theme(plot.title = element_text(colour = "black",
                                    face = "bold",
                                    size = 20,
                                    hjust = 0.5,
                                    vjust = 4,
                                    angle = 0),
          plot.subtitle = element_text(colour = "Navy",
                                       size = 13,
                                       hjust = 0.5,
                                       vjust = 5.5,
                                       angle = 0),
          strip.text = element_text(colour = "black",
                                    size = 10,
                                    face = "bold"))
  
  return(plot)
  
  
}

Get_Pitch_Percentages_Type_2023 <- function(df, pitcher, date1, date2) {
  
  player_stats <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           TaggedPitchType != "Other", TaggedPitchType != "Undefined")
  
  player_name <- df %>%
    filter(Pitcher == pitcher)
  
  player_name <- player_name %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  player_stats <- player_stats %>%
    mutate(Count = paste(Balls, Strikes, sep = "-"),
           PitchClass = ifelse(TaggedPitchType %in% c("Fastball","Sinker"), "Fastball",
                               ifelse(TaggedPitchType %in% c("Slider", "Curveball", "Slurve", "Cutter"), 
                                      "Breaking Ball",
                                      ifelse(TaggedPitchType %in% c("ChangeUp", "Splitter"), "Off-speed", "Other"))),
           CountType = ifelse(Count %in%
                                c("1-0", "2-0",
                                  "2-1", "3-1"), "'Fastball Count'",
                              ifelse(Count %in%
                                       c("0-1", "0-2", "1-2", "2-2"), "Ahead",
                                     ifelse(Count %in% c("0-0", "1-1", "3-2"), "Even",
                                            "3-0"))))
  
  player_stats %>%
    group_by(CountType) %>%
    summarise(total_pitches = n()) -> test
  
  player_stats %>%
    group_by(CountType, PitchClass) %>%
    summarise(pitches = n()) -> test_pt
  
  test_total <- left_join(test, test_pt, by = "CountType")
  
  test_total %>%
    group_by(CountType, PitchClass) %>%
    summarise(pcts = round(100*(pitches / total_pitches),1)) -> count_pcts
  
  plot <- ggplot(count_pcts, aes(x="", y=pcts, fill=PitchClass)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("Breaking Ball" = '#EEE716', "Fastball" = '#D22D49', 
                                 "Off-speed" = '#1DBE3A')) +
    facet_wrap(~CountType, ncol = 6) +
    theme_void() +
    labs(title = paste(name_first,name_last,"Pitch% by Count Type"),
         fill = "Pitch Class",
         #subtitle = paste("2023 Season"),
         caption = "Fastball Count: 1-0, 2-0, 2-1, 3-1
                    Ahead: 0-1, 0-2, 1-2, 2-2
                    Even: 0-0, 1-1, 3-2") +
    theme(plot.title = element_text(colour = "black",
                                    face = "bold",
                                    size = 20,
                                    hjust = 0.5,
                                    vjust = 4,
                                    angle = 0),
          plot.subtitle = element_text(colour = "black",
                                       size = 13,
                                       hjust = 0.5,
                                       vjust = 5.5,
                                       angle = 0),
          plot.caption = element_text(color = "black",
                                      face = "bold"),
          strip.text = element_text(colour = "black",
                                    size = 10,
                                    face = "bold"))
  
  return(plot)
  
  
}

Get_Pitch_Stats_Count_2023 <- function(df, pitcher, date1, date2) {
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2)
  
  player_stats <- df %>%
    filter(
      PitchCall != "Undefined",
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    mutate(
      TopZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 2.9, 3.75), T, F),
      BotZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 1.35, 2.2), T, F),
      is_BBE = if_else(PitchCall == "InPlay", T, F),
      is_BBE_EV = if_else(PitchCall == "InPlay" & TaggedHitType != "Bunt", T, F),
      is_Swing = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "StrikeSwinging","InPlay"), T, F),
      is_Whiff = if_else(PitchCall == "StrikeSwinging", T, F),
      is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Strike = if_else(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay", "StrikeCalled"), T, F),
      is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85), T, F),
      is_SweetSpot = if_else(PitchCall == "InPlay" & between(Angle, 8, 32), T, F),
      is_HardHit = if_else(PitchCall == "InPlay" & ExitSpeed >= 95, T, F),
      is_CalledStrike = if_else(PitchCall == "StrikeCalled", T, F),
      is_Ball = if_else(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch"), T, F),
      is_Foul = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "FoulBallFieldable"), T, F),
      is_Hit = if_else(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), T, F),
      is_GB = if_else(Angle < 10, T, F),
      is_FB = if_else(between(Angle, 25, 50), T, F),
      is_Single = if_else(PlayResult == "Single", T, F),
      is_Double = if_else(PlayResult == "Double", T, F),
      is_Triple = if_else(PlayResult == "Triple", T, F),
      is_HR = if_else(PlayResult == "HomeRun", T, F),
      is_Error = if_else(PlayResult == "Error", T, F),
      is_Sac = if_else(PlayResult == "Sacrifice", T, F),
      is_Dos = if_else(PitchCall == "HitByPitch", T, F),
      is_Strikeout = if_else(Strikes == 2 & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Walk = if_else(Balls == 3 & PitchCall %in% c("BallCalled", "BallinDirt"), T, F),
      xwOBA_weights = xwOBACON_value + (is_Walk * 0.7) + (is_Dos * 0.7),
      is_AB = is_BBE + is_Strikeout - is_Sac,
      is_PA = is_BBE + is_Strikeout + is_Walk + is_Dos,
      Count = paste(Balls, Strikes, sep = "-"),
      PitchClass = ifelse(TaggedPitchType %in% c("Fastball","Sinker"), "Fastball",
                          ifelse(TaggedPitchType %in% c("Slider", "Curveball", "Slurve", "Cutter"), 
                                 "Breaking Ball",
                                 ifelse(TaggedPitchType %in% c("ChangeUp", "Splitter"), "Off-speed", "Other"))),
      CountType = ifelse(Count %in%
                           c("1-0", "2-0",
                             "2-1", "3-1"), "'Fastball Count'",
                         ifelse(Count %in%
                                  c("0-1", "0-2", "1-2", "2-2"), "Ahead",
                                ifelse(Count %in% c("0-0", "1-1", "3-2"), "Even",
                                       "3-0")))
    ) %>%
    group_by(Count) %>%
    summarise(
      Pitches = n(),
      `CSW%` = round((mean(is_CSW, na.rm = T)),5),
      `Whiff%` = round(mean(is_Whiff[is_Swing == TRUE], na.rm = T),5),
      `Strike%` = round(mean(is_Strike, na.rm = T),5),
      `Zone%` = round(mean(is_Zone, na.rm = T),5),
      `Swing%` = round(mean(is_Swing, na.rm = T),5),
      `Chase%` = round(mean(is_Swing[is_Zone == FALSE], na.rm = TRUE),5),
      `Z-Swing%` = round(mean(is_Swing[is_Zone == TRUE], na.rm = T),5),
      `Z-Whiff%` = round(mean(is_Whiff[is_Zone == TRUE & is_Swing == TRUE]),5),
      BBE = sum(is_BBE),
      wOBA = round(mean(woba_value[is_PA == TRUE], na.rm = TRUE), 3),
      wOBACON = round(mean(woba_value[is_BBE == TRUE], na.rm = TRUE), 3),
      xwOBACON = round(mean(xwOBACON_value[is_BBE == TRUE], na.rm = T),3),
      xwOBA = round(mean(xwOBA_weights[is_PA == TRUE], na.rm = TRUE), 3),
      `RV/100` = round(mean(Run_Value, na.rm = T) * 100, 2),
    ) %>%
    ungroup() %>%
    ## select(TaggedPitchType, Pitches, xwOBA, `CSW%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `Whiff%`, `Z-Swing%`, BBE, xwOBACON) %>%
    select(Count, Pitches, `CSW%`, `Whiff%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, 
           `Z-Swing%`, `Z-Whiff%`, `RV/100`, BBE, wOBACON, wOBA, xwOBACON, xwOBA) %>%
    arrange(Count)
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    #gt(caption = paste(name_first,name_last,"Pitch Stats by Count")) %>%
    gt() %>%
    cols_label(
      Count = md("**Count**"),
      Pitches = md("**Pitches**"),
      `CSW%` = md("**CSW%**"),
      `Whiff%` = md("**Whiff%**"),
      `Strike%` = md("**Strike%**"),
      `Zone%` = md("**Zone%**"),
      `Swing%` = md("**Swing%**"),
      `Chase%` = md("**Chase%**"),
      `Z-Swing%` = md("**Z-Swing%**"),
      `Z-Whiff%` = md("**Z-Whiff%**"),
      `RV/100` = md("**RV/100**"),
      BBE = md("**BBE**"),
      wOBACON = md("**wOBACON**"),
      wOBA = md("**wOBA**"),
      xwOBACON = md("**xwOBACON**"),
      xwOBA = md("**xwOBA**")
    ) %>%
    cols_align("center") %>%
    fmt_percent(
      `CSW%`:`Z-Whiff%`,
      decimals = 1
    ) %>%
    tab_spanner(
      label = md("**Pitch Level**"),
      columns = c(`CSW%`, `Whiff%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, 
                  `Z-Swing%`, `Z-Whiff%`, `RV/100`)
    ) %>%
    tab_spanner(
      label = md("**PA Level**"),
      columns = c(wOBACON, wOBA, xwOBACON, xwOBA)
    ) %>%
    tab_header(title = paste(name_first,name_last,"Pitch Stats by Count"))
  #tab_footnote(footnote = "Pitches Tracked by Trackman")
  
  return(stats_table)
  
}

Get_Pitch_Stats_CountType_2023 <- function(df, pitcher, date1, date2) {
  
  type_colors <- c(
    "Fastball" = "#D22D49",
    "Breaking Ball" = "#EEE716",
    "Off-speed" = "#1DBE3A"
  )
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2)
  
  player_stats <- df %>%
    filter(
      PitchCall != "Undefined",
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    mutate(TopZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 2.9, 3.75), T, F),
           BotZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 1.35, 2.2), T, F),
           is_BBE = if_else(PitchCall == "InPlay", T, F),
           is_BBE_EV = if_else(PitchCall == "InPlay" & TaggedHitType != "Bunt", T, F),
           is_Swing = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "StrikeSwinging","InPlay"), T, F),
           is_Whiff = if_else(PitchCall == "StrikeSwinging", T, F),
           is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
           is_Strike = if_else(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay", "StrikeCalled"), T, F),
           is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85), T, F),
           is_SweetSpot = if_else(PitchCall == "InPlay" & between(Angle, 8, 32), T, F),
           is_HardHit = if_else(PitchCall == "InPlay" & ExitSpeed >= 95, T, F),
           is_CalledStrike = if_else(PitchCall == "StrikeCalled", T, F),
           is_Ball = if_else(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch"), T, F),
           is_Foul = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "FoulBallFieldable"), T, F),
           is_Hit = if_else(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), T, F),
           is_GB = if_else(Angle < 10, T, F),
           is_FB = if_else(between(Angle, 25, 50), T, F),
           is_Single = if_else(PlayResult == "Single", T, F),
           is_Double = if_else(PlayResult == "Double", T, F),
           is_Triple = if_else(PlayResult == "Triple", T, F),
           is_HR = if_else(PlayResult == "HomeRun", T, F),
           is_Error = if_else(PlayResult == "Error", T, F),
           is_Sac = if_else(PlayResult == "Sacrifice", T, F),
           is_Dos = if_else(PitchCall == "HitByPitch", T, F),
           is_Strikeout = if_else(Strikes == 2 & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
           is_Walk = if_else(Balls == 3 & PitchCall %in% c("BallCalled", "BallinDirt"), T, F),
           xwOBA_weights = xwOBACON_value + (is_Walk * 0.7) + (is_Dos * 0.7),
           is_AB = is_BBE + is_Strikeout - is_Sac,
           is_PA = is_BBE + is_Strikeout + is_Walk + is_Dos,
           Count = paste(Balls, Strikes, sep = "-"),
           PitchClass = ifelse(TaggedPitchType %in% c("Fastball","Sinker"), "Fastball",
                               ifelse(TaggedPitchType %in% c("Slider", "Curveball", "Slurve", "Cutter"), 
                                      "Breaking Ball",
                                      ifelse(TaggedPitchType %in% c("ChangeUp", "Splitter"), "Off-speed", "Other"))),
           CountType = ifelse(Count %in%
                                c("1-0", "2-0",
                                  "2-1", "3-1"), "'Fastball Count'",
                              ifelse(Count %in%
                                       c("0-1", "0-2", "1-2", "2-2"), "Ahead",
                                     ifelse(Count %in% c("0-0", "1-1", "3-2"), "Even",
                                            "3-0")))) %>%
    mutate(
      pitch_class = factor(
        PitchClass,
        levels = c(
          "Fastball", "Breaking Ball", "Off-speed"
        )
      )
    ) %>%
    group_by(CountType, pitch_class) %>%
    summarise(
      Pitches = n(),
      `CSW%` = round((mean(is_CSW, na.rm = T)),5),
      `Whiff%` = round(mean(is_Whiff[is_Swing == TRUE], na.rm = T),5),
      `Strike%` = round(mean(is_Strike, na.rm = T),5),
      `Zone%` = round(mean(is_Zone, na.rm = T),5),
      `Swing%` = round(mean(is_Swing, na.rm = T),5),
      `Chase%` = round(mean(is_Swing[is_Zone == FALSE], na.rm = TRUE),5),
      `Z-Swing%` = round(mean(is_Swing[is_Zone == TRUE], na.rm = T),5),
      `Z-Whiff%` = round(mean(is_Whiff[is_Zone == TRUE & is_Swing == TRUE]),5),
      BBE = sum(is_BBE),
      wOBA = round(mean(woba_value[is_PA == TRUE], na.rm = TRUE), 3),
      wOBACON = round(mean(woba_value[is_BBE == TRUE], na.rm = TRUE), 3),
      xwOBACON = round(mean(xwOBACON_value[is_BBE == TRUE], na.rm = T),3),
      xwOBA = round(mean(xwOBA_weights[is_PA == TRUE], na.rm = TRUE), 3),
      `RV/100` = round(mean(Run_Value, na.rm = T) * 100, 2),
    ) %>%
    ungroup() %>%
    ## select(TaggedPitchType, Pitches, xwOBA, `CSW%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `Whiff%`, `Z-Swing%`, BBE, xwOBACON) %>%
    select(CountType, pitch_class, Pitches,`CSW%`, `Whiff%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, 
           `Z-Swing%`, `Z-Whiff%`, `RV/100`, BBE, wOBACON, wOBA, xwOBACON, xwOBA) %>%
    arrange(CountType, desc(Pitches))
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    #gt(caption = paste(name_first,name_last,"Pitch Stats by Count Type")) %>%
    gt() %>%
    data_color(
      pitch_class,
      method = "factor",
      palette = type_colors
    ) %>%
    cols_label(
      pitch_class = md("**Pitch Class**"),
      CountType = md("**Count Type**"),
      Pitches = md("**Pitches**"),
      `CSW%` = md("**CSW%**"),
      `Whiff%` = md("**Whiff%**"),
      `Strike%` = md("**Strike%**"),
      `Zone%` = md("**Zone%**"),
      `Swing%` = md("**Swing%**"),
      `Chase%` = md("**Chase%**"),
      `Z-Swing%` = md("**Z-Swing%**"),
      `Z-Whiff%` = md("**Z-Whiff%**"),
      `RV/100` = md("**RV/100**"),
      BBE = md("**BBE**"),
      wOBACON = md("**wOBACON**"),
      wOBA = md("**wOBA**"),
      xwOBACON = md("**xwOBACON**"),
      xwOBA = md("**xwOBA**")
    ) %>%
    cols_align("center") %>%
    fmt_percent(
      `CSW%`:`Z-Whiff%`,
      decimals = 1
    ) %>%
    tab_spanner(
      label = md("**Pitch Level**"),
      columns = c(`CSW%`, `Whiff%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, 
                  `Z-Swing%`, `Z-Whiff%`, `RV/100`)
    ) %>%
    tab_spanner(
      label = md("**PA Level**"),
      columns = c(wOBACON, wOBA, xwOBACON, xwOBA)
    ) %>%
    tab_header(title = paste(name_first,name_last,"Pitch Stats by Count Type"))
  #tab_footnote(footnote = "Pitches Tracked by Trackman")
  
  return(stats_table)
  
}

Get_Platoon_Pitch_Locations_2023 <- function(df, pitcher, date1, date2) {
  
  add_zone_c <- function(Color = "red"){
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    geom_path(aes(.data$x, .data$y), data=kZone,
              lwd=1, col=Color, inherit.aes = FALSE)
  }
  
  is_pitch_colors <- c(
    "1" = "#000000",
    "0" = "#000000"
  )
  
  sc_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  player_stats <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           TaggedPitchType != "Other", 
           TaggedPitchType != "Undefined",
           BatterSide %in% c("Left","Right")) %>%
    mutate(is_pitch = if_else(!is.na(TaggedPitchType), 1, 0),
           is_pitch = as.character(is_pitch))
  
  player_name <- df %>%
    filter(Pitcher == pitcher)
  player_name <- player_name %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  #plot <- ggplot(player_stats, aes(PlateLocSide, PlateLocHeight)) +
  #  geom_density_2d_filled(contour_var = "ndensity", alpha = 0.90) +
  #  scale_fill_brewer(palette = "Spectral", direction = -1) +
  #  add_zone_c(Color = "black") +
  #  theme_bw() +
  #  xlim(-1.7, 1.7) +
  #  ylim(0.5, 4.25) +
  #  theme(legend.position = "none") +
  #  theme(text=element_text(size=15)) +
  #  labs(title = paste(name_first,name_last,"Platoon Pitch Contours"),
  #       #subtitle = paste("2023 Season")
  #       ) +
  #  ylab("") +
  #  xlab("Pitcher's Perspective") +
  #  coord_equal() +
  #  facet_grid(BatterSide~TaggedPitchType) +
  #  theme(plot.title = element_text(colour = "black",
  #                                  face = "bold",
  #                                  size = 20,
  #                                  hjust = 0.5,
  #                                  vjust = 0.8,
  #                                  angle = 0),
  #        plot.subtitle = element_text(colour = "black",
  #                                     size = 13,
  #                                     hjust = 0.5,
  #                                     vjust = 0.8,
  #                                     angle = 0))
  
  
  plot <- ggplot(player_stats, aes(PlateLocSide, PlateLocHeight)) +
    geom_hdr(
      aes(
        fill = after_stat(probs),
        color = is_pitch
      ),
      alpha = 0.7
    ) +
    scale_color_manual(values = is_pitch_colors) +
    scale_fill_brewer(palette = "Blues",
                      direction = 1) +
    add_zone_c(Color = "black") +
    ylim(0,4.5) +
    xlim(-1.7,1.7) +
    coord_fixed() +
    theme_classic() +
    labs(
      title = paste(name_first,name_last,"Platoon Pitch Contours"),
      x = "Pitcher's Perspective",
      y = "",
      fill = ""
    ) +
    facet_grid(BatterSide~TaggedPitchType) +
    guides(
      color = "none"
    ) +
    theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "black", size = 14,
                                       hjust = 0.5, vjust = 0.8, angle = 0),
          text=element_text(size=14),
          legend.position = "bottom",
          panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(colour = "black", size = 12, face = "bold")) +
    geom_segment(aes(x = -0.85, y = 0.15, xend = 0.85, yend = 0.15), linewidth = 1, color = "black") + 
    geom_segment(aes(x = -0.85, y = 0.3, xend = -0.85, yend = 0.15), linewidth = 1, color = "black") + 
    geom_segment(aes(x = -0.85, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
    geom_segment(aes(x = 0, y = 0.5, xend = 0.85, yend = 0.3), linewidth = 1, color = "black") + 
    geom_segment(aes(x = 0.85, y = 0.3, xend = 0.85, yend = 0.15), linewidth = 1, color = "black")
  
  
  
  #plot <- ggplot(player_stats, aes(PlateLocSide, PlateLocHeight, color = TaggedPitchType)) +
  #  geom_point(size = 5, alpha =0.8) +
  #  scale_color_manual(values = sc_colors) +
  #  add_zone_c(Color = "black") +
  #  ylim(0,4.5) +
  #  xlim(-2,2) +
  #  coord_fixed() +
  #  theme_classic() +
  #  labs(
  #    title = paste(name_first,name_last,"Platoon Pitch Locations"),
  #    x = "Pitcher's Perspective",
  #    y = "",
  #    fill = "",
  #    color = ""
  #  ) +
  #  facet_grid(BatterSide~TaggedPitchType) +
  #  guides(
  #    color = "none"
  #  ) +
  #  theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
  #                                  hjust = 0.5, vjust = 0.8, angle = 0),
  #        plot.subtitle = element_text(colour = "black", size = 14,
  #                                     hjust = 0.5, vjust = 0.8, angle = 0),
  #        text=element_text(size=14),
  #        legend.position = "bottom",
  #        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
  #        axis.title = element_text(face = "bold"),
  #        strip.text = element_text(colour = "black", size = 12, face = "bold")) +
  #  geom_segment(aes(x = -0.85, y = 0.15, xend = 0.85, yend = 0.15), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = -0.85, y = 0.3, xend = -0.85, yend = 0.15), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = -0.85, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = 0, y = 0.5, xend = 0.85, yend = 0.3), linewidth = 1, color = "black") + 
  #  geom_segment(aes(x = 0.85, y = 0.3, xend = 0.85, yend = 0.15), linewidth = 1, color = "black")
  
  
  
  return(plot)
  
}

Get_Overall_Platoon_Pitch_Stats_2023 <- function(df, pitcher, date1, date2) {
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2)
  
  player_stats <- df %>%
    filter(
      PitchCall != "Undefined",
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    mutate(
      TopZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 2.9, 3.75), T, F),
      BotZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 1.35, 2.2), T, F),
      is_BBE = if_else(PitchCall == "InPlay", T, F),
      is_BBE_EV = if_else(PitchCall == "InPlay" & TaggedHitType != "Bunt", T, F),
      is_Swing = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "StrikeSwinging","InPlay"), T, F),
      is_Whiff = if_else(PitchCall == "StrikeSwinging", T, F),
      is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Strike = if_else(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay", "StrikeCalled"), T, F),
      is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85), T, F),
      is_SweetSpot = if_else(PitchCall == "InPlay" & between(Angle, 8, 32), T, F),
      is_HardHit = if_else(PitchCall == "InPlay" & ExitSpeed >= 95, T, F),
      is_CalledStrike = if_else(PitchCall == "StrikeCalled", T, F),
      is_Ball = if_else(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch"), T, F),
      is_Foul = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "FoulBallFieldable"), T, F),
      is_Hit = if_else(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), T, F),
      is_GB = if_else(Angle < 10, T, F),
      is_FB = if_else(between(Angle, 25, 50), T, F),
      is_Single = if_else(PlayResult == "Single", T, F),
      is_Double = if_else(PlayResult == "Double", T, F),
      is_Triple = if_else(PlayResult == "Triple", T, F),
      is_HR = if_else(PlayResult == "HomeRun", T, F),
      is_Error = if_else(PlayResult == "Error", T, F),
      is_Sac = if_else(PlayResult == "Sacrifice", T, F),
      is_Dos = if_else(PitchCall == "HitByPitch", T, F),
      is_Strikeout = if_else(Strikes == 2 & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Walk = if_else(Balls == 3 & PitchCall %in% c("BallCalled", "BallinDirt"), T, F),
      xwOBA_weights = xwOBACON_value + (is_Walk * 0.7) + (is_Dos * 0.7),
      is_AB = is_BBE + is_Strikeout - is_Sac,
      is_PA = is_BBE + is_Strikeout + is_Walk + is_Dos
    ) %>%
    group_by(BatterSide) %>%
    summarise(
      Pitches = n(),
      `FPS%` = round(mean(is_Strike[PitchofPA == 1]),5),
      `CSW%` = round((mean(is_CSW, na.rm = T)),5),
      `Put-Away%` = round(mean(is_CSW[Strikes == 2], na.rm = T),5),
      `Whiff%` = round(mean(is_Whiff[is_Swing == TRUE], na.rm = T),5),
      `Strike%` = round(mean(is_Strike, na.rm = T),5),
      `Zone%` = round(mean(is_Zone, na.rm = T),5),
      `Swing%` = round(mean(is_Swing, na.rm = T),5),
      `Chase%` = round(mean(is_Swing[is_Zone == FALSE], na.rm = TRUE),5),
      `Z-Swing%` = round(mean(is_Swing[is_Zone == TRUE], na.rm = T),5),
      `Z-Whiff%` = round(mean(is_Whiff[is_Zone == TRUE & is_Swing == TRUE]),5),
      BBE = sum(is_BBE),
      wOBA = round(mean(woba_value[is_PA == TRUE], na.rm = TRUE), 3),
      wOBACON = round(mean(woba_value[is_BBE == TRUE], na.rm = TRUE), 3),
      xwOBACON = round(mean(xwOBACON_value[is_BBE == TRUE], na.rm = T),3),
      xwOBA = round(mean(xwOBA_weights[is_PA == TRUE], na.rm = TRUE), 3),
      `RV/100` = round(mean(Run_Value, na.rm = T) * 100, 2),
    ) %>%
    ## select(TaggedPitchType, Pitches, xwOBA, `CSW%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `Whiff%`, `Z-Swing%`, BBE, xwOBACON) %>%
    select(BatterSide,Pitches, `Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, 
           `Z-Swing%`, `Z-Whiff%`, `RV/100`, BBE, wOBACON, wOBA, xwOBACON, xwOBA)
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    #gt(caption = paste(name_first,name_last,"Overall Platoon Stats")) %>%
    gt() %>%
    cols_label(
      BatterSide = md("**Batter Side**"),
      Pitches = md("**Pitches**"),
      `CSW%` = md("**CSW%**"),
      `FPS%` = md("**FPS%**"),
      `Put-Away%` = md("**Put-Away%**"),
      `Whiff%` = md("**Whiff%**"),
      `Strike%` = md("**Strike%**"),
      `Zone%` = md("**Zone%**"),
      `Swing%` = md("**Swing%**"),
      `Chase%` = md("**Chase%**"),
      `Z-Swing%` = md("**Z-Swing%**"),
      `Z-Whiff%` = md("**Z-Whiff%**"),
      `RV/100` = md("**RV/100**"),
      BBE = md("**BBE**"),
      wOBACON = md("**wOBACON**"),
      wOBA = md("**wOBA**"),
      xwOBACON = md("**xwOBACON**"),
      xwOBA = md("**xwOBA**")
    ) %>%
    cols_align("center") %>%
    fmt_percent(
      `Strike%`:`Z-Whiff%`,
      decimals = 1
    ) %>%
    tab_spanner(
      label = md("**Pitch Level**"),
      columns = c(`Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, 
                  `Z-Swing%`, `Z-Whiff%`)
    ) %>%
    tab_spanner(
      label = md("**PA Level**"),
      columns = c(wOBACON, wOBA, xwOBACON, xwOBA)
    ) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:14px;padding-bottom:14px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:16px;padding-right:16px;",
      locations = cells_column_labels()
    ) %>%
    tab_header(title = paste(name_first,name_last,"Overall Platoon Stats"))
  #tab_footnote(footnote = "Pitches Tracked by Trackman")
  
  return(stats_table)
  
}

Get_PitchType_Platoon_Pitch_Stats_2023 <- function(df, pitcher, date1, date2) {
  
  sc_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2)
  
  player_stats <- df %>%
    filter(
      PitchCall != "Undefined",
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other"
    ) %>%
    mutate(
      TopZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 2.9, 3.75), T, F),
      BotZoneCheck = if_else(between(PlateLocSide, -1.15, 1.15) & between(PlateLocHeight, 1.35, 2.2), T, F),
      is_BBE = if_else(PitchCall == "InPlay", T, F),
      is_BBE_EV = if_else(PitchCall == "InPlay" & TaggedHitType != "Bunt", T, F),
      is_Swing = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "StrikeSwinging","InPlay"), T, F),
      is_Whiff = if_else(PitchCall == "StrikeSwinging", T, F),
      is_CSW = if_else(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Strike = if_else(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay", "StrikeCalled"), T, F),
      is_Zone = if_else(between(PlateLocHeight, 1.6, 3.5) & between(PlateLocSide, -0.85, 0.85), T, F),
      is_SweetSpot = if_else(PitchCall == "InPlay" & between(Angle, 8, 32), T, F),
      is_HardHit = if_else(PitchCall == "InPlay" & ExitSpeed >= 95, T, F),
      is_CalledStrike = if_else(PitchCall == "StrikeCalled", T, F),
      is_Ball = if_else(PitchCall %in% c("BallCalled", "BallinDirt", "HitByPitch"), T, F),
      is_Foul = if_else(PitchCall %in% c("FoulBallNotFieldable", "FoulBall", "FoulBallFieldable"), T, F),
      is_Hit = if_else(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), T, F),
      is_GB = if_else(Angle < 10, T, F),
      is_FB = if_else(between(Angle, 25, 50), T, F),
      is_Single = if_else(PlayResult == "Single", T, F),
      is_Double = if_else(PlayResult == "Double", T, F),
      is_Triple = if_else(PlayResult == "Triple", T, F),
      is_HR = if_else(PlayResult == "HomeRun", T, F),
      is_Error = if_else(PlayResult == "Error", T, F),
      is_Sac = if_else(PlayResult == "Sacrifice", T, F),
      is_Dos = if_else(PitchCall == "HitByPitch", T, F),
      is_Strikeout = if_else(Strikes == 2 & PitchCall %in% c("StrikeSwinging", "StrikeCalled"), T, F),
      is_Walk = if_else(Balls == 3 & PitchCall %in% c("BallCalled", "BallinDirt"), T, F),
      xwOBA_weights = xwOBACON_value + (is_Walk * 0.7) + (is_Dos * 0.7),
      is_AB = is_BBE + is_Strikeout - is_Sac,
      is_PA = is_BBE + is_Strikeout + is_Walk + is_Dos
    ) %>%
    mutate(
      pitch_type = factor(
        TaggedPitchType,
        levels = c(
          "Fastball", "Sinker", "Cutter", "Slider", "Curveball", "ChangeUp"
        )
      )
    ) %>%
    group_by(BatterSide,pitch_type) %>%
    summarise(
      Pitches = n(),
      `FPS%` = round(mean(is_Strike[PitchofPA == 1]),5),
      `CSW%` = round((mean(is_CSW, na.rm = T)),5),
      `Put-Away%` = round(mean(is_CSW[Strikes == 2], na.rm = T),5),
      `Whiff%` = round(mean(is_Whiff[is_Swing == TRUE], na.rm = T),5),
      `Strike%` = round(mean(is_Strike, na.rm = T),5),
      `Zone%` = round(mean(is_Zone, na.rm = T),5),
      `Swing%` = round(mean(is_Swing, na.rm = T),5),
      `Chase%` = round(mean(is_Swing[is_Zone == FALSE], na.rm = TRUE),5),
      `Z-Swing%` = round(mean(is_Swing[is_Zone == TRUE], na.rm = T),5),
      `Z-Whiff%` = round(mean(is_Whiff[is_Zone == TRUE & is_Swing == TRUE]),5),
      BBE = sum(is_BBE),
      wOBA = round(mean(woba_value[is_PA == TRUE], na.rm = TRUE), 3),
      wOBACON = round(mean(woba_value[is_BBE == TRUE], na.rm = TRUE), 3),
      xwOBACON = round(mean(xwOBACON_value[is_BBE == TRUE], na.rm = T),3),
      xwOBA = round(mean(xwOBA_weights[is_PA == TRUE], na.rm = TRUE), 3),
      `RV/100` = round(mean(Run_Value, na.rm = T) * 100, 2),
    ) %>%
    ungroup() %>%
    group_by(BatterSide) %>%
    mutate(`Mix` = scales::percent(Pitches / sum(Pitches), accuracy = 0.1)) %>%
    ungroup() %>%
    ## select(TaggedPitchType, Pitches, xwOBA, `CSW%`, `Strike%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `Whiff%`, `Z-Swing%`, BBE, xwOBACON) %>%
    select(pitch_type, BatterSide,Pitches, `Mix`, `Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, 
           `Z-Whiff%`, `RV/100`, BBE, wOBACON, wOBA, xwOBACON, xwOBA) %>%
    arrange(BatterSide,desc(Pitches))
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    #gt(caption = paste(name_first,name_last,"Pitch Stats")) %>%
    gt() %>%
    data_color(
      pitch_type,
      method = "factor",
      palette = sc_colors
    ) %>%
    cols_label(
      pitch_type = md("**Pitch Type**"),
      BatterSide = md("**Batter Side**"),
      Pitches = md("**Pitches**"),
      `Mix` = md("**Mix**"),
      `CSW%` = md("**CSW%**"),
      `FPS%` = md("**FPS%**"),
      `Put-Away%` = md("**Put-Away%**"),
      `Whiff%` = md("**Whiff%**"),
      `Strike%` = md("**Strike%**"),
      `Zone%` = md("**Zone%**"),
      `Swing%` = md("**Swing%**"),
      `Chase%` = md("**Chase%**"),
      `Z-Whiff%` = md("**Z-Whiff%**"),
      `RV/100` = md("**RV/100**"),
      BBE = md("**BBE**"),
      wOBACON = md("**wOBACON**"),
      wOBA = md("**wOBA**"),
      xwOBACON = md("**xwOBACON**"),
      xwOBA = md("**xwOBA**")
    ) %>%
    cols_align("center") %>%
    fmt_percent(
      `Strike%`:`Z-Whiff%`,
      decimals = 1
    ) %>%
    tab_spanner(
      label = md("**Pitch Level**"),
      columns = c(`Strike%`, `FPS%`, `CSW%`, `Whiff%`, `Put-Away%`, `Zone%`, `Swing%`, `Chase%`, `Z-Whiff%`, `RV/100`)
    ) %>%
    tab_spanner(
      label = md("**PA Level**"),
      columns = c(wOBACON, wOBA, xwOBACON, xwOBA)
    ) %>%
    tab_spanner(
      label = md("**Pitch Usage**"),
      columns = c(Pitches, Mix)
    ) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:12px;padding-bottom:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:12px;padding-right:12px;",
      locations = cells_column_labels()
    ) %>%
    tab_header(title = paste(name_first,name_last,"Platoon Stats by Pitch Type"))
  #tab_footnote(footnote = "Pitches Tracked by Trackman")
  
  return(stats_table)
  
}

Get_Pitch_Movement_2023 <- function(df, pitcher, date1, date2) {
  
  sc_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  df <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           Date != "2023-10-06")
  
  player_stats <- df %>%
    filter(
      TaggedPitchType != "Undefined",
      TaggedPitchType != "Other") %>%
    mutate(TopZone = case_when(between(PlateLocHeight, 2.90, 3.75) & between(PlateLocSide, -1.15, 1.15) ~ TRUE, TRUE ~ FALSE),
           BotZone = case_when(between(PlateLocHeight, 1.4, 2.15) & between(PlateLocSide, -1.15, 1.15) ~ TRUE, TRUE ~ FALSE)) %>%
    mutate(
      pitch_type = factor(
        TaggedPitchType,
        levels = c(
          "Fastball", "Sinker", "Cutter", "Slider", "Curveball", "ChangeUp"
        )
      )
    ) %>%
    group_by(pitch_type) %>%
    summarise(Pitches = n(),
              Velo = (round(mean(RelSpeed, na.rm = T),1)),
              IVB = (round(mean(InducedVertBreak, na.rm = T),1)),
              HB = (round(mean(HorzBreak, na.rm = T),1)),
              SpinRate = (round(mean(SpinRate, na.rm = T))),
              SpinAxis = round(mean(SpinAxis, na.rm = T)),
              `Rel Ht.` = round(mean(RelHeight, na.rm = T),1),
              `Rel Si.` = round(mean(RelSide, na.rm = T),1),
              TopZoneVAA = round(mean(VertApprAngle[TopZone == TRUE], na.rm = T),2),
              BotZoneVAA = round(mean(VertApprAngle[BotZone == TRUE], na.rm = T),2),
              MaxVelo = round(max(RelSpeed, na.rm = T),1),
              `Ext.` = round(mean(Extension, na.rm = T),1),
              `wStuff+` = round(mean(wStuff_plus_2, na.rm = T)),
              `Stuff+` = round(mean(Stuff_plus, na.rm = T))
    ) %>%
    ungroup() %>%
    mutate(`Mix` = scales::percent(Pitches / sum(Pitches), accuracy = 0.1)) %>%
    select(pitch_type, Pitches, `Mix`, Velo, MaxVelo, IVB, HB, SpinRate, SpinAxis, `Rel Ht.`, `Rel Si.`, `Ext.`, TopZoneVAA, BotZoneVAA, `wStuff+`, `Stuff+`) %>%
    arrange(desc(Pitches))
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  stats_table <- player_stats %>% 
    gt() %>%
    data_color(
      pitch_type,
      method = "factor",
      palette = sc_colors
    ) %>%
    cols_label(
      pitch_type = md("**Pitch Type**"),
      Pitches = md("**Pitches**"),
      Mix = md("**Mix**"),
      Velo = md("**Velo**"),
      MaxVelo = md("**Max Velo**"),
      IVB = md("**IVB**"),
      HB = md("**HB**"),
      SpinRate = md("**Spin Rate**"),
      SpinAxis = md("**Spin Axis**"),
      `Rel Ht.` = md("**Rel. Ht.**"),
      `Rel Si.` = md("**Rel. Si.**"),
      `Ext.` = md("**Ext.**"),
      TopZoneVAA = md("**Top Zone**"),
      BotZoneVAA = md("**Bot. Zone**"),
      `wStuff+` = md("**wStuff+**")
    ) %>%
    cols_align("center") %>%
    tab_spanner(
      label = md("**Velcoity & Movement**"),
      columns = c(Velo, MaxVelo, IVB, HB, SpinRate, SpinAxis)
    ) %>%
    tab_spanner(
      label = md("**VAA**"),
      columns = c(TopZoneVAA, BotZoneVAA)
    ) %>%
    tab_spanner(
      label = md("**Release Point**"),
      columns = c(`Rel Ht.`, `Rel Si.`, `Ext.`)
    ) %>%
    tab_spanner(
      label = md("**Pitch Usage**"),
      columns = c(Pitches, Mix)
    ) %>%
    tab_spanner(
      label = md("**Pitch Models**"),
      columns = c(`wStuff+`, `Stuff+`)
    ) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:14px;padding-bottom:14px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:16px;padding-right:16px;",
      locations = cells_column_labels()
    ) %>%
    tab_header(title = paste(name_first,name_last,"Pitch Movement Stats")) %>%
    tab_footnote(footnote = "wStuff+: 100 Average / 50 Standard Deviation") %>%
    tab_footnote(footnote = "Stuff+: 100 Average / 10 Standard Deviation")
  
  return(stats_table)
  
}

Get_Pitch_Movement_Plot_2023 <- function(df, pitcher, date1, date2) {
  
  pitch_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  player_data <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           Date != "2023-10-06",
           TaggedPitchType != "Undefined", TaggedPitchType != "Other")
  
  player_cleaned_data <- player_data %>%
    dplyr::filter(!is.na(InducedVertBreak), !is.na(HorzBreak), !is.na(RelSpeed))
  
  player_pitch_types <- unique(player_cleaned_data$TaggedPitchType)
  
  player_pitch_avgs <- player_cleaned_data %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(InducedVertBreak = mean(InducedVertBreak, na.rm = T),
                     HorzBreak = mean(HorzBreak, na.rm = T))
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  ggplot2::ggplot(data = player_cleaned_data, aes(x = HorzBreak, y = InducedVertBreak, 
                                                  color = TaggedPitchType, size = RelSpeed)) +
    ggplot2::geom_point(alpha = 0.6) +
    geom_point(data = player_pitch_avgs, aes(x = HorzBreak, y = InducedVertBreak,fill = TaggedPitchType), 
               size = 6, alpha = 1, pch = 21, stroke =2, color = "#000000") +
    ggplot2::scale_color_manual(values = pitch_colors, limits = player_pitch_types) +
    scale_fill_manual(values = pitch_colors, limits = player_pitch_types) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 1) + 
    ggplot2::geom_vline(xintercept = 0, linewidth = 1) +
    ggplot2::xlim(-25, 25) +
    ggplot2::ylim(-25, 25) +
    ggplot2::labs(title = paste(name_first,name_last,"Pitch Movement"),
                  x = "Horizontal Break",
                  y = "Induced Vertical Break",
                  color = "", size = "Velocity", fill = "") +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed() +
    guides(size = "none",color = "none") + 
    theme(plot.title = element_text(colour = "black", size = 18, face = "bold",
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "black", size = 14,
                                       hjust = 0.5, vjust = 0.8, angle = 0),
          text=element_text(size=14),
          legend.position = "bottom",
          panel.border = element_rect(fill = NA, colour = "black", linewidth = 2),
          axis.title = element_text(face = "bold"))
  
}

Get_Release_Point_Plot <- function(df, pitcher, date1, date2) {
  
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  player_stats <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           TaggedPitchType != "Other", TaggedPitchType != "Undefined",
           Date != "2023-10-06")
  
  pitch_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  pitch_avgs <- player_stats %>% 
    filter(is.na(InducedVertBreak)==F,
           is.na(HorzBreak)==F) %>%
    group_by(TaggedPitchType) %>% 
    summarise(InducedVertBreak = mean(InducedVertBreak),
              HorzBreak = mean(HorzBreak))
  
  player_pitch_types <- unique(pitch_avgs$TaggedPitchType)
  
  p <- ggplot(player_stats, aes(x = RelSide, y = RelHeight, 
                                color = TaggedPitchType, 
                                alpha = I(0.65), size = 0.8)) +
    geom_point() +
    scale_color_manual(values = pitch_colors, limits = player_pitch_types) +
    xlim(-4,4) +
    ylim(0, 7) +
    labs(title = paste(name_first,name_last,"Release Locations"),
         color = "") +
    xlab("Pitcher's Perspective") +
    ylab("") +
    coord_fixed() +
    geom_curve(x = -3, xend = 3, y = 0, yend = 0,
               curvature = -.2, inherit.aes = F) + # Mound
    geom_segment(x=-5, xend=5, y=-0, yend =0, inherit.aes = F) + # Ground
    geom_segment(x=-.75, xend=.75, y=.35, yend =.35, inherit.aes = F) + # Rubber
    theme(legend.key.height= unit(.2, 'cm'),
          legend.key.width= unit(.2, 'cm'),
          legend.title = element_text(size=5),
          legend.text = element_text(size=5),
          axis.text=element_text(size=8)) +
    guides(size = "none",
           alpha = "none") + theme_bw() +
    theme(plot.title = element_text(colour = "black", size = 18, face = "bold",
                                    hjust = 0.5, vjust = 0.8, angle = 0),
          plot.subtitle = element_text(colour = "black", size = 14,
                                       hjust = 0.5, vjust = 0.8, angle = 0),
          text=element_text(size=14),
          legend.position = "bottom",
          panel.border = element_rect(fill=NA, colour = "black", linewidth = 2),
          axis.title = element_text(face="bold"))
  
  
  return(p)
  
}

Get_SpinAxis_Plot <- function(df, pitcher, date1, date2) {
  
  # add in list with statcast hex codes
  pitch_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  # Filter df to grab the pitcher's name
  player_name <- df %>%
    filter(Pitcher == pitcher) %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  # split the names between the comma
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  # filter the df to only contain pitches from the chosen pitcher
  player_stats <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           TaggedPitchType != "Other", TaggedPitchType != "Undefined",
           Date != "2023-10-06")
  
  # create df with the names of each pitch type the pitcher throws
  pitch_avgs <- player_stats %>% 
    filter(is.na(InducedVertBreak)==F,
           is.na(HorzBreak)==F) %>%
    group_by(TaggedPitchType) %>% 
    summarise(InducedVertBreak = mean(InducedVertBreak),
              HorzBreak = mean(HorzBreak))
  # create a list of these pitch types
  player_pitch_types <- unique(pitch_avgs$TaggedPitchType)
  
  # get the avg spin axis and spin rate for each of the pitchers pitch types
  player_pitch_avgs <- player_stats %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(SpinAxis = mean(SpinAxis, na.rm = T),
                     SpinRate = mean(SpinRate, na.rm = T))
  
  # create the plot
  p <- ggplot(player_stats, aes(x = SpinAxis, 
                                y = SpinRate, 
                                color = TaggedPitchType)) +
    geom_point(aes(color = TaggedPitchType), 
               size = 3, 
               alpha = 0.6) +
    scale_color_manual(values = pitch_colors, 
                       limits = player_pitch_types) +
    geom_point(data = player_pitch_avgs, 
               aes(x = SpinAxis, 
                   y = SpinRate, 
                   fill = TaggedPitchType), 
               size = 6, 
               alpha = 1, 
               pch = 21, 
               stroke =2, 
               color = "#000000") +
    scale_fill_manual(values = pitch_colors, 
                      limits = player_pitch_types) +
    labs(title = paste(name_first,name_last,"Spin Axis"),
         color = "",
         fill = "",
         x = "",
         y = "") +
    coord_polar(start = 180 * pi / 180) +
    scale_x_continuous(limits = c(0,360),
                       breaks = seq(0, 360, by = 30)) +
    scale_y_continuous(limits = c(1000,3000)) +
    theme(legend.key.height= unit(.2, 'cm'),
          legend.key.width= unit(.2, 'cm'),
          legend.title = element_text(size=5),
          legend.text = element_text(size=5),
          axis.text=element_text(size=8)) +
    guides(size = "none",
           alpha = "none",
           color = "none") + 
    theme_bw() +
    theme(plot.title = element_text(colour = "black", 
                                    size = 18, 
                                    face = "bold",
                                    hjust = 0.5,
                                    vjust = 0.8, 
                                    angle = 0),
          plot.subtitle = element_text(colour = "black", 
                                       size = 14,
                                       hjust = 0.5, 
                                       vjust = 0.8, 
                                       angle = 0),
          text=element_text(size=14),
          legend.position = "bottom",
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          panel.border = element_rect(fill = NA, 
                                      colour = "black", 
                                      linewidth = 2),
          axis.title = element_text(face="bold"))
  
  
  return(p)
  
}

Get_Pitch_Percentages_Platoon_2023 <- function(df, pitcher, date1, date2) {
  
  pitch_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "ChangeUp" = "#1DBE3A"
  )
  
  player_stats <- df %>%
    filter(Pitcher == pitcher,
           Date >= date1 & Date <= date2,
           TaggedPitchType != "Other", TaggedPitchType != "Undefined")
  
  player_name <- df %>%
    filter(Pitcher == pitcher)
  
  player_name <- player_name %>%
    group_by(Pitcher) %>%
    summarise(Pitches = n())
  
  name <- str_split_fixed(player_name$Pitcher,",",n = 2)
  name_first <- name[,2]
  name_last <- name[,1]
  
  player_stats <- player_stats %>%
    mutate(Count = paste(Balls, Strikes, sep = "-"),
           PitchClass = ifelse(TaggedPitchType %in% c("Fastball","Sinker"), "Fastball",
                               ifelse(TaggedPitchType %in% c("Slider", "Curveball", "Slurve", "Cutter"), 
                                      "Breaking Ball",
                                      ifelse(TaggedPitchType %in% c("ChangeUp", "Splitter"), "Off-speed", "Other"))),
           CountType = ifelse(Count %in%
                                c("1-0", "2-0",
                                  "2-1", "3-1"), "'Fastball Count'",
                              ifelse(Count %in%
                                       c("0-1", "0-2", "1-2", "2-2"), "Ahead",
                                     ifelse(Count %in% c("0-0", "1-1", "3-2"), "Even",
                                            "3-0"))))
  
  player_stats %>%
    group_by(BatterSide) %>%
    summarise(total_pitches = n()) -> test
  
  player_stats %>%
    group_by(BatterSide, TaggedPitchType) %>%
    summarise(pitches = n()) -> test_pt
  
  test_total <- left_join(test, test_pt, by = "BatterSide")
  
  test_total %>%
    group_by(BatterSide, TaggedPitchType) %>%
    summarise(pcts = round(100*(pitches / total_pitches),1)) -> count_pcts
  
  count_pcts <- count_pcts %>%
    arrange(desc(TaggedPitchType)) %>%
    mutate(text_y = cumsum(pcts) - pcts/2,
           labels = paste0(pcts,"%"))
  
  plot <- ggplot(count_pcts, aes(x="", y=pcts, fill=TaggedPitchType)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_manual(values = pitch_colors) +
    facet_wrap(~BatterSide, ncol = 2) +
    geom_label_repel(aes(label = labels, y = text_y), 
                     nudge_x = 0.68, nudge_y = 0.68,
                     size = 5, show.legend = F) +
    theme_void() +
    labs(title = paste(name_first,name_last,"Pitch% by Batter Side"),
         fill = "Pitch Type"
         #subtitle = paste("2023 Season")
    ) +
    theme(plot.title = element_text(colour = "black",
                                    face = "bold",
                                    size = 20,
                                    hjust = 0.5,
                                    vjust = 4,
                                    angle = 0),
          plot.subtitle = element_text(colour = "black",
                                       size = 13,
                                       hjust = 0.5,
                                       vjust = 5.5,
                                       angle = 0),
          strip.text = element_text(colour = "black",
                                    size = 12,
                                    face = "bold"),
    )
  
  return(plot)
  
  
}

Get_RHP_MLB_Avgs <- function() {
  
  sc_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "Splitter" = "#3BACAC",
    "Changeup" = "#1DBE3A"
  )
  
  ## Create the dataframes with MLB RHP averages ---
  RHP_Avgs <- data.frame(
    pitch_type  = c("Fastball", "Slider", "Sinker","Curveball","Changeup","Cutter","Splitter"),
    velo = c(94.1,84.8,93.7,79.9,86.0,89.6,87.3),
    ivb = c(16.1,1.9,8.6,-9.6,5.9,8.1,3.9),
    hb = c(7.2,-7.1,14.9,-9.2,14.1,-2.8,11.5),
    spinrate = c(2282,2448,2156,2542,1760,2400,1432),
    spinaxis = c(213,106,223,44,238,179,232),
    relht = c(5.8,5.7,5.6,5.9,5.8,5.9,5.9),
    relsi = c(1.8,1.9,1.9,1.7,1.9,1.8,1.8),
    ext = c(6.4,6.3,6.3,6.3,6.4,6.3,6.3),
    topvaa = c(-4.27,-6.28,-4.84,-8.38,-5.94,-5.36,-6.17),
    botvaa = c(-5.80,-7.79,-6.41,-9.68,-7.38,-7.01,-7.62)
  )
  
  
  RHP_Avgs %>%
    mutate(
      pitch_type = factor(
        pitch_type,
        levels = c(
          "Fastball", "Sinker", "Cutter", "Slider", "Curveball", "Splitter", "Changeup"
        )
      )
    ) %>%
    as_tibble() %>%
    gt() %>%
    cols_align("center") %>%
    cols_label(
      #p_throws = "",
      pitch_type = "Pitch Type",
      velo = "Velo",
      ivb = "IVB",
      hb = "HB",
      spinrate = "Spin Rate",
      spinaxis = "Spin Axis",
      relht = "Rel Ht.",
      relsi = "Rel Si.",
      ext = "Ext.",
      topvaa = "Top Zone",
      botvaa = "Bot. Zone"
    ) %>%
    fmt_number(
      velo:hb,
      decimals = 1
    ) %>%
    fmt_number(
      spinrate:spinaxis,
      decimals = 0
    ) %>%
    fmt_number(
      relht:ext,
      decimals = 1
    ) %>%
    fmt_number(
      topvaa:botvaa,
      decimals = 2
    ) %>%
    data_color(
      pitch_type,
      method = "factor",
      palette = sc_colors
    ) %>%
    tab_header(title = md("**Average MLB Right Handed Pitcher Pitch Characteristics**")) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:16px;padding-bottom:16px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:16px;padding-right:16px;",
      locations = cells_column_labels()
    ) %>%
    tab_spanner(
      label = md("**Velcoity & Movement**"),
      columns = c(velo, ivb, hb, spinrate, spinaxis)
    ) %>%
    tab_spanner(
      label = md("**VAA**"),
      columns = c(topvaa, botvaa)
    ) %>%
    tab_spanner(
      label = md("**Release Point**"),
      columns = c(relht, relsi, ext)
    )
  
  
}

Get_LHP_MLB_Avgs <- function() {
  
  sc_colors <- c(
    "Fastball" = "#D22D49",
    "Sinker" = "#FE9D00",
    "Cutter" = "#933F2C",
    "Slider" = "#EEE716",
    "Curveball" = "#00D1ED",
    "Splitter" = "#3BACAC",
    "Changeup" = "#1DBE3A"
  )
  
  ## Create the dataframes with MLB LHP averages ---
  LHP_Avgs <- data.frame(
    pitch_type  = c("Fastball", "Slider", "Sinker", "Curveball", "Changeup", "Cutter", "Splitter"),
    velo = c(93.0,84.0,92.6,78.5,84.1,87.2,84.2),
    ivb = c(16.0,1.7,9.1,-8.2,6.8,7.7,4.2),
    hb = c(-7.5,6.6,-15.0,8.6,-14.3,1.7,-8.6),
    spinrate = c(2248,2377,2102,2452,1718,2289,1041),
    spinaxis = c(146,246,134,306,121,172,120),
    relht = c(5.9,5.8,5.7,6.0,5.8,5.8,5.8),
    relsi = c(-1.8,-2.1,-2.1,1.8,2.0,2.0,1.5),
    ext = c(6.4,6.2,6.3,6.2,6.3,6.2,6.6),
    topvaa = c(-4.44,-6.45,-4.87,-8.46,-5.95,-5.46,-6.45),
    botvaa = c(-5.99,-8.00,-6.59,-9.65,-7.49,-7.10,-7.62)
  )
  
  
  LHP_Avgs %>%
    mutate(
      pitch_type = factor(
        pitch_type,
        levels = c(
          "Fastball", "Sinker", "Cutter", "Slider", "Curveball", "Splitter", "Changeup"
        )
      )
    ) %>%
    as_tibble() %>%
    gt() %>%
    cols_align("center") %>%
    cols_label(
      #p_throws = "",
      pitch_type = "Pitch Type",
      velo = "Velo",
      ivb = "IVB",
      hb = "HB",
      spinrate = "Spin Rate",
      spinaxis = "Spin Axis",
      relht = "Rel Ht.",
      relsi = "Rel Si.",
      ext = "Ext.",
      topvaa = "Top Zone",
      botvaa = "Bot. Zone"
    ) %>%
    fmt_number(
      velo:hb,
      decimals = 1
    ) %>%
    fmt_number(
      spinrate:spinaxis,
      decimals = 0
    ) %>%
    fmt_number(
      relht:ext,
      decimals = 1
    ) %>%
    fmt_number(
      topvaa:botvaa,
      decimals = 2
    ) %>%
    data_color(
      pitch_type,
      method = "factor",
      palette = sc_colors
    ) %>%
    tab_header(title = md("**Average MLB Left Handed Pitcher Pitch Characteristics**")) %>%
    tab_options(
      table.font.size = 20,
      heading.title.font.size = px(30)
    ) %>%
    tab_style(
      style = "padding-top:16px;padding-bottom:16px;",
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = "padding-left:16px;padding-right:16px;",
      locations = cells_column_labels()
    ) %>%
    tab_spanner(
      label = md("**Velcoity & Movement**"),
      columns = c(velo, ivb, hb, spinrate, spinaxis)
    ) %>%
    tab_spanner(
      label = md("**VAA**"),
      columns = c(topvaa, botvaa)
    ) %>%
    tab_spanner(
      label = md("**Release Point**"),
      columns = c(relht, relsi, ext)
    )
  
  
}

# user interface ----------
ui <- fluidPage(
  theme = shinythemes::shinytheme("simplex"),
  fixedRow(wellPanel(
    h1(id="big-heading", "CSUF Pitching App: Summary Stats Breakdown"),
    tags$style(HTML("#big-heading{color: black;}", "#big-heading{text-align: center;}")),
    selectInput("pitcher", "Pitcher:",
                choices = unique(Example_Pitching_App_Data$Pitcher)),
    hr(),
    dateRangeInput('dateRange',
                   label = 'Filter Games by Date',
                   start = as.Date('2024-02-16') , end = as.Date('2024-05-31')
    ),
    img(src = "https://pbs.twimg.com/media/CV3em8QUAAEFis_.png", 
        style = "display: block; margin-left: auto; margin-right: auto;", height = 260, width = 368)
  )),
  
  #titlePanel("CoopGraphs: Pitcher Team Stats"),
  
  
  ## Surface Level Stats Area --- First area you see when you open the app
  h1(id="title-heading", "CSUF Pitching App: Pitch Level Stats"),
  tags$style(HTML("#title-heading{color: black;}", "#title-heading{text-align: center;}")),
  
  shiny::column(12,align="center",gt_output("overall_stats_table")
  ),
  
  shiny::column(12,align="center",gt_output("stats_table")
  ),
  
  shiny::column(12,align="center",plotOutput("location_plot",
                                             height = 600,
                                             width = 1200)
  ),
  
  shiny::column(12,align="center",gt_output("attackzone_table")
  ),
  
  ## Pitch Movement Area ---
  h1(id="movement-heading", "CSUF Pitching App: Pitch Movement Breakdown"),
  tags$style(HTML("#movement-heading{color: black;}", "#movement-heading{text-align: center;}")),
  
  shiny::column(12,align="center", gt_output("movement_table")
  ),
  
  shiny::column(4,plotOutput("movement_plot",
                             height = 650,
                             width = 650),
                align="left"),
  shiny::column(4,plotOutput("release_plot",
                             height = 650,
                             width = 650),
                align="center"),
  shiny::column(4,plotOutput("spinaxis_plot",
                             height = 650,
                             width = 650),
                align="right"),
  
  ## MLB Avg Pitch Movement Area ---
  #h1(id="mlb-movement-heading", "CSUF Pitching App: MLB Pitch Movement Averages"),
  #tags$style(HTML("#mlb-movement-heading{color: black;}", "#mlb-movement-heading{text-align: center;}")),
  
  #shiny::column(12,align="center", gt_output("rhp_movement_table")
  #),
  #shiny::column(12,align="center", gt_output("lhp_movement_table")
  #),
  
  
  ## Platoon Splits Breakdown Area
  h1(id="subtitle2-heading", "CSUF Pitching App: Platoon Splits Breakdown"),
  tags$style(HTML("#subtitle2-heading{color: black;}", "#subtitle2-heading{text-align: center;}")),
  
  shiny::column(12, align="center", gt_output("platoon_overall_stats_table")
  ),
  
  shiny::column(12, align="center", gt_output("platoon_pitchtype_stats_table")
  ),
  
  shiny::column(12,align="center",plotOutput("platoon_location_plot",
                                             height = 700,
                                             width = 1300)
  ),
  
  shiny::column(12,align="center",plotOutput("platoon_percentages_plot",
                                             height = 700,
                                             width = 1200)
  ),
  
  
  ## Count Breakdown Area ---
  h1(id="subtitle-heading", "CSUF Pitching App: Count Stats Breakdown"),
  tags$style(HTML("#subtitle-heading{color: black;}", "#subtitle-heading{text-align: center;}")),
  
  shiny::column(12,align="center",gt_output("counttype_stats_table")
  ),
  
  shiny::column(12,align="center",gt_output("count_stats_table")
  ),
  
  shiny::column(12,align="center",plotOutput("percentage_type_plot",
                                             height = 700,
                                             width = 1200)
  ),
  
  shiny::column(12,align="center",plotOutput("percentage_plot",
                                             height = 700,
                                             width = 1200)
  )
  
)


# server ----------
server <- function(input, output, session) {
  
  output$stats_table <- render_gt({
    Get_Pitch_Stats_2023(Example_Pitching_App_Data,
                         input$pitcher,
                         input$dateRange[1],
                         input$dateRange[2])
  })
  
  output$overall_stats_table <- render_gt({
    Get_Overall_Pitch_Stats_2023(Example_Pitching_App_Data,
                                 input$pitcher,
                                 input$dateRange[1],
                                 input$dateRange[2])
  })
  
  output$location_plot <- renderPlot({
    Get_Pitch_Locations_2023(Example_Pitching_App_Data,
                             input$pitcher,
                             input$dateRange[1],
                             input$dateRange[2])
  }, res = 96)
  
  output$percentage_plot <- renderPlot({
    Get_Pitch_Percentages_2023(Example_Pitching_App_Data,
                               input$pitcher,
                               input$dateRange[1],
                               input$dateRange[2])
  }, res = 96)
  
  output$count_stats_table <- render_gt({
    Get_Pitch_Stats_Count_2023(Example_Pitching_App_Data,
                               input$pitcher,
                               input$dateRange[1],
                               input$dateRange[2])
  })
  
  output$percentage_type_plot <- renderPlot({
    Get_Pitch_Percentages_Type_2023(Example_Pitching_App_Data,
                                    input$pitcher,
                                    input$dateRange[1],
                                    input$dateRange[2])
  }, res = 96)
  
  output$counttype_stats_table <- render_gt({
    Get_Pitch_Stats_CountType_2023(Example_Pitching_App_Data,
                                   input$pitcher,
                                   input$dateRange[1],
                                   input$dateRange[2])
  })
  
  output$platoon_location_plot <- renderPlot({
    Get_Platoon_Pitch_Locations_2023(Example_Pitching_App_Data,
                                     input$pitcher,
                                     input$dateRange[1],
                                     input$dateRange[2])
  }, res = 96)
  
  output$platoon_overall_stats_table <- render_gt({
    Get_Overall_Platoon_Pitch_Stats_2023(Example_Pitching_App_Data,
                                         input$pitcher,
                                         input$dateRange[1],
                                         input$dateRange[2])
  })
  
  output$platoon_pitchtype_stats_table <- render_gt({
    Get_PitchType_Platoon_Pitch_Stats_2023(Example_Pitching_App_Data,
                                           input$pitcher,
                                           input$dateRange[1],
                                           input$dateRange[2])
  })
  
  output$movement_table <- render_gt({
    Get_Pitch_Movement_2023(Example_Pitching_App_Data,
                            input$pitcher,
                            input$dateRange[1],
                            input$dateRange[2])
  })
  
  output$attackzone_table <- render_gt({
    Get_AttackZone_Stats_2023(Example_Pitching_App_Data,
                            input$pitcher,
                            input$dateRange[1],
                            input$dateRange[2])
  })
  
  output$movement_plot <- renderPlot({
    Get_Pitch_Movement_Plot_2023(Example_Pitching_App_Data,
                                 input$pitcher,
                                 input$dateRange[1],
                                 input$dateRange[2])
  }, res = 96)
  
  output$release_plot <- renderPlot({
    Get_Release_Point_Plot(Example_Pitching_App_Data,
                           input$pitcher,
                           input$dateRange[1],
                           input$dateRange[2])
  }, res = 96)
  
  output$spinaxis_plot <- renderPlot({
    Get_SpinAxis_Plot(Example_Pitching_App_Data,
                      input$pitcher,
                      input$dateRange[1],
                      input$dateRange[2])
  }, res = 96)
  
  output$platoon_percentages_plot <- renderPlot({
    Get_Pitch_Percentages_Platoon_2023(Example_Pitching_App_Data,
                                       input$pitcher,
                                       input$dateRange[1],
                                       input$dateRange[2])
  }, res = 96)
  
  #output$rhp_movement_table <- render_gt({
  #  Get_RHP_MLB_Avgs()
  #})
  
  #output$lhp_movement_table <- render_gt({
  #  Get_LHP_MLB_Avgs()
  #})
  
  
}

# run app -------------
shinyApp(ui = ui, server = server)
