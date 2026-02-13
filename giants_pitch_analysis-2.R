# San Francisco Giants Pitch Speed Analysis System
# Analyzes batter performance vs pitch types/speeds and pitcher effectiveness

library(tidyverse)
library(ggplot2)

# ===== DATA GENERATION =====
set.seed(2025)

# Giants 2025 Roster (Key Batters)
giants_batters <- c(
  "Matt Chapman", "LaMonte Wade Jr.", "Tyler Fitzgerald", 
  "Patrick Bailey", "Mike Yastrzemski", "Heliot Ramos",
  "Jung Hoo Lee", "Marco Luciano", "Wilmer Flores"
)

# Pitch types with realistic MLB speed ranges
pitch_types <- list(
  "Fastball" = c(90, 100),
  "Curveball" = c(70, 85),
  "Slider" = c(80, 90),
  "Changeup" = c(78, 88),
  "Sinker" = c(88, 96),
  "Cutter" = c(85, 93)
)

# Generate batter performance data
generate_batter_data <- function() {
  data <- tibble()
  
  for (batter in giants_batters) {
    for (pitch_name in names(pitch_types)) {
      speed_range <- pitch_types[[pitch_name]]
      speeds <- seq(speed_range[1], speed_range[2], by = 1)
      
      for (speed in speeds) {
        base_skill <- runif(1, 0.200, 0.320)
        optimal_speed <- mean(speed_range)
        speed_penalty <- abs(speed - optimal_speed) * runif(1, -0.003, 0.001)
        avg <- max(0.100, min(0.500, base_skill + speed_penalty + rnorm(1, 0, 0.030)))
        
        pitcher_names <- c("Walker Buehler", "Zac Gallen", "Yu Darvish", 
                          "Blake Snell", "Joe Musgrove", "Merrill Kelly",
                          "Clayton Kershaw", "Tyler Glasnow", "Kyle Freeland")
        pitcher <- sample(pitcher_names, 1)
        
        # Add strike zone location (13 zones total)
        # Strike Zone (1-9):
        # 7 8 9 (high)
        # 4 5 6 (middle)  
        # 1 2 3 (low)
        # Chase Zones (10-13):
        # 10 = Low & Away (below zone, outside)
        # 11 = High & Away (above zone, outside)
        # 12 = Low & In (below zone, inside)
        # 13 = High & In (above zone, inside)
        zone <- sample(1:13, 1)
        
        # Adjust batting avg for chase zones (harder to hit)
        if (zone > 9) {
          avg <- avg * runif(1, 0.6, 0.85)  # Chase pitches harder to hit
        }
        
        data <- bind_rows(data, tibble(
          batter = batter,
          pitch_type = pitch_name,
          speed = speed,
          batting_avg = round(avg, 3),
          at_bats = sample(10:50, 1),
          pitcher_faced = pitcher,
          zone = zone
        ))
      }
    }
  }
  data
}

# Generate pitcher effectiveness data
generate_pitcher_data <- function() {
  giants_pitchers <- c(
    "Logan Webb", "Kyle Harrison", "Jordan Hicks",
    "Robbie Ray", "Blake Snell", "Camilo Doval"
  )
  
  data <- tibble()
  
  for (pitcher in giants_pitchers) {
    for (pitch_name in names(pitch_types)) {
      speed_range <- pitch_types[[pitch_name]]
      speeds <- seq(speed_range[1], speed_range[2], by = 1)
      
      for (speed in speeds) {
        nl_batters <- c("Mookie Betts", "Freddie Freeman", "Ketel Marte",
                       "Manny Machado", "Fernando Tatis Jr.", "Corbin Carroll",
                       "Christian Walker", "Xander Bogaerts", "Ryan McMahon")
        
        batter <- sample(nl_batters, 1)
        base_effectiveness <- runif(1, 0.180, 0.280)
        speed_factor <- rnorm(1, 0, 0.025)
        opp_avg <- max(0.100, min(0.450, base_effectiveness + speed_factor))
        
        # Add strike zone location (13 zones)
        zone <- sample(1:13, 1)
        
        # Adjust opponent avg for chase zones (pitchers more effective)
        if (zone > 9) {
          opp_avg <- opp_avg * runif(1, 0.6, 0.85)
        }
        
        data <- bind_rows(data, tibble(
          pitcher = pitcher,
          pitch_type = pitch_name,
          speed = speed,
          opponent_avg = round(opp_avg, 3),
          opponent = batter,
          batters_faced = sample(8:40, 1),
          zone = zone
        ))
      }
    }
  }
  data
}

# Generate datasets
cat("Generating data...\n")
batter_data <- generate_batter_data()
pitcher_data <- generate_pitcher_data()

# ===== ANALYSIS FUNCTIONS =====

analyze_pitch_type <- function(pitch_name) {
  if (!pitch_name %in% names(pitch_types)) {
    cat("Available pitch types:", paste(names(pitch_types), collapse=", "), "\n")
    return(NULL)
  }
  
  cat("\n==============================================\n")
  cat(paste("GIANTS BATTERS vs", toupper(pitch_name), "\n"))
  cat("==============================================\n\n")
  
  pitch_data <- batter_data %>%
    filter(pitch_type == pitch_name) %>%
    group_by(batter) %>%
    summarise(
      avg_ba = mean(batting_avg),
      max_speed_hit = max(speed[batting_avg >= 0.250]),
      best_speed = speed[which.max(batting_avg)],
      best_avg = max(batting_avg),
      pitcher_at_max = pitcher_faced[which.max(speed[batting_avg >= 0.250])],
      .groups = "drop"
    ) %>%
    arrange(desc(avg_ba))
  
  for (i in 1:nrow(pitch_data)) {
    row <- pitch_data[i,]
    cat(sprintf("%d. %s\n", i, row$batter))
    cat(sprintf("   Overall Avg vs %s: %.3f\n", pitch_name, row$avg_ba))
    cat(sprintf("   Fastest %s Hit Well (>.250): %d mph (vs %s)\n", 
                pitch_name, row$max_speed_hit, row$pitcher_at_max))
    cat(sprintf("   Best Performance: %.3f @ %d mph\n\n", 
                row$best_avg, row$best_speed))
  }
  
  return(invisible(pitch_data))
}

batter_breakdown <- function(batter_name) {
  if (!batter_name %in% giants_batters) {
    cat("Batter not found. Available batters:\n")
    cat(paste(giants_batters, collapse="\n"), "\n")
    return(NULL)
  }
  
  cat("\n==============================================\n")
  cat(paste("DETAILED BREAKDOWN:", toupper(batter_name), "\n"))
  cat("==============================================\n\n")
  
  player_data <- batter_data %>%
    filter(batter == batter_name)
  
  for (pitch in names(pitch_types)) {
    pitch_specific <- player_data %>%
      filter(pitch_type == pitch) %>%
      arrange(desc(batting_avg))
    
    cat(sprintf("%s:\n", pitch))
    cat(sprintf("  Best Speed: %d mph (%.3f avg)\n", 
                pitch_specific$speed[1], pitch_specific$batting_avg[1]))
    cat(sprintf("  Worst Speed: %d mph (%.3f avg)\n", 
                pitch_specific$speed[nrow(pitch_specific)], 
                pitch_specific$batting_avg[nrow(pitch_specific)]))
    
    struggles <- pitch_specific %>% filter(batting_avg < 0.200)
    if (nrow(struggles) > 0) {
      cat(sprintf("  Struggles at: %s mph\n", 
                  paste(struggles$speed, collapse=", ")))
    }
    cat("\n")
  }
}

pitcher_effectiveness <- function(pitcher_name = NULL) {
  if (is.null(pitcher_name)) {
    pitchers <- unique(pitcher_data$pitcher)
    cat("Available Giants Pitchers:\n")
    cat(paste(pitchers, collapse="\n"), "\n")
    return(NULL)
  }
  
  cat("\n==============================================\n")
  cat(paste("PITCHER ANALYSIS:", toupper(pitcher_name), "\n"))
  cat("==============================================\n\n")
  
  pitcher_stats <- pitcher_data %>%
    filter(pitcher == pitcher_name) %>%
    group_by(pitch_type) %>%
    summarise(
      avg_opponent_ba = mean(opponent_avg),
      most_effective_speed = speed[which.min(opponent_avg)],
      worst_opponent = opponent[which.max(opponent_avg)],
      best_opponent = opponent[which.min(opponent_avg)],
      .groups = "drop"
    ) %>%
    arrange(avg_opponent_ba)
  
  cat("Most Effective Pitches (Lower opponent avg = better):\n\n")
  
  for (i in 1:nrow(pitcher_stats)) {
    row <- pitcher_stats[i,]
    cat(sprintf("%d. %s - Opponent avg: %.3f\n", i, row$pitch_type, row$avg_opponent_ba))
    cat(sprintf("   Most effective at: %d mph\n", row$most_effective_speed))
    cat(sprintf("   Dominates: %s\n", row$best_opponent))
    cat(sprintf("   Vulnerable to: %s\n\n", row$worst_opponent))
  }
}

pitcher_vs_lineup <- function(pitcher_name = NULL) {
  if (is.null(pitcher_name)) {
    pitchers <- unique(pitcher_data$pitcher)
    cat("Available Giants Pitchers:\n")
    cat(paste(pitchers, collapse="\n"), "\n")
    return(NULL)
  }
  
  cat("\n==============================================\n")
  cat(paste("PITCHER MATCHUP GUIDE:", toupper(pitcher_name), "\n"))
  cat("==============================================\n\n")
  
  pitcher_stats <- pitcher_data %>%
    filter(pitcher == pitcher_name) %>%
    group_by(opponent) %>%
    summarise(
      overall_avg = mean(opponent_avg),
      best_pitch = pitch_type[which.min(opponent_avg)],
      best_pitch_speed = speed[which.min(opponent_avg)],
      best_pitch_avg = min(opponent_avg),
      worst_pitch = pitch_type[which.max(opponent_avg)],
      worst_pitch_speed = speed[which.max(opponent_avg)],
      worst_pitch_avg = max(opponent_avg),
      .groups = "drop"
    ) %>%
    arrange(overall_avg)
  
  cat("EASIEST TO HARDEST MATCHUPS:\n\n")
  
  for (i in 1:nrow(pitcher_stats)) {
    row <- pitcher_stats[i,]
    
    # Color code the matchup
    if (row$overall_avg < 0.220) {
      status <- "✓✓ DOMINATES"
    } else if (row$overall_avg < 0.250) {
      status <- "✓ ADVANTAGE"
    } else if (row$overall_avg < 0.280) {
      status <- "⚠ NEUTRAL"
    } else {
      status <- "✗ VULNERABLE"
    }
    
    cat(sprintf("%d. %s %s (Overall: %.3f)\n", i, row$opponent, status, row$overall_avg))
    cat(sprintf("   ATTACK WITH: %s @ %d mph (%.3f avg)\n", 
                row$best_pitch, row$best_pitch_speed, row$best_pitch_avg))
    cat(sprintf("   AVOID: %s @ %d mph (%.3f avg)\n\n", 
                row$worst_pitch, row$worst_pitch_speed, row$worst_pitch_avg))
  }
  
  return(invisible(pitcher_stats))
}

predict_matchup <- function(batter_name, pitch_type, speed) {
  result <- batter_data %>%
    filter(batter == batter_name, 
           pitch_type == pitch_type,
           speed == speed)
  
  if (nrow(result) == 0) {
    cat("No data for this combination\n")
    return(NULL)
  }
  
  cat(sprintf("\nPREDICTION: %s vs %d mph %s\n", 
              batter_name, speed, pitch_type))
  cat(sprintf("Expected Batting Average: %.3f\n", result$batting_avg[1]))
  
  avg_vs_pitch <- batter_data %>%
    filter(batter == batter_name, pitch_type == pitch_type) %>%
    summarise(mean(batting_avg)) %>%
    pull()
  
  diff <- result$batting_avg[1] - avg_vs_pitch
  
  if (diff > 0.030) {
    cat("Assessment: EXCELLENT matchup\n")
  } else if (diff > 0) {
    cat("Assessment: GOOD matchup\n")
  } else if (diff > -0.030) {
    cat("Assessment: FAIR matchup\n")
  } else {
    cat("Assessment: POOR matchup\n")
  }
}

gameplan_vs_team <- function(pitcher_name, team_name = "Dodgers") {
  # Define typical lineups
  lineups <- list(
    "Dodgers" = c("Mookie Betts", "Freddie Freeman"),
    "Padres" = c("Manny Machado", "Fernando Tatis Jr.", "Xander Bogaerts"),
    "Diamondbacks" = c("Ketel Marte", "Corbin Carroll", "Christian Walker"),
    "Rockies" = c("Ryan McMahon")
  )
  
  if (!team_name %in% names(lineups)) {
    cat("Available teams: Dodgers, Padres, Diamondbacks, Rockies\n")
    return(NULL)
  }
  
  lineup <- lineups[[team_name]]
  
  cat("\n╔════════════════════════════════════════════════════╗\n")
  cat(sprintf("║  GAME PLAN: %s vs %s\n", toupper(pitcher_name), toupper(team_name)))
  cat("╚════════════════════════════════════════════════════╝\n\n")
  
  for (batter in lineup) {
    batter_data_filtered <- pitcher_data %>%
      filter(pitcher == pitcher_name, opponent == batter)
    
    if (nrow(batter_data_filtered) == 0) next
    
    best <- batter_data_filtered %>%
      arrange(opponent_avg) %>%
      head(1)
    
    worst <- batter_data_filtered %>%
      arrange(desc(opponent_avg)) %>%
      head(1)
    
    avg_overall <- mean(batter_data_filtered$opponent_avg)
    
    cat(sprintf("═══ %s ═══\n", toupper(batter)))
    cat(sprintf("Overall Expected Avg: %.3f\n", avg_overall))
    cat(sprintf("✓ GO-TO PITCH: %s @ %d mph (%.3f)\n", 
                best$pitch_type, best$speed, best$opponent_avg))
    cat(sprintf("✗ AVOID: %s @ %d mph (%.3f)\n\n", 
                worst$pitch_type, worst$speed, worst$opponent_avg))
  }
}

plot_batter_heatmap <- function(batter_name) {
  player_data <- batter_data %>%
    filter(batter == batter_name)
  
  p <- ggplot(player_data, aes(x = speed, y = pitch_type, fill = batting_avg)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", mid = "yellow", high = "green",
                        midpoint = 0.250, 
                        limits = c(0.100, 0.400),
                        name = "Batting\nAverage") +
    labs(title = paste("Performance Heatmap:", batter_name),
         x = "Pitch Speed (mph)",
         y = "Pitch Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  ggsave(paste0(gsub(" ", "_", batter_name), "_heatmap.png"), 
         p, width = 10, height = 6, dpi = 300)
  
  cat(sprintf("Heatmap saved for %s\n", batter_name))
}

plot_strike_zone <- function(batter_name, pitch_type = "All") {
  # Filter data
  zone_data <- batter_data %>%
    filter(batter == batter_name)
  
  if (pitch_type != "All") {
    zone_data <- zone_data %>% filter(pitch_type == !!pitch_type)
  }
  
  # Aggregate by zone
  zone_summary <- zone_data %>%
    group_by(zone) %>%
    summarise(
      avg_ba = mean(batting_avg),
      pitches = n(),
      .groups = "drop"
    )
  
  # Create complete zone data (all 13 zones)
  all_zones_complete <- tibble(zone = 1:13) %>%
    left_join(zone_summary, by = "zone") %>%
    mutate(
      # Map zones to coordinates
      x = case_when(
        zone <= 9 ~ ((zone - 1) %% 3) + 2,
        zone == 12 ~ 2,  # Low & In
        zone == 13 ~ 2,  # High & In
        zone == 10 ~ 4,  # Low & Away
        zone == 11 ~ 4   # High & Away
      ),
      y = case_when(
        zone <= 9 ~ ceiling(zone / 3) + 1,
        zone == 12 ~ 1,
        zone == 10 ~ 1,
        zone == 13 ~ 5,
        zone == 11 ~ 5
      ),
      zone_type = if_else(zone <= 9, "strike", "chase"),
      display_text = if_else(is.na(avg_ba), "N/A", sprintf("%.3f", avg_ba))
    )
  
  # Create background grid for all positions
  all_positions <- expand.grid(x = 1:5, y = 1:5) %>%
    left_join(all_zones_complete, by = c("x", "y")) %>%
    mutate(
      is_zone = !is.na(zone),
      fill_value = if_else(is.na(avg_ba), 0.250, avg_ba)  # Use midpoint for N/A
    )
  
  p <- ggplot(all_positions, aes(x = x, y = y)) +
    # Draw all tiles
    geom_tile(aes(fill = fill_value, alpha = zone_type), 
              color = "black", linewidth = 1.5) +
    # Add strike zone border
    geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
              fill = NA, color = "black", linewidth = 3) +
    # Add text for all zones
    geom_text(data = filter(all_positions, is_zone), 
              aes(label = display_text,
                  color = if_else(display_text == "N/A", "black", "white")), 
              size = 5, fontface = "bold") +
    scale_fill_gradient2(
      low = "#d32f2f", 
      mid = "#ffc107", 
      high = "#4caf50",
      midpoint = 0.250,
      limits = c(0.100, 0.400),
      name = "Batting\nAverage",
      na.value = "gray70"
    ) +
    scale_color_identity() +
    scale_alpha_manual(
      values = c("strike" = 1.0, "chase" = 0.7),
      na.value = 0.15,
      guide = "none"
    ) +
    scale_x_continuous(breaks = NULL, limits = c(0.5, 5.5)) +
    scale_y_continuous(breaks = NULL, limits = c(0.5, 5.5)) +
    coord_fixed() +
    labs(
      title = paste("Strike Zone + Chase Areas:", batter_name),
      subtitle = if(pitch_type == "All") "All Pitch Types" else pitch_type
    ) +
    annotate("text", x = 1, y = 5, label = "High\nIn", size = 3, fontface = "bold") +
    annotate("text", x = 1, y = 1, label = "Low\nIn", size = 3, fontface = "bold") +
    annotate("text", x = 5, y = 5, label = "High\nAway", size = 3, fontface = "bold") +
    annotate("text", x = 5, y = 1, label = "Low\nAway", size = 3, fontface = "bold") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
      legend.position = "right",
      plot.margin = margin(10, 10, 10, 10)
    )
  
  filename <- paste0(gsub(" ", "_", batter_name), "_strike_zone")
  if (pitch_type != "All") {
    filename <- paste0(filename, "_", pitch_type)
  }
  filename <- paste0(filename, ".png")
  
  ggsave(filename, p, width = 9, height = 8, dpi = 300)
  
  cat(sprintf("\n=== STRIKE ZONE ANALYSIS: %s ===\n", batter_name))
  if (pitch_type != "All") cat(sprintf("Pitch Type: %s\n", pitch_type))
  cat("\n")
  
  # Sort and display
  zones_with_data <- all_zones_complete %>%
    filter(!is.na(avg_ba)) %>%
    arrange(desc(avg_ba))
  
  zones_without_data <- all_zones_complete %>%
    filter(is.na(avg_ba))
  
  cat("HOT ZONES (Best to Worst):\n\n")
  cat("STRIKE ZONE:\n")
  strike_zones <- zones_with_data %>% filter(zone <= 9)
  if (nrow(strike_zones) > 0) {
    for (i in 1:nrow(strike_zones)) {
      row <- strike_zones[i,]
      zone_name <- c("Low-Inside", "Low-Middle", "Low-Outside",
                     "Mid-Inside", "Mid-Middle", "Mid-Outside",
                     "High-Inside", "High-Middle", "High-Outside")[row$zone]
      
      status <- if(row$avg_ba >= 0.300) "🔥 CRUSHING" 
                else if(row$avg_ba >= 0.270) "✓ HOT"
                else if(row$avg_ba >= 0.230) "~ AVERAGE"
                else "❄ COLD"
      
      cat(sprintf("%d. Zone %d (%s): %.3f %s\n", 
                  i, row$zone, zone_name, row$avg_ba, status))
    }
  }
  
  cat("\nCHASE ZONES:\n")
  chase_zones <- zones_with_data %>% filter(zone > 9)
  if (nrow(chase_zones) > 0) {
    for (i in 1:nrow(chase_zones)) {
      row <- chase_zones[i,]
      zone_name <- c("Low & Away", "High & Away", "Low & In", "High & In")[row$zone - 9]
      
      status <- if(row$avg_ba >= 0.250) "⚠ CHASES" 
                else if(row$avg_ba >= 0.180) "~ SELECTIVE"
                else "✓ LAYS OFF"
      
      cat(sprintf("  Zone %d (%s): %.3f %s\n", 
                  row$zone, zone_name, row$avg_ba, status))
    }
  }
  
  if (nrow(zones_without_data) > 0) {
    cat("\nZONES WITH NO DATA:\n")
    for (i in 1:nrow(zones_without_data)) {
      row <- zones_without_data[i,]
      if (row$zone <= 9) {
        zone_name <- c("Low-Inside", "Low-Middle", "Low-Outside",
                       "Mid-Inside", "Mid-Middle", "Mid-Outside",
                       "High-Inside", "High-Middle", "High-Outside")[row$zone]
      } else {
        zone_name <- c("Low & Away", "High & Away", "Low & In", "High & In")[row$zone - 9]
      }
      cat(sprintf("  Zone %d (%s): N/A\n", row$zone, zone_name))
    }
  }
  
  cat(sprintf("\nVisualization saved: %s\n", filename))
  cat("\n💡 TIP: Chase zones (outside strike zone) typically show lower averages\n")
}

plot_pitcher_zone <- function(pitcher_name, pitch_type = "All") {
  # Filter data
  zone_data <- pitcher_data %>%
    filter(pitcher == pitcher_name)
  
  if (pitch_type != "All") {
    zone_data <- zone_data %>% filter(pitch_type == !!pitch_type)
  }
  
  # Aggregate by zone
  zone_summary <- zone_data %>%
    group_by(zone) %>%
    summarise(
      opp_avg = mean(opponent_avg),
      pitches = n(),
      .groups = "drop"
    )
  
  # Create complete zone data (all 13 zones)
  all_zones_complete <- tibble(zone = 1:13) %>%
    left_join(zone_summary, by = "zone") %>%
    mutate(
      x = case_when(
        zone <= 9 ~ ((zone - 1) %% 3) + 2,
        zone == 12 ~ 2,
        zone == 13 ~ 2,
        zone == 10 ~ 4,
        zone == 11 ~ 4
      ),
      y = case_when(
        zone <= 9 ~ ceiling(zone / 3) + 1,
        zone == 12 ~ 1,
        zone == 10 ~ 1,
        zone == 13 ~ 5,
        zone == 11 ~ 5
      ),
      zone_type = if_else(zone <= 9, "strike", "chase"),
      display_text = if_else(is.na(opp_avg), "N/A", sprintf("%.3f", opp_avg))
    )
  
  # Create background grid
  all_positions <- expand.grid(x = 1:5, y = 1:5) %>%
    left_join(all_zones_complete, by = c("x", "y")) %>%
    mutate(
      is_zone = !is.na(zone),
      fill_value = if_else(is.na(opp_avg), 0.250, opp_avg)
    )
  
  p <- ggplot(all_positions, aes(x = x, y = y)) +
    geom_tile(aes(fill = fill_value, alpha = zone_type), 
              color = "black", linewidth = 1.5) +
    geom_rect(aes(xmin = 1.5, xmax = 4.5, ymin = 1.5, ymax = 4.5),
              fill = NA, color = "black", linewidth = 3) +
    geom_text(data = filter(all_positions, is_zone), 
              aes(label = display_text,
                  color = if_else(display_text == "N/A", "black", "white")), 
              size = 5, fontface = "bold") +
    scale_fill_gradient2(
      low = "#4caf50",
      mid = "#ffc107", 
      high = "#d32f2f",
      midpoint = 0.250,
      limits = c(0.100, 0.400),
      name = "Opponent\nAverage",
      na.value = "gray70"
    ) +
    scale_color_identity() +
    scale_alpha_manual(
      values = c("strike" = 1.0, "chase" = 0.7),
      na.value = 0.15,
      guide = "none"
    ) +
    scale_x_continuous(breaks = NULL, limits = c(0.5, 5.5)) +
    scale_y_continuous(breaks = NULL, limits = c(0.5, 5.5)) +
    coord_fixed() +
    labs(
      title = paste("Zone Effectiveness:", pitcher_name),
      subtitle = if(pitch_type == "All") "All Pitch Types" else pitch_type
    ) +
    annotate("text", x = 1, y = 5, label = "High\nIn", size = 3, fontface = "bold") +
    annotate("text", x = 1, y = 1, label = "Low\nIn", size = 3, fontface = "bold") +
    annotate("text", x = 5, y = 5, label = "High\nAway", size = 3, fontface = "bold") +
    annotate("text", x = 5, y = 1, label = "Low\nAway", size = 3, fontface = "bold") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
      legend.position = "right",
      plot.margin = margin(10, 10, 10, 10)
    )
  
  filename <- paste0(gsub(" ", "_", pitcher_name), "_zone_effectiveness")
  if (pitch_type != "All") {
    filename <- paste0(filename, "_", pitch_type)
  }
  filename <- paste0(filename, ".png")
  
  ggsave(filename, p, width = 9, height = 8, dpi = 300)
  
  cat(sprintf("\n=== ZONE EFFECTIVENESS: %s ===\n", pitcher_name))
  if (pitch_type != "All") cat(sprintf("Pitch Type: %s\n", pitch_type))
  cat("\n")
  
  zones_with_data <- all_zones_complete %>%
    filter(!is.na(opp_avg)) %>%
    arrange(opp_avg)
  
  zones_without_data <- all_zones_complete %>%
    filter(is.na(opp_avg))
  
  cat("MOST TO LEAST EFFECTIVE ZONES:\n\n")
  cat("STRIKE ZONE:\n")
  strike_zones <- zones_with_data %>% filter(zone <= 9)
  if (nrow(strike_zones) > 0) {
    for (i in 1:nrow(strike_zones)) {
      row <- strike_zones[i,]
      zone_name <- c("Low-Inside", "Low-Middle", "Low-Outside",
                     "Mid-Inside", "Mid-Middle", "Mid-Outside",
                     "High-Inside", "High-Middle", "High-Outside")[row$zone]
      
      status <- if(row$opp_avg <= 0.200) "🔥 DOMINANT" 
                else if(row$opp_avg <= 0.240) "✓ EFFECTIVE"
                else if(row$opp_avg <= 0.270) "~ AVERAGE"
                else "⚠ VULNERABLE"
      
      cat(sprintf("%d. Zone %d (%s): %.3f opp avg %s\n", 
                  i, row$zone, zone_name, row$opp_avg, status))
    }
  }
  
  cat("\nCHASE ZONES:\n")
  chase_zones <- zones_with_data %>% filter(zone > 9)
  if (nrow(chase_zones) > 0) {
    for (i in 1:nrow(chase_zones)) {
      row <- chase_zones[i,]
      zone_name <- c("Low & Away", "High & Away", "Low & In", "High & In")[row$zone - 9]
      
      status <- if(row$opp_avg <= 0.150) "🎯 GET CHASES" 
                else if(row$opp_avg <= 0.200) "✓ EFFECTIVE"
                else "⚠ GETS HIT"
      
      cat(sprintf("  Zone %d (%s): %.3f opp avg %s\n", 
                  row$zone, zone_name, row$opp_avg, status))
    }
  }
  
  if (nrow(zones_without_data) > 0) {
    cat("\nZONES WITH NO DATA:\n")
    for (i in 1:nrow(zones_without_data)) {
      row <- zones_without_data[i,]
      if (row$zone <= 9) {
        zone_name <- c("Low-Inside", "Low-Middle", "Low-Outside",
                       "Mid-Inside", "Mid-Middle", "Mid-Outside",
                       "High-Inside", "High-Middle", "High-Outside")[row$zone]
      } else {
        zone_name <- c("Low & Away", "High & Away", "Low & In", "High & In")[row$zone - 9]
      }
      cat(sprintf("  Zone %d (%s): N/A\n", row$zone, zone_name))
    }
  }
  
  cat(sprintf("\nVisualization saved: %s\n", filename))
  cat("\n💡 TIP: Chase zones help generate swings & misses. Lower opp avg = more chases!\n")
}

# Save datasets to current working directory
write_csv(batter_data, "giants_batter_data.csv")
write_csv(pitcher_data, "giants_pitcher_data.csv")

cat("\nData generated!\n")
cat("Run: source('giants_pitch_analysis.R')\n\n")

cat("NEW FUNCTIONS:\n")
cat("- pitcher_vs_lineup('Logan Webb') - See matchups by opposing batter\n")
cat("- gameplan_vs_team('Blake Snell', 'Dodgers') - Full game plan vs team\n")
cat("- plot_strike_zone('Matt Chapman') - Batter hot/cold zones\n")
cat("- plot_strike_zone('Matt Chapman', 'Slider') - Zone map for specific pitch\n")
cat("- plot_pitcher_zone('Logan Webb') - Pitcher effectiveness by zone\n")
cat("- plot_pitcher_zone('Logan Webb', 'Changeup') - Zone map for specific pitch\n\n")

# Demo
analyze_pitch_type("Curveball")
batter_breakdown("Matt Chapman")
