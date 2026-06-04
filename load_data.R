##install.packages("ggrepel")
#install.packages("plotly")
library(readr)
df_G_M <- read_delim("Datos-G-M.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
head(df_G_M)
library(tidyverse)
df_G_M %>% mutate(across(c(Cod_Centro, Cod_Tit, Cod_Asig), as.character))

### Data validation
# Load required library
library(dplyr)
library(tidyr)

# Ensure df_G_M is available (assuming it is already loaded as a tibble)
# df_G_M <- ... 

# Load required libraries

library(stringr)

# ==============================================================================
# 1. Handle Missing Values
# ==============================================================================
# df_clean stores only records that have zero missing values across all columns
df_clean <- df_G_M %>% 
  drop_na()

# ==============================================================================
# 2. Filter Main Subjects
# ==============================================================================
# Extract standard subjects by omitting non-traditional administrative blocks.
# We use case-insensitive regex matching to catch variations in naming/coding.
df_main_subjects <- df_clean %>%
  filter(!str_detect(Nom_Asig, regex("Fin de Grado|Prácticas|Fin de Máster|Movilidad|Práctico", ignore_case = TRUE)))

# ==============================================================================
# 3. Create Structured & Consistent Analysis Dataset
# ==============================================================================
df_analysis <- df_main_subjects %>%
  # Safeguard: Remove physically impossible anomalies before subtraction 
  # to prevent negative counts which will crash the Binomial GLM models later.
  filter(
    Num_Mat >= Num_Pres,
    Num_Pres >= Num_Superan,
    Num_Mat > 0,
    Num_Pres >= 0
  ) %>%
  # Calculate the exact complements mathematically
  mutate(
    Num_No_Pres     = Num_Mat - Num_Pres,
    Num_No_Superan  = Num_Pres - Num_Superan
  ) %>%
  # Explicitly convert categorical variables to factors for the GLM step
  mutate(
    Cod_Centro    = as.factor(Cod_Centro),
    Centro        = as.factor(Centro),
    Nombre_Titulo = as.factor(Nombre_Titulo),
    Cod_Tit       = as.factor(Cod_Tit),
    Cod_Asig      = as.factor(Cod_Asig),
    Nom_Asig      = as.factor(Nom_Asig),
    Curso         = as.factor(Curso)
  ) %>%
  # Select and arrange columns exactly as requested
  select(
    Cod_Centro, Centro, Nombre_Titulo, Cod_Tit, Cod_Asig, Nom_Asig, 
    Num_Mat, Num_Pres, Num_Superan, Num_No_Pres, Num_No_Superan, Curso
  )

# ==============================================================================
# Diagnostic Summary
# ==============================================================================
cat("--- Data Processing Summary ---\n")
cat("Original Records (df_G_M):      ", nrow(df_G_M), "\n")
cat("No-NA Records (df_clean):        ", nrow(df_clean), "\n")
cat("Standard Subjects (df_main):     ", nrow(df_main_subjects), "\n")
cat("Final Analysis Ready (df_analys):", nrow(df_analysis), "\n")
#####################################################################
## Exploratory Data Analysis
########################################################################

# Load required libraries

library(ggplot2)
library(scales) # For cleaner axis formatting

# ==============================================================================
# 1. FEATURE ENGINEERING: Calculate Rates
# ==============================================================================
# We add descriptive rates. If Num_Pres is 0, Pass_Rate is set to NA.
df_eda <- df_analysis %>%
  mutate(
    Dropout_Rate = Num_No_Pres / Num_Mat,
    Pass_Rate    = if_else(Num_Pres > 0, Num_Superan / Num_Pres, NA_real_)
  )

# Quick summary profile of global metrics
cat("--- Global Metrics Summary ---\n")
df_eda %>%
  summarise(
    Avg_Dropout_Rate = mean(Dropout_Rate, na.rm = TRUE),
    Avg_Pass_Rate    = mean(Pass_Rate, na.rm = TRUE)
  ) %>%
  print()

################ 171 
df_eda_171 <- filter(df_analysis, Cod_Tit == '171')
df_eda_171 <- df_eda_171 %>%
  mutate(
    Dropout_Rate = Num_No_Pres / Num_Mat,
    Pass_Rate    = if_else(Num_Pres > 0, Num_Superan / Num_Pres, NA_real_)
  )

# Quick summary profile of global metrics
cat("--- Global Metrics Summary ---\n")
df_eda_171 %>%
  summarise(
    Avg_Dropout_Rate = mean(Dropout_Rate, na.rm = TRUE),
    Avg_Pass_Rate    = mean(Pass_Rate, na.rm = TRUE)
  ) %>%
  print()
################ 240
df_eda_240 <- filter(df_analysis, Cod_Tit == '240')
df_eda_240 <- df_eda_240 %>%
  mutate(
    Dropout_Rate = Num_No_Pres / Num_Mat,
    Pass_Rate    = if_else(Num_Pres > 0, Num_Superan / Num_Pres, NA_real_)
  )

# Quick summary profile of global metrics
cat("--- Global Metrics Summary ---\n")
df_eda_240 %>%
  summarise(
    Avg_Dropout_Rate = mean(Dropout_Rate, na.rm = TRUE),
    Avg_Pass_Rate    = mean(Pass_Rate, na.rm = TRUE)
  ) %>%
  print()
################ 241
df_eda_241 <- filter(df_analysis, Cod_Tit == '241')
df_eda_241 <- df_eda_241 %>%
  mutate(
    Dropout_Rate = Num_No_Pres / Num_Mat,
    Pass_Rate    = if_else(Num_Pres > 0, Num_Superan / Num_Pres, NA_real_)
  )

# Quick summary profile of global metrics
cat("--- Global Metrics Summary ---\n")
df_eda_241 %>%
  summarise(
    Avg_Dropout_Rate = mean(Dropout_Rate, na.rm = TRUE),
    Avg_Pass_Rate    = mean(Pass_Rate, na.rm = TRUE)
  ) %>%
  print()
################ 247
df_eda_247 <- filter(df_analysis, Cod_Tit == '247')
df_eda_247 <- df_eda_247 %>%
  mutate(
    Dropout_Rate = Num_No_Pres / Num_Mat,
    Pass_Rate    = if_else(Num_Pres > 0, Num_Superan / Num_Pres, NA_real_)
  )

# Quick summary profile of global metrics
cat("--- Global Metrics Summary ---\n")
df_eda_247 %>%
  summarise(
    Avg_Dropout_Rate = mean(Dropout_Rate, na.rm = TRUE),
    Avg_Pass_Rate    = mean(Pass_Rate, na.rm = TRUE)
  ) %>%
  print()






# 1. Isolate problematic records
df_problematic <- df_eda %>%
  filter(is.na(Pass_Rate) | is.nan(Pass_Rate) | is.infinite(Pass_Rate))





# 2. Print a quick summary to see why it's happening
cat("Total problem rows found:", nrow(df_problematic), "\n")

# 3. View a breakdown of the top degrees experiencing this issue
df_problematic %>%
  group_by(Cod_Tit) %>%
  summarise(
    problem_records_count = n(),
    # Check if these rows have 0 exam takers (adjust column names if different in your file)
    rows_with_zero_takers = sum(ifelse(Num_Superan + Num_No_Superan == 0, 1, 0), na.rm = TRUE)
  ) %>%
  arrange(desc(problem_records_count)) %>%
  print()

# 4. Look at a small sample of the raw data to confirm
df_problematic %>%
  select(Cod_Tit, Cod_Asig, Curso, matches("Superan|Matriculados|Presentados|Pass_Rate")) %>%
  head(10) %>%
  print()
# ==============================================================================
# 2. VISUALIZATION 1: Distribution of Pass Rates and Dropout Rates
# ==============================================================================
# This helps us see if the data is skewed or bimodal.

# Distribution of Pass Rates
plot1 <- ggplot(df_eda, aes(x = Pass_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#2b8cbe", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "St Pass Rates across Subjects",
    x = "Pass Rate (Passes / Exam Takers)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()

# Distribution of Dropout Rates
plot2 <- ggplot(df_eda, aes(x = Dropout_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#e34a33", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "St Dropout Rates across Subjects",
    x = "Dropout Rate (No-Shows / Enrolled)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()
#install.packages("gridExtra")
library(gridExtra)

grid.arrange(plot1, plot2, ncol=2)
############################# 171
plot1 <- ggplot(df_eda_171, aes(x = Pass_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#2b8cbe", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Pass Rates across Subjects (171)",
    x = "Pass Rate (Passes / Exam Takers)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()
# Distribution of Dropout Rates
plot2 <- ggplot(df_eda_171, aes(x = Dropout_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#e34a33", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Dropout Rates across Subjects (171)",
    x = "Dropout Rate (No-Shows / Enrolled)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()

grid.arrange(plot1, plot2, ncol=2)

############################# 240
plot1 <- ggplot(df_eda_240, aes(x = Pass_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#2b8cbe", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Pass Rates across Subjects (240)",
    x = "Pass Rate (Passes / Exam Takers)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()
# Distribution of Dropout Rates
plot2 <- ggplot(df_eda_240, aes(x = Dropout_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#e34a33", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Dropout Rates across Subjects (240)",
    x = "Dropout Rate (No-Shows / Enrolled)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()

grid.arrange(plot1, plot2, ncol=2)

############################# 241
plot1 <- ggplot(df_eda_241, aes(x = Pass_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#2b8cbe", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Pass Rates across Subjects (241)",
    x = "Pass Rate (Passes / Exam Takers)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()
# Distribution of Dropout Rates
plot2 <- ggplot(df_eda_241, aes(x = Dropout_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#e34a33", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Dropout Rates across Subjects (241)",
    x = "Dropout Rate (No-Shows / Enrolled)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()

grid.arrange(plot1, plot2, ncol=2)

############################# 247
plot1 <- ggplot(df_eda_247, aes(x = Pass_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#2b8cbe", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Pass Rates across Subjects (247)",
    x = "Pass Rate (Passes / Exam Takers)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()
# Distribution of Dropout Rates
plot2 <- ggplot(df_eda_247, aes(x = Dropout_Rate)) +
  geom_histogram(binwidth = 0.05, fill = "#e34a33", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = percent) +
  labs(
    title = "Dropout Rates across Subjects (247)",
    x = "Dropout Rate (No-Shows / Enrolled)",
    y = "Count of Subject-Year Records"
  ) +
  theme_minimal()

grid.arrange(plot1, plot2, ncol=2)

# ==============================================================================
# 3. VISUALIZATION 2: The Difficulty Landscape (Pass vs. Dropout)
# ==============================================================================
# This scatter plot visually establishes the "quadrants" of difficulty.
ggplot(df_eda, aes(x = Dropout_Rate, y = Pass_Rate)) +
  geom_point(aes(size = Num_Mat), alpha = 0.4, color = "#4a148c") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Subject Landscape: Pass Rate vs. Dropout Rate",
    x = "Dropout Rate (Inaction)",
    y = "Pass Rate (Success given Attempt)",
    size = "Enrolled Students"
  ) +
  theme_minimal()

# ==============================================================================
# 4. VISUALIZATION 3: Trend Over Academic Years (Curso)
# ==============================================================================
# Checking if grading or dropout trends shifted systematically across cohorts.
ggplot(df_eda, aes(x = Curso, y = Pass_Rate, fill = Curso)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Evolution of Pass Rates over Academic Years",
    x = "Academic Year (Curso)",
    y = "Pass Rate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_eda, aes(x = Curso, y = Dropout_Rate, fill = Curso)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Evolution of Drop-out Rates over Academic Years",
    x = "Academic Year (Curso)",
    y = "Drop-out Rate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########################################################
## COVID
#################################################
# Load required libraries

library(broom)

# Ensure rows with 0 attempts are excluded for the pass rate model 
# to avoid fitting issues, though quasibinomial handles weights well.
df_model_grading <- df_analysis %>% filter(Num_Pres > 0)

# ==============================================================================
# 1. QUANTIFYING THE SHIFT IN PASS RATES (Exam Lenency)
# ==============================================================================
# We use 'quasibinomial' instead of 'binomial' because educational data 
# usually exhibits overdispersion, which can artificially inflate p-values.

fit_pass_shift <- glm(
  cbind(Num_Superan, Num_No_Superan) ~ Curso, 
  data = df_model_grading, 
  family = quasibinomial(link = "logit")
)

cat("--- Statistical Analysis of Pass Rate Shifts by Academic Year ---\n")
pass_summary <- tidy(fit_pass_shift, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
  rename(
    Predictor = term,
    Odds_Ratio_Passing = estimate,
    P_Value = p.value
  )

print(pass_summary)

# ==============================================================================
# 2. QUANTIFYING THE SHIFT IN DROPOUT RATES (Student Retraction)
# ==============================================================================
fit_drop_shift <- glm(
  cbind(Num_No_Pres, Num_Pres) ~ Curso, 
  data = df_analysis, 
  family = quasibinomial(link = "logit")
)

cat("\n--- Statistical Analysis of Dropout Rate Shifts by Academic Year ---\n")
drop_summary <- tidy(fit_drop_shift, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
  rename(
    Predictor = term,
    Odds_Ratio_Dropping = estimate,
    P_Value = p.value
  )

print(drop_summary)
#################################################33
## Models for Pass and Drop-out
## GLM
################################################
# Load required libraries

library(purrr)


# ==============================================================================
# 1. DEFINE HELPER FUNCTIONS FOR CLEAN MODELING
# ==============================================================================
# These functions safely fit the GLM and return a clean, tidied data frame.
# We wrap them in tryCatch to prevent a single problematic degree from crashing the script.
# Re-merge into our clean, warning-free baseline profile
# full_join ensures subjects with 100% dropouts aren't deleted from the study


# 1. DEFINE ERROR-SHIELDED ENGINES
# 'possibly' catches errors and returns NULL instead of halting execution
fit_pass_model_safe <- possibly(function(data) {
  glm(cbind(Num_Superan, Num_No_Superan) ~ 0 + Cod_Asig + Curso, data = data, family = quasibinomial)
}, otherwise = NULL)

fit_drop_model_safe <- possibly(function(data) {
  glm(cbind(Num_No_Pres, Num_Presentados) ~ 0 + Cod_Asig + Curso, data = data, family = quasibinomial)
}, otherwise = NULL)


# 2. RUN THE UNIFIED DUAL-MODEL PIPELINE
df_models <- df_analysis %>%
  mutate(
    Cod_Asig = str_trim(as.character(Cod_Asig)),
    Cod_Tit  = str_trim(as.character(Cod_Tit)),
    Curso    = as.factor(Curso),
    Num_No_Superan  = Num_No_Superan, 
    Num_Presentados = Num_Mat - Num_No_Pres
  ) %>%
  group_by(Cod_Tit, Cod_Asig) %>%
  mutate(
    subject_occurrences = n(),
    grand_total_failed  = sum(Num_No_Superan, na.rm = TRUE),
    grand_total_passed  = sum(Num_Superan, na.rm = TRUE),
    grand_total_dropped = sum(Num_No_Pres, na.rm = TRUE),
    grand_total_present = sum(Num_Presentados, na.rm = TRUE)
  ) %>%
  filter(subject_occurrences > 1) %>% 
  mutate(
    Num_No_Superan  = ifelse(grand_total_failed == 0,  Num_No_Superan + 0.5,  Num_No_Superan),
    Num_Superan     = ifelse(grand_total_passed == 0,  Num_Superan + 0.5,     Num_Superan),
    Num_No_Pres     = ifelse(grand_total_dropped == 0, Num_No_Pres + 0.5,     Num_No_Pres),
    Num_Presentados = ifelse(grand_total_present == 0, Num_Presentados + 0.5, Num_Presentados)
  ) %>%
  ungroup() %>%
  select(-starts_with("grand_"), -subject_occurrences) %>%
  group_by(Cod_Tit) %>%
  nest() %>%
  # Map using our new error-insulated safe engines
  mutate(
    model_pass = map(data, fit_pass_model_safe),
    model_drop = map(data, fit_drop_model_safe)
  )

df_subject_pass_difficulty <- df_models %>%
  # Filter out programs where the model could not be computed
  filter(!map_lgl(model_pass, is.null)) %>%
  mutate(tidy_pass = map(model_pass, tidy)) %>%
  select(Cod_Tit, tidy_pass) %>%
  unnest(tidy_pass) %>%
  filter(str_detect(term, "Cod_Asig")) %>%
  mutate(
    Cod_Asig = str_replace(term, "Cod_Asig", ""),
    Pass_Log_Odds = estimate
  ) %>%
  select(Cod_Tit, Cod_Asig, Pass_Log_Odds, Pass_Std_Error = std.error, Pass_P_Value = p.value)

df_subject_drop_difficulty <- df_models %>%
  # Filter out programs where the model could not be computed
  filter(!map_lgl(model_drop, is.null)) %>%
  mutate(tidy_drop = map(model_drop, tidy)) %>%
  select(Cod_Tit, tidy_drop) %>%
  unnest(tidy_drop) %>%
  filter(str_detect(term, "Cod_Asig")) %>%
  mutate(
    Cod_Asig = str_replace(term, "Cod_Asig", ""),
    Drop_Log_Odds = estimate
  ) %>%
  select(Cod_Tit, Cod_Asig, Drop_Log_Odds, Drop_Std_Error = std.error, Drop_P_Value = p.value)

cat("Pass difficulty rows extracted:", nrow(df_subject_pass_difficulty), "\n")
cat("Drop difficulty rows extracted:", nrow(df_subject_drop_difficulty), "\n")


df_difficulty_profile <- df_subject_pass_difficulty %>%
  full_join(df_subject_drop_difficulty, by = c("Cod_Tit", "Cod_Asig"))

##########################33
## Reviewing exceptions
######
# 1. Isolate the exact data slice fed into the model for Degree 158
df_158_diagnostic <- df_analysis %>%
  mutate(
    Cod_Tit  = str_trim(as.character(Cod_Tit)),
    Cod_Asig = str_trim(as.character(Cod_Asig)),
    Curso    = as.factor(Curso)
  ) %>%
  filter(Cod_Tit == "158") %>%
  mutate(
    Total_Evaluated = Num_Superan + Num_No_Superan
  )

# 2. Test Scenario 1: Are there rows where nobody took the exam?
ghost_rows <- df_158_diagnostic %>% 
  filter(Total_Evaluated == 0)

cat("--- 👻 SCENARIO 1: GHOST ROWS ---\n")
if(nrow(ghost_rows) > 0) {
  print(ghost_rows %>% select(Curso, Cod_Asig, Nom_Asig, Num_Superan, Num_No_Superan))
} else {
  cat("No completely empty rows found. Moving to scenario 2...\n\n")
}

# 3. Test Scenario 2: Find sub-cells with absolute 100% or 0% pass rates
extreme_subcells <- df_158_diagnostic %>%
  filter(Total_Evaluated > 0) %>%
  mutate(Pass_Rate = Num_Superan / Total_Evaluated) %>%
  filter(Pass_Rate == 1 | Pass_Rate == 0)

cat("--- 🎯 SCENARIO 2: ABSOLUTE SUB-CELLS ---\n")
if(nrow(extreme_subcells) > 0) {
  print(extreme_subcells %>% 
          select(Curso, Cod_Asig, Nom_Asig, Num_Superan, Num_No_Superan, Pass_Rate) %>% 
          arrange(Pass_Rate))
} else {
  cat("No absolute 0% or 100% year-blocks found.\n")
}

# ==============================================================================
# 4. INSTANT DIAGNOSTIC CHECK (Verification)
# ==============================================================================
pure_dropout_subjects <- df_difficulty_profile %>% 
  filter(is.na(Pass_Log_Odds))

cat("--- Debugging Summary ---\n")
cat("Subjects with Pass Metrics:  ", nrow(df_subject_pass_difficulty), "\n")
cat("Subjects with Drop Metrics:  ", nrow(df_subject_drop_difficulty), "\n")
cat("Total Unique Combined:       ", nrow(df_difficulty_profile), "\n")
cat("Subjects identified as 100% total dropout courses: ", nrow(pure_dropout_subjects), "\n")


#############################
### Graphics
#############################
library(ggplot2)


# Calculate the median within each degree program to draw accurate local quadrant lines
df_vis_faceted <- df_difficulty_profile %>%
  group_by(Cod_Tit) %>%
  mutate(
    median_pass = median(Pass_Log_Odds, na.rm = TRUE),
    median_drop = median(Drop_Log_Odds, na.rm = TRUE)
  ) %>%
  ungroup()

##############################
#install.packages("ggrepel") # Run this if you don't have ggrepel installed
library(ggrepel) 

plot_degree_difficulty <- dbclear <- function(target_program_code) {
  
  # 1. Filter for the specific degree and calculate local medians
  df_prog <- df_difficulty_profile %>%
    filter(Cod_Tit == target_program_code) %>%
    mutate(
      median_pass = median(Pass_Log_Odds, na.rm = TRUE),
      median_drop = median(Drop_Log_Odds, na.rm = TRUE)
    )
  
  # 2. Flag the extreme outliers to label them (e.g., bottom 10% pass, top 10% drop)
  # This keeps the graph clean by only labeling the subjects that need attention.
  df_labeled <- df_prog %>%
    mutate(
      is_outlier = Pass_Log_Odds < quantile(Pass_Log_Odds, 0.15) | 
        Drop_Log_Odds > quantile(Drop_Log_Odds, 0.85)
    )
  
  # 3. Generate the plot
  ggplot(df_prog, aes(x = Drop_Log_Odds, y = Pass_Log_Odds)) +
    # Quadrant crosshairs
    geom_hline(aes(yintercept = median_pass), linetype = "dashed", color = "gray50") +
    geom_vline(aes(xintercept = median_drop), linetype = "dashed", color = "gray50") +
    
    # All subjects in this degree
    geom_point(color = "#4a148c", alpha = 0.6, size = 3) +
    
    # Smart text labels for outliers only
    geom_text_repel(
      data = filter(df_labeled, is_outlier == TRUE),
      aes(label = Cod_Asig),
      size = 3,
      max.overlaps = 15,
      box.padding = 0.5
    ) +
    
    labs(
      title = paste("Subject Difficulty Analysis for Program:", target_program_code),
      subtitle = "Dashed lines represent program medians. Labeled points represent high-risk subjects.",
      x = "Dropout Risk (Log-Odds of Dropping Out)",
      y = "Exam Success (Log-Odds of Passing)"
    ) +
    theme_minimal()
}

# HOW TO USE IT:
# Replace "YOUR_DEGREE_CODE" with an actual string from your Cod_Tit column
# plot_degree_difficulty("YOUR_DEGREE_CODE")
plot_degree_difficulty('171')
plot_degree_difficulty('240')
plot_degree_difficulty('241')
plot_degree_difficulty('247')
#plot_degree_difficulty('183')
#####################################3
library(purrr)

analyze_program_difficulty <- function(target_program_code, df_profile = df_difficulty_profile) {
  
  # ============================================================================
  # 1. FILTER & CLASSIFY QUADRANTS
  # ============================================================================
  # Filter for the target program and calculate local baseline medians
  df_prog <- df_profile %>%
    filter(Cod_Tit == target_program_code)
  
  if (nrow(df_prog) == 0) {
    stop(paste("Program code", target_program_code, "not found in the dataset."))
  }
  
  med_pass <- median(df_prog$Pass_Log_Odds, na.rm = TRUE)
  med_drop <- median(df_prog$Drop_Log_Odds, na.rm = TRUE)
  
  # Assign each subject to an explicit academic risk quadrant
  df_analyzed <- df_prog %>%
    mutate(
      Quadrant = case_when(
        Drop_Log_Odds >= med_drop & Pass_Log_Odds < med_pass  ~ "Critical Bottleneck",
        Drop_Log_Odds < med_drop  & Pass_Log_Odds < med_pass  ~ "Deceptively Hard Exam",
        Drop_Log_Odds >= med_drop & Pass_Log_Odds >= med_pass ~ "High Workload Survivor",
        Drop_Log_Odds < med_drop  & Pass_Log_Odds >= med_pass ~ "Accessible / Engaging"
      ),
      Quadrant = factor(Quadrant, levels = c("Critical Bottleneck", "Deceptively Hard Exam", 
                                             "High Workload Survivor", "Accessible / Engaging"))
    )
  
  # ============================================================================
  # 2. GENERATE THE POLISHED VISUALIZATION
  # ============================================================================
  # Define meaningful institutional colors for the quadrants
  quad_colors <- c(
    "Critical Bottleneck"    = "#d95f02", # Warning Orange/Red
    "Deceptively Hard Exam"  = "#7570b3", # Soft Purple
    "High Workload Survivor" = "#e7298a", # Deep Pink
    "Accessible / Engaging"  = "#1b9e77"  # Safe Green
  )
  
  plot_out <- ggplot(df_analyzed, aes(x = Drop_Log_Odds, y = Pass_Log_Odds, color = Quadrant)) +
    # Draw local reference boundaries
    geom_hline(yintercept = med_pass, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    geom_vline(xintercept = med_drop, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    
    # Plot subjects
    geom_point(alpha = 0.7, size = 3.5) +
    
    # Label problematic subjects (Bottlenecks and Hard Exams)
    geom_text_repel(
      data = filter(df_analyzed, Quadrant %in% c("Critical Bottleneck", "Deceptively Hard Exam")),
      aes(label = Cod_Asig),
      size = 3,
      color = "black",
      max.overlaps = 20,
      box.padding = 0.4,
      point.padding = 0.2
    ) +
    
    scale_color_manual(values = quad_colors) +
    labs(
      title = paste("Academic Performance Diagnostic: Program", target_program_code),
      subtitle = "Dashed lines represent program medians. Labeled subjects require structural review.",
      x = "Dropout Risk (Log-Odds of Student Abandonment)",
      y = "Exam Evaluation Success (Log-Odds of Passing)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.background = element_rect(fill = "#fafafa", color = NA)
    )
  
  # ============================================================================
  # 3. CREATE PRIORITIZED REPORT TABLES
  # ============================================================================
  # Isolate the top 5 most critical bottleneck subjects (highest dropout, lowest pass odds)
  top_bottlenecks <- df_analyzed %>%
    filter(Quadrant == "Critical Bottleneck") %>%
    arrange(Pass_Log_Odds, desc(Drop_Log_Odds)) %>%
    head(5) %>%
    select(Cod_Asig, Pass_Log_Odds, Drop_Log_Odds)
  
  top_deceptively_hard <- df_analyzed %>%
    filter(Quadrant == "Deceptively Hard Exam") %>%
    arrange(Pass_Log_Odds, desc(Drop_Log_Odds)) %>%
    head(5) %>%
    select(Cod_Asig, Pass_Log_Odds, Drop_Log_Odds)
  
  top_accesible <- df_analyzed %>%
    filter(Quadrant == "Accessible / Engaging") %>%
    arrange(desc(Pass_Log_Odds), desc(Drop_Log_Odds)) %>%
    head(5) %>%
    select(Cod_Asig, Pass_Log_Odds, Drop_Log_Odds)
  
  top_high_workload <- df_analyzed %>%
    filter(Quadrant == "High Workload Survivor") %>%
    arrange(Pass_Log_Odds, desc(Drop_Log_Odds)) %>%
    head(5) %>%
    select(Cod_Asig, Pass_Log_Odds, Drop_Log_Odds)
  
  
  # Display the results directly to the console
  cat("\n==================================================\n")
  cat(" DIAGNOSTIC REPORT FOR PROGRAM:", target_program_code, "\n")
  cat("==================================================\n")
  cat("Total Active Subjects Evaluated:", nrow(df_analyzed), "\n\n")
  cat("Top 5 Priority Action Subjects (Critical Bottlenecks):\n")
  print(top_bottlenecks)
  cat("Top 5 Priority Action Subjects (Deceptively Hard Exam):\n")
  cat("==================================================\n")
  print(top_deceptively_hard)
  cat("Top 5 Priority Action Subjects (Accesible):\n")
  cat("==================================================\n")
  print(top_accesible)
  cat("Top 5 Priority Action Subjects (High Workload):\n")
  cat("==================================================\n")
  print(top_high_workload)
  
  # Display the plot
  print(plot_out)
  
  # Return the fully classified dataset silently for further custom reporting
  return(invisible(df_analyzed))
}

# Example execution (Replace 'DEGREE_01' with an actual code from your Cod_Tit column)
program_data_171 <- analyze_program_difficulty("171")
#write.csv(program_data, file =  "program_data_171.csv")
#mutate(program_data_171, is_outlier = Pass_Log_Odds < 
#              quantile(Pass_Log_Odds, 0.15) | Drop_Log_Odds > quantile(Drop_Log_Odds, 0.85)) %>%
#filter(is_outlier == TRUE) %>% arrange(Pass_Log_Odds)

program_data_240 <- analyze_program_difficulty("240")
program_data_241 <- analyze_program_difficulty("241")
program_data_247 <- analyze_program_difficulty("247")
##########################################3
#install.packages("plotly") # Run this if you don't have plotly installed
library(plotly)

analyze_program_interactive <- function(target_program_code, df_profile = df_difficulty_profile) {
  
  # 1. Filter and calculate local program medians
  df_prog <- df_profile %>%
    filter(Cod_Tit == target_program_code)
  
  if (nrow(df_prog) == 0) {
    stop(paste("Program code", target_program_code, "not found in the dataset."))
  }
  
  med_pass <- median(df_prog$Pass_Log_Odds, na.rm = TRUE)
  med_drop <- median(df_prog$Drop_Log_Odds, na.rm = TRUE)
  
  # 2. Assign behavioral quadrants
  df_analyzed <- df_prog %>%
    mutate(
      Quadrant = case_when(
        Drop_Log_Odds >= med_drop & Pass_Log_Odds < med_pass  ~ "Critical Bottleneck",
        Drop_Log_Odds < med_drop  & Pass_Log_Odds < med_pass  ~ "Deceptively Hard Exam",
        Drop_Log_Odds >= med_drop & Pass_Log_Odds >= med_pass ~ "High Workload Survivor",
        Drop_Log_Odds < med_drop  & Pass_Log_Odds >= med_pass ~ "Accessible / Engaging"
      )
    )
  
  # Color palette configuration
  quad_colors <- c(
    "Critical Bottleneck"    = "#d95f02",
    "Deceptively Hard Exam"  = "#7570b3",
    "High Workload Survivor" = "#e7298a",
    "Accessible / Engaging"  = "#1b9e77"
  )
  
  # 3. Build the base ggplot structure with a custom interactive tooltip string
  base_plot <- ggplot(df_analyzed, aes(
    x = Drop_Log_Odds, 
    y = Pass_Log_Odds, 
    color = Quadrant,
    # This text aesthetic constructs the interactive hover window layout
    text = paste0(
      "<b>Subject Code:</b> ", Cod_Asig, "<br>",
      "<b>Risk Category:</b> ", Quadrant, "<br>",
      "<b>Exam Pass (Log-Odds):</b> ", round(Pass_Log_Odds, 3), "<br>",
      "<b>Dropout (Log-Odds):</b> ", round(Drop_Log_Odds, 3)
    )
  )) +
    geom_hline(yintercept = med_pass, linetype = "dashed", color = "gray60") +
    geom_vline(xintercept = med_drop, linetype = "dashed", color = "gray60") +
    geom_point(alpha = 0.7, size = 3) +
    scale_color_manual(values = quad_colors) +
    labs(
      title = paste("Interactive Performance Diagnostic: Program", target_program_code),
      x = "Dropout Risk (Log-Odds)",
      y = "Exam Success (Log-Odds)"
    ) +
    theme_minimal() +
    theme(legend.position = "none") # Handled natively by Plotly's interactive legend
  
  # 4. Transform into a smooth HTML interactive graphic
  interactive_plot <- ggplotly(base_plot, tooltip = "text") %>%
    layout(
      margin = list(t = 60, b = 40),
      title = list(font = list(size = 15, face = "bold"))
    )
  
  # Display the interactive widget in your RStudio Viewer / R Markdown output
  return(interactive_plot)
}
analyze_program_interactive("171")
analyze_program_interactive("240")
analyze_program_interactive("241")
analyze_program_interactive("247")
##########################################
## Relationship between pass rate and drop-out rate in a degree.

analyze_degree_correlation <- function(target_program_code, df_profile = df_difficulty_profile) {
  
  # 1. Filter dataset for the target degree program
  df_prog <- df_profile %>% 
    filter(Cod_Tit == target_program_code)
  
  # Structural Check: Ensure there are enough data points to compute correlation
  # (Correlations require a minimum of 3 complete points to calculate variance)
  df_complete <- df_prog %>% filter(!is.na(Pass_Log_Odds) & !is.na(Drop_Log_Odds))
  n_obs <- nrow(df_complete)
  
  if (n_obs < 3) {
    stop(paste("Program", target_program_code, "only has", n_obs, 
               "complete subject records. Correlation cannot be reliably computed."))
  }
  
  # 2. Run Pearson Correlation Hypothesis Test
  # 'complete.obs' safely ignores the 100% total dropout rows (NAs) during computation
  test_res <- cor.test(df_prog$Drop_Log_Odds, df_prog$Pass_Log_Odds, 
                       use = "complete.obs", method = "pearson")
  
  r_value <- test_res$estimate
  p_value <- test_res$p.value
  
  # 3. Translate coefficients into institutional insight
  interpretation <- case_when(
    r_value <= -0.5 & p_value < 0.05 ~ "Strong Negative Correlation (High dropouts heavily track with failing exams)",
    r_value <= -0.2 & p_value < 0.05 ~ "Moderate Negative Correlation (General trend connecting abandonment to difficulty)",
    r_value >= 0.4  & p_value < 0.05 ~ "Positive Correlation (High dropouts act as a filter; leaving an easier exam for survivors)",
    p_value >= 0.05                  ~ "No Statistically Significant Linear Relationship detected"
  )
  
  # 4. Print Summary Report to Console
  cat("\n==================================================\n")
  cat(" CORRELATION ANALYSIS FOR PROGRAM:", target_program_code, "\n")
  cat("==================================================\n")
  cat("Active Subjects (Complete Cases): ", n_obs, "\n")
  cat("Correlation Coefficient (r):     ", round(r_value, 4), "\n")
  cat("Significance Level (p-value):     ", format.pval(p_value, digits = 4), "\n")
  cat("Curriculum Assessment:           ", interpretation, "\n")
  cat("==================================================\n")
  
  # 5. Generate Explanatory Graphic
  plot_out <- ggplot(df_prog, aes(x = Drop_Log_Odds, y = Pass_Log_Odds)) +
    # Add linear regression trend line with a 95% confidence interval band
    geom_smooth(method = "lm", formula = y ~ x, color = "#e34a33", fill = "#fdbb84", alpha = 0.25) +
    
    # Add individual subject coordinate points
    geom_point(color = "#4a148c", alpha = 0.6, size = 3) +
    
    # Add data box on the chart showing statistical details
    annotate("label", 
             x = min(df_prog$Drop_Log_Odds, na.rm = TRUE), 
             y = max(df_prog$Pass_Log_Odds, na.rm = TRUE),
             label = paste0("Pearson r = ", round(r_value, 3), "\np-value = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, fill = "#ffffff", alpha = 0.85, fontface = "bold", size = 3.5) +
    
    labs(
      title = paste("Pass Rate vs. Dropout Rate Correlation: Program", target_program_code),
      subtitle = "Controlled log-odds metrics. A downward slope reveals that high-abandonment courses also feature harder evaluations.",
      x = "Controlled Dropout Risk (Log-Odds of Abandonment)",
      y = "Controlled Exam Success (Log-Odds of Passing)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.background = element_rect(fill = "#fcfcfc", color = NA)
    )
  
  # Render the plot to the active window
  print(plot_out)
  
  # Silently return data metrics for downstream automation workflows
  return(invisible(list(r = r_value, p_value = p_value, n = n_obs)))
}
# Substitute 'YOUR_PROGRAM_CODE' with a valid string identifier from your data
stats_output_171 <- analyze_degree_correlation("171")
stats_output_240 <- analyze_degree_correlation("240")
stats_output_241 <- analyze_degree_correlation("241")
stats_output_247 <- analyze_degree_correlation("247")

##################################33
## Dispersion check
#################################

df_dispersion_check <- df_models %>%
  ungroup() %>%
  # Filter out any programs where the pass model failed to fit
  filter(!map_lgl(model_pass, is.null)) %>%
  mutate(
    # 1. Extract Residual Deviance
    res_deviance = map_dbl(model_pass, ~ deviance(.x)),
    
    # 2. Extract Residual Degrees of Freedom
    res_df = map_dbl(model_pass, ~ df.residual(.x)),
    
    # 3. Calculate the Classical Deviance Ratio
    deviance_ratio = res_deviance / res_df,
    
    # 4. Extract the Quasibinomial Estimated Phi (Dispersion Parameter)
    # This is the exact correction factor calculated by the quasi-family
    phi_parameter = map_dbl(model_pass, ~ summary(.x)$dispersion),
    
    # 5. Run a Formal Chi-Squared Goodness-of-Fit Test
    p_value_chi2 = map_dbl(model_pass, ~ pchisq(deviance(.x), df.residual(.x), lower.tail = FALSE))
  ) %>%
  # Clean up the output table for easy viewing
  select(Cod_Tit, res_deviance, res_df, deviance_ratio, phi_parameter, p_value_chi2)

# Print the top 10 rows to your console so you can copy/paste it to me
print(head(df_dispersion_check, 10))
filter(df_dispersion_check, Cod_Tit %in% c('171', '240', '241', '247'))
###########################################3
#### Risk matrix
######################################3

generate_degree_risk_matrix <- function(target_program_code, df_profile = df_difficulty_profile) {
  
  # 1. Filter and isolate complete subject cases for this specific degree
  df_prog <- df_profile %>%
    filter(Cod_Tit == target_program_code) %>%
    filter(!is.na(Pass_Log_Odds) & !is.na(Drop_Log_Odds)) %>%
    # Invert pass odds so higher values mean a more severe academic barrier (Impact)
    mutate(failure_impact = -Pass_Log_Odds)
  
  if (nrow(df_prog) == 0) {
    stop(paste("No complete data found for program code:", target_program_code))
  }
  
  # 2. Establish local risk baselines using the program's medians
  med_x <- median(df_prog$Drop_Log_Odds)
  med_y <- median(df_prog$failure_impact)
  
  # Define extreme padding boundaries to perfectly fit the background color matrix tiles
  pad <- 0.5
  min_x <- min(df_prog$Drop_Log_Odds) - pad
  max_x <- max(df_prog$Drop_Log_Odds) + pad
  min_y <- min(df_prog$failure_impact) - pad
  max_y <- max(df_prog$failure_impact) + pad
  
  # 3. Classify every subject into an institutional Risk Zone
  df_ranked <- df_prog %>%
    mutate(
      Risk_Zone = case_when(
        Drop_Log_Odds >= med_x & failure_impact >= med_y ~ "CRITICAL (Red Zone)",
        Drop_Log_Odds < med_x  & failure_impact >= med_y ~ "HIGH IMPACT (Yellow Zone)",
        Drop_Log_Odds >= med_x & failure_impact < med_y  ~ "HIGH LIKELIHOOD (Orange Zone)",
        Drop_Log_Odds < med_x  & failure_impact < med_y  ~ "LOW RISK (Green Zone)"
      ),
      # Create a numerical Priority Rank (Higher failure + Higher dropout = Higher priority)
      Priority_Score = scale(Drop_Log_Odds) + scale(failure_impact)
    ) %>%
    arrange(desc(Priority_Score))
  
  # 4. Generate the Visual Risk Matrix Plot
  risk_plot <- ggplot(df_ranked, aes(x = Drop_Log_Odds, y = failure_impact)) +
    
    # Paint the Background Matrix Tiles
    # Low Risk (Bottom-Left: Green)
    geom_rect(aes(xmin = min_x, xmax = med_x, ymin = min_y, ymax = med_y), fill = "#d4edda", alpha = 0.1) +
    # High Impact (Top-Left: Yellow)
    geom_rect(aes(xmin = min_x, xmax = med_x, ymin = med_y, ymax = max_y), fill = "#fff3cd", alpha = 0.1) +
    # High Likelihood (Bottom-Right: Orange)
    geom_rect(aes(xmin = med_x, xmax = max_x, ymin = min_y, ymax = med_y), fill = "#ffe8d6", alpha = 0.1) +
    # Critical Risk Bottleneck (Top-Right: Red)
    geom_rect(aes(xmin = med_x, xmax = max_x, ymin = med_y, ymax = max_y), fill = "#f8d7da", alpha = 0.1) +
    
    # Axis crosshairs dividing the matrix
    geom_hline(yintercept = med_y, linetype = "dotdash", color = "gray40", linewidth = 0.5) +
    geom_vline(xintercept = med_x, linetype = "dotdash", color = "gray40", linewidth = 0.5) +
    
    # Plot the subjects as risk events
    geom_point(aes(fill = Risk_Zone), color = "black", shape = 21, size = 3.5, stroke = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = c(
      "CRITICAL (Red Zone)"         = "#dc3545", # Red
      "HIGH IMPACT (Yellow Zone)"   = "#ffc107", # Yellow
      "HIGH LIKELIHOOD (Orange Zone)"= "#fd7e14", # Orange
      "LOW RISK (Green Zone)"       = "#28a745"  # Green
    )) +
    
    # Smart-label the high priority targets (Red and Orange zones)
    geom_text_repel(
      data = filter(df_ranked, Risk_Zone %in% c("CRITICAL (Red Zone)", "HIGH LIKELIHOOD (Orange Zone)")),
      aes(label = Cod_Asig),
      size = 3, fontface = "bold", color = "#212529",
      box.padding = 0.4, max.overlaps = 15
    ) +
    
    # Zone Labels in corners
    annotate("text", x = max_x, y = max_y, label = "🔴 CRITICAL", hjust = 1, vjust = 1, fontface = "bold", color = "#b52a37") +
    annotate("text", x = min_x, y = max_y, label = "🟡 HIGH IMPACT", hjust = 0, vjust = 1, fontface = "bold", color = "#9c7604") +
    annotate("text", x = max_x, y = min_y, label = "🟠 HIGH LIKELIHOOD", hjust = 1, vjust = 0, fontface = "bold", color = "#c45a02") +
    annotate("text", x = min_x, y = min_y, label = "🟢 LOW RISK", hjust = 0, vjust = 0, fontface = "bold", color = "#1e6b30") +
    
    labs(
      title = paste("Curriculum Risk Priority Matrix | Program:", target_program_code),
      subtitle = "Quadrants defined relative to internal program medians. Labeled subjects represent immediate triage risks.",
      x = "Likelihood Factor: Dropout Risk (Log-Odds)",
      y = "Impact Factor: Academic Failure Barrier (-Log-Odds of Passing)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = "#111111"),
      panel.grid.major = element_line(color = "#ebeebe", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
  
  # Print plot to display window
  print(risk_plot)
  
  # Format a clean console display registry
  cat("\n=======================================================\n")
  cat(" RISK REGISTRY SUMMARY FOR DEGREE:", target_program_code, "\n")
  cat("=======================================================\n")
  print(df_ranked %>% 
          select(Cod_Asig, Risk_Zone, `Dropout_Risk(X)` = Drop_Log_Odds, `Failure_Impact(Y)` = failure_impact) %>% 
          head(10))
  cat("=======================================================\n")
  
  # Return the actionable table silently so the user can pipe it or write to CSV
  return(invisible(df_ranked))
}

# 1. Run the function for your chosen degree program
degree_audit_171 <- generate_degree_risk_matrix("171")
degree_audit_240 <- generate_degree_risk_matrix("240")
degree_audit_241 <- generate_degree_risk_matrix("241")
degree_audit_247 <- generate_degree_risk_matrix("247")
# 2. Export the prioritized list straight to a CSV file
#write.csv(degree_audit, "degree_risk_priorities.csv", row.names = FALSE)

##########################################33
## Parameters of the models
#######################################

extract_degree_diagnostics <- function(target_program_code, df_nested_models = df_models) {
  
  # Filter for the target degree and explicitly UNGROUP to prevent downstream warnings
  df_target <- df_nested_models %>% 
    filter(Cod_Tit == target_program_code) %>%
    ungroup() # <- This clears the hidden grouping variables completely
  
  if (nrow(df_target) == 0) {
    stop(paste("Degree program", target_program_code, "not found."))
  }
  
  # 1. EXTRACT MODEL-LEVEL METRICS (AIC, Deviance, df)
  model_summary <- df_target %>%
    mutate(
      glance_pass = map(model_pass, function(m) {
        if (!is.null(m)) glance(m) else tibble()
      }),
      glance_drop = map(model_drop, function(m) {
        if (!is.null(m)) glance(m) else tibble()
      })
    ) %>%
    select(Cod_Tit, glance_pass, glance_drop) %>%
    pivot_longer(cols = c(glance_pass, glance_drop), names_to = "Model_Type", values_to = "stats") %>%
    unnest(stats) %>%
    mutate(Model_Type = ifelse(Model_Type == "glance_pass", "Pass Rate Model", "Dropout Model")) %>%
    select(Cod_Tit, Model_Type, null.deviance, df.null, deviance, df.residual, AIC, BIC)
  
  # 2. EXTRACT COEFFICIENT-LEVEL METRICS (Standard Errors per Subject)
  subject_errors <- df_target %>%
    select(Cod_Tit, tidy_pass, tidy_drop) %>%
    pivot_longer(cols = c(tidy_pass, tidy_drop), names_to = "Model_Type", values_to = "coefs") %>%
    unnest(coefs) %>%
    filter(str_detect(term, "Cod_Asig")) %>%
    mutate(
      Cod_Asig = str_remove(term, "Cod_Asig"),
      Model_Type = ifelse(Model_Type == "tidy_pass", "Pass Rate", "Dropout Rate")
    ) %>%
    select(Cod_Asig, Model_Type, log_odds_estimate = estimate, std_error = std.error, p_value = p.value) %>%
    arrange(Model_Type, desc(std_error))
  
  return(list(
    model_fit      = model_summary,
    subject_errors = subject_errors
  ))
}
# Run the extractor for your degree
degree_info_171 <- extract_degree_diagnostics("171")
degree_info_240 <- extract_degree_diagnostics("240")
degree_info_241 <- extract_degree_diagnostics("241")
degree_info_247 <- extract_degree_diagnostics("247")
# View the overall macro metrics (AIC, Deviance)
print(degree_info_171$model_fit)
print(degree_info_240$model_fit)
print(degree_info_241$model_fit)
print(degree_info_247$model_fit)

# View the subject-level standard errors
print(head(degree_info_171$subject_errors, n=25))

#########################################
## Compute the Model Chi-Square
##################################


compute_model_chisq <- function(target_program_code, df_nested_models = df_models) {
  
  # Filter down to the target program and ungroup cleanly
  df_target <- df_nested_models %>% 
    filter(Cod_Tit == target_program_code) %>%
    ungroup()
  
  if (nrow(df_target) == 0) {
    stop(paste("Degree program", target_program_code, "not found."))
  }
  
  # Internal function to calculate metrics for a single GLM object
  get_chisq_stats <- function(model_obj) {
    # If the model failed to compile, return a row of NAs matching the structure
    if (is.null(model_obj)) {
      return(tibble(
        Null_Deviance    = NA_real_,
        Res_Deviance     = NA_real_,
        Model_ChiSq_Raw  = NA_real_,
        Phi_Parameter    = NA_real_,
        Model_ChiSq_Adj  = NA_real_,
        DF_Difference    = NA_integer_,
        P_Value_Adjusted = NA_real_
      ))
    }
    
    # FIX: Extract directly from the GLM object properties to avoid function errors
    null_dev <- model_obj$null.deviance
    res_dev  <- model_obj$deviance
    df_n     <- model_obj$df.null
    df_r     <- model_obj$df.residual
    
    # Extract the model's unique Quasibinomial Phi parameter
    phi <- summary(model_obj)$dispersion
    
    # Calculate Chi-Square statistics
    raw_chisq <- null_dev - res_dev
    df_diff   <- df_n - df_r
    
    # Apply the quasi-scale adjustment to guard against false-positives
    scaled_chisq <- raw_chisq / phi
    p_val_scaled <- pchisq(scaled_chisq, df_diff, lower.tail = FALSE)
    
    return(tibble(
      Null_Deviance    = null_dev,
      Res_Deviance     = res_dev,
      Model_ChiSq_Raw  = raw_chisq,
      Phi_Parameter    = phi,
      Model_ChiSq_Adj  = scaled_chisq,
      DF_Difference    = df_diff,
      P_Value_Adjusted = p_val_scaled
    ))
  }
  
  # Run calculations across both models and bind the results cleanly
  results <- df_target %>%
    mutate(
      pass_stats = map(model_pass, get_chisq_stats),
      drop_stats = map(model_drop, get_chisq_stats)
    ) %>%
    select(Cod_Tit, pass_stats, drop_stats) %>%
    tidyr::pivot_longer(cols = c(pass_stats, drop_stats), names_to = "Model_Type", values_to = "data") %>%
    tidyr::unnest(data) %>%
    mutate(Model_Type = ifelse(Model_Type == "pass_stats", "Pass Rate Model", "Dropout Model"))
  
  return(results)
}
chi_sq_summary_171 <- compute_model_chisq('171')
print(chi_sq_summary_171)
chi_sq_summary_240 <- compute_model_chisq('240')
print(chi_sq_summary_240)


#####################################
### Pseudo R^2 extractor
#################################
#install.packages('PerformanceAnalytics')
#library(PerformanceAnalytics) # For general metrics if needed, but we'll compute from scratch for speed


compute_degree_pseudo_r2 <- function(target_program_code, df_nested_models = df_models) {
  
  # Filter for the target degree program
  df_target <- df_nested_models %>% 
    filter(Cod_Tit == target_program_code) %>%
    ungroup()
  
  if (nrow(df_target) == 0) {
    stop(paste("Degree program", target_program_code, "not found."))
  }
  
  # Core mathematical engine using direct vector extraction
  calculate_metrics <- function(model_obj) {
    if (is.null(model_obj)) {
      return(tibble(McFadden = NA, McFadden_Adj = NA, Cox_Snell = NA, Nagelkerke = NA, Tjur_D = NA, Hosmer_Lemeshow_P = NA))
    }
    
    # 1. Extract structural vectors directly from the compiled GLM object
    fitted_vals <- fitted(model_obj)
    totals      <- model_obj$prior.weights
    y_prop      <- model_obj$y
    
    # Reconstruct absolute student counts (independent of original column names)
    successes   <- round(y_prop * totals)
    failures    <- totals - successes
    
    # 2. Calculate true Binomial Log-Likelihoods directly using dbinom()
    lnL_fit  <- sum(dbinom(successes, size = totals, prob = fitted_vals, log = TRUE))
    
    p_null   <- sum(successes) / sum(totals)
    lnL_null <- sum(dbinom(successes, size = totals, prob = p_null, log = TRUE))
    
    N <- sum(totals)             # Total student-subject interactions
    k <- length(coef(model_obj)) # Total parameters estimated
    
    # 3. Compute Pseudo-R2 Formulations
    # McFadden
    R2_mcfadden <- 1 - (lnL_fit / lnL_null)
    
    # McFadden Adjusted (penalizes for curriculum size)
    R2_mcfadden_adj <- 1 - ((lnL_fit - k) / lnL_null)
    
    # Cox & Snell
    R2_cox_snell <- 1 - exp((2 / N) * (lnL_null - lnL_fit))
    
    # Nagelkerke (Rescaled to guarantee a maximum possible value of 1.0)
    max_cox_snell <- 1 - exp((2 / N) * lnL_null)
    R2_nagelkerke <- R2_cox_snell / max_cox_snell
    
    # Tjur's D (Gap between mean predictions of successful vs unsuccessful outcomes)
    if (sum(successes) > 0 && sum(failures) > 0) {
      mean_p_pass <- sum(fitted_vals * successes) / sum(successes)
      mean_p_fail <- sum(fitted_vals * failures) / sum(failures)
      R2_tjur     <- mean_p_pass - mean_p_fail
    } else {
      R2_tjur     <- 0
    }
    
    # Hosmer-Lemeshow Calibration Test Approximation
    hl_chisq <- sum(((y_prop - fitted_vals)^2 * totals) / (fitted_vals * (1 - fitted_vals) + 1e-6), na.rm = TRUE)
    hl_df    <- max(2, length(unique(model_obj$linear.predictors)) - k) 
    p_hl     <- pchisq(hl_chisq, df = hl_df, lower.tail = FALSE)
    
    return(tibble(
      McFadden           = round(R2_mcfadden, 4),
      McFadden_Adj       = round(R2_mcfadden_adj, 4),
      Cox_Snell          = round(R2_cox_snell, 4),
      Nagelkerke         = round(R2_nagelkerke, 4),
      Tjur_D             = round(R2_tjur, 4),
      Hosmer_Lemeshow_P  = round(p_hl, 4)
    ))
  }
  
  # Run the calculation cleanly across both performance dimensions
  r2_summary <- df_target %>%
    mutate(
      pass_r2 = map(model_pass, calculate_metrics),
      drop_r2 = map(model_drop, calculate_metrics)
    ) %>%
    select(Cod_Tit, pass_r2, drop_r2) %>%
    pivot_longer(cols = c(pass_r2, drop_r2), names_to = "Model_Type", values_to = "metrics") %>%
    unnest(metrics) %>%
    mutate(Model_Type = ifelse(Model_Type == "pass_r2", "Pass Rate Model", "Dropout Model"))
  
  return(r2_summary)
}

r2_summary_171 <- compute_degree_pseudo_r2('171')
print(r2_summary_171)
r2_summary_240 <- compute_degree_pseudo_r2('240')
print(r2_summary_240)
##########################################
### Students effect


# Step 1: Assume a crosswalk table exists
# subject_crosswalk <- tibble(
#   Cod_Asig_171 = c("1710002", "1710003"),
#   Cod_Asig_240 = c("2400001", "2400002"),
#   Unified_Name = c("Calculus_I", "Physics_I")
# )

df_crosswalk <- read_delim("Unif_asig.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)


analyze_cross_degree_student_effect <- function(df_analysis, df_crosswalk) {
  
  cat("🚀 Harmonizing datasets and building crosswalk linkages...\n")
  
  # 1. Clean and normalize identifiers using your exact column schema
  df_analysis_clean <- df_analysis %>%
    mutate(
      Cod_Asig = str_trim(as.character(Cod_Asig)),
      Cod_Tit  = str_trim(as.character(Cod_Tit)),
      Curso    = as.factor(Curso)
    )
  
  df_crosswalk_clean <- df_crosswalk %>%
    mutate(
      Cod_Asig     = str_trim(as.character(Cod_Asig)),
      Unified_name = as.factor(str_trim(as.character(Unified_name)))
    )
  
  # Target degrees for the multi-group comparison
  target_degrees <- c("171", "240", "241", "247")
  
  # 2. Filter down to targets and execute the long-form join
  df_pooled <- df_analysis_clean %>%
    filter(Cod_Tit %in% target_degrees) %>%
    inner_join(df_crosswalk_clean, by = "Cod_Asig") %>%
    mutate(
      # Force Degree 171 to be the explicit baseline reference category
      Cod_Tit = factor(Cod_Tit, levels = target_degrees),
      
      # FIX: Map directly to your cleaned column abbreviations
      # Num_No_Superan is already present in your file, so we just pass it through safely
      Num_No_Superan = Num_No_Superan, 
      
      # Calculate exam-present students for the dropout model denominator
      Num_Presentados = Num_Mat - Num_No_Pres
    )
  
  if (nrow(df_pooled) == 0) {
    stop("❌ Error: Zero rows matched between df_analysis and df_crosswalk. Verify 'Cod_Asig' types match exactly.")
  }
  
  cat(paste("📊 Matched", length(unique(df_pooled$Unified_name)), "unique shared subjects across groups.\n"))
  cat("🏋️  Running Quasibinomial matrix regressions...\n")
  
  # 3. Model Formulations using the interaction shorthand (*)
  # Pass Rate Model matrix: [Passed, Failed]
  formula_pass <- cbind(Num_Superan, Num_No_Superan) ~ Unified_name * Cod_Tit + Curso
  
  # Dropout Model matrix: [Dropped Out, Showed Up to Exam]
  formula_drop <- cbind(Num_No_Pres, Num_Presentados) ~ Unified_name * Cod_Tit + Curso
  
  model_pass <- glm(formula_pass, data = df_pooled, family = quasibinomial)
  model_drop <- glm(formula_drop, data = df_pooled, family = quasibinomial)
  
  # 4. Post-estimation extraction tool
  extract_student_effects <- function(model_obj, model_label) {
    tidy(model_obj) %>%
      # Isolate only the coefficients assessing the non-baseline degrees
      filter(str_detect(term, "Cod_Tit")) %>%
      mutate(
        Model_Type = model_label,
        Odds_Ratio = exp(estimate),
        # Generate dynamic textual interpretations based on thresholds
        Actionable_Insight = case_when(
          p.value > 0.05 ~ "No statistically significant difference vs Degree 171 profiles.",
          str_detect(term, ":") ~ "Interaction Effect: The degree's baseline advantage/disadvantage shifts for this specific subject.",
          TRUE ~ paste0("Global Effect: Under identical environments, this cohort has a ", 
                        round(abs(exp(estimate) - 1) * 100, 1), "% ", 
                        ifelse(estimate > 0, "higher", "lower"), 
                        " odds of this outcome compared to Degree 171.")
        )
      ) %>%
      select(Model_Type, Term = term, Log_Odds = estimate, Odds_Ratio, Std_Error = std.error, P_Value = p.value, Actionable_Insight)
  }
  
  # 5. Combine and deliver the final report structure
  final_report <- bind_rows(
    extract_student_effects(model_pass, "Pass Rate Model"),
    extract_student_effects(model_drop, "Dropout Model")
  )
  
  cat("✅ Statistical extraction complete.\n")
  return(final_report)
}
# Run the pipeline
student_effects_matrix <- analyze_cross_degree_student_effect(df_analysis, df_crosswalk)

# View the macro global differences between student cohorts
student_effects_matrix %>%
  filter(!str_detect(Term, ":")) %>%
  print(n = Inf)

# Extract significant interaction rows to see subject-specific anomalies
interaction_effects <- student_effects_matrix %>%
  filter(str_detect(Term, ":")) %>%
  # Filter for statistically meaningful shifts (P < 0.05)
  filter(P_Value < 0.05) %>%
  arrange(Model_Type, P_Value)

# Print the table
print(interaction_effects, n = Inf)
#############################
### Student effect by subject

compute_subject_specific_or <- function(df_analysis, df_crosswalk) {
  
  cat("🔄 Preparing head-to-head data slice for Degrees 171 and 240...\n")
  
  # 1. Standardize and filter down to the two target degrees
  df_headless <- df_analysis %>%
    mutate(
      Cod_Asig = str_trim(as.character(Cod_Asig)),
      Cod_Tit  = str_trim(as.character(Cod_Tit)),
      Curso    = as.factor(Curso)
    ) %>%
    filter(Cod_Tit %in% c("171", "240")) %>%
    inner_join(
      df_crosswalk %>% mutate(Cod_Asig = str_trim(as.character(Cod_Asig))),
      by = "Cod_Asig"
    ) %>%
    mutate(
      # Degree 171 is the baseline; coefficients will show 240's direct multiplier
      Cod_Tit = factor(Cod_Tit, levels = c("171", "240")),
      Num_No_Superan = Num_Mat - Num_Superan
    )
  
  cat("🏋️  Running stratified parallel models across all shared subjects...\n")
  
  # 2. Group by subject, safely run localized GLMs, and extract the clean metrics
  subject_or_matrix <- df_headless %>%
    group_by(Unified_name) %>%
    # Safeguard: Ensure both degrees are present in the subject history to avoid empty cells
    filter(
      length(unique(Cod_Tit)) == 2,
      sum(Num_Superan) > 0,
      sum(Num_No_Superan) > 0
    ) %>%
    nest() %>%
    mutate(
      # Use safely() to prevent a single non-converging subject from breaking the whole loop
      model_fit = map(data, ~ safely(glm)(cbind(Num_Superan, Num_No_Superan) ~ Cod_Tit + Curso, 
                                          data = .x, 
                                          family = quasibinomial)),
      # Extract only the specific effect of being a Degree 240 student
      extracted_stats = map(model_fit, ~ {
        if (is.null(.x$result)) return(NULL)
        tidy(.x$result) %>% filter(term == "Cod_Tit240")
      })
    ) %>%
    # Clean up and unnest the statistical rows
    filter(!map_lgl(extracted_stats, is.null)) %>%
    select(Unified_name, extracted_stats) %>%
    unnest(extracted_stats) %>%
    mutate(
      Odds_Ratio = exp(estimate),
      Is_Significant = ifelse(p.value < 0.05, "Yes", "No"),
      Actionable_Insight = case_when(
        p.value > 0.05 ~ "No statistical difference. Students perform identically here.",
        estimate > 0   ~ paste0("Degree 240 students have ", round((Odds_Ratio - 1) * 100, 1), "% HIGHER odds of passing."),
        estimate < 0   ~ paste0("Degree 240 advantage flips! They have ", round((1 - Odds_Ratio) * 100, 1), "% LOWER odds of passing.")
      )
    ) %>%
    select(Unified_name, Direct_Odds_Ratio = Odds_Ratio, P_Value = p.value, Is_Significant, Actionable_Insight) %>%
    arrange(desc(Direct_Odds_Ratio))
  
  cat("✅ Subject-level matrix generated successfully.\n")
  return(subject_or_matrix)
}
subject_level_report <- compute_subject_specific_or(df_analysis, df_crosswalk)
print(subject_level_report, n = Inf)
filter(subject_level_report, Is_Significant == "Yes") %>% print()
filter(subject_level_report, Is_Significant == "No") %>% print()
######