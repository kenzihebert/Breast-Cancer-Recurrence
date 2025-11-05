library(shiny)
library(Matrix)
library(xgboost)
library(dplyr)
library(waffle)
library(ggplot2)
library(extrafont)
library(showtext)

#extrafont::font_import (pattern = "fa-", prompt =  FALSE)
#loadfonts(device = "all")

#extrafont::fonttable() %>% 
  #dplyr::as_tibble() %>% 
  #dplyr::filter(grepl("Awesome", FamilyName)) %>% 
  #select(FamilyName, FontName, fontfile)


#font_add(family = "FontAwesome5Free-Solid", regular = "fa-solid-900.ttf")
#font_add(family = "FontAwesome5Free-Regular", regular = "fa-regular-400.ttf")
#font_add(family = "FontAwesome5Brands-Regular", regular = "fa-brands-400.ttf")
#showtext_auto()


# Load the trained XGBoost model (save your best model as 'best_xgb_model.rds' after training)
xgb_model <- readRDS("best_xgb_model.rds")

# Load the column names used in training
xgb_colnames <- readRDS("xgb_colnames.rds")

# Define the predictors used in the model (update this list to match your model's predictors)
predictor_names <- c("Definitive.Surgery.Type", "Neoadjuvant.Chemotherapy", "Adjuvant.Chemotherapy", 
"Neoadjuvant.Radiation.Therapy", "Adjuvant.Radiation.Therapy", "Neoadjuvant.Endocrine.Therapy.Medications", 
"Adjuvant.Endocrine.Therapy.Medications", "Neoadjuvant.Anti.Her2.Neu.Therapy", "Adjuvant.Anti.Her2.Neu.Therapy", 
"Mol.Subtype", "TumorGradeT", "TumorGradeN", "TumorGradeM")

# Get min/max for numeric predictors (from data)
clinData <- read.csv("clinData_subset_cleaned.csv")

# Remove unwanted columns
clinData <- clinData %>%
  select(-recurrence_1yr, -Recurrence.event.s.,-ER, -PR, -HER2, -recurrence_2yr, -recurrence_3yr, -X)

# Define predictors and their types
predictor_info <- list(
  Definitive.Surgery.Type = list(type = "cat", choices = c("BCS", "mastectomy")),
  Neoadjuvant.Chemotherapy = list(type = "bin"),
  Adjuvant.Chemotherapy = list(type = "bin"),
  Neoadjuvant.Radiation.Therapy = list(type = "bin"),
  Adjuvant.Radiation.Therapy = list(type = "bin"),
  Neoadjuvant.Endocrine.Therapy.Medications = list(type = "bin"),
  Adjuvant.Endocrine.Therapy.Medications = list(type = "bin"),
  Neoadjuvant.Anti.Her2.Neu.Therapy = list(type = "bin"),
  Adjuvant.Anti.Her2.Neu.Therapy = list(type = "bin"),
  Mol.Subtype = list(type = "num"),
  TumorGradeT = list(type = "num"),
  TumorGradeN = list(type = "num"),
  TumorGradeM = list(type = "num")
)

# Get min/max for numeric predictors
num_limits <- lapply(names(predictor_info), function(var) {
  if (predictor_info[[var]]$type == "num") {
    rng <- range(clinData[[var]], na.rm = TRUE)
    list(min = rng[1], max = rng[2])
  } else {
    NULL
  }
})
names(num_limits) <- names(predictor_info)

# Define patient-friendly questions for each predictor
predictor_questions <- list(
  Definitive.Surgery.Type = "What type of surgery did you have?",
  Neoadjuvant.Chemotherapy = "Chemotherapy",
  Adjuvant.Chemotherapy = "Chemotherapy",
  Neoadjuvant.Radiation.Therapy = "Radiation Therapy",
  Adjuvant.Radiation.Therapy = "Radiation Therapy",
  Neoadjuvant.Endocrine.Therapy.Medications = "Endocrine Therapy Medications",
  Adjuvant.Endocrine.Therapy.Medications = "Endocrine Therapy Medications",
  Neoadjuvant.Anti.Her2.Neu.Therapy = "Anti-HER2/Neu Therapy",
  Adjuvant.Anti.Her2.Neu.Therapy = "Anti-HER2/Neu Therapy",
  Mol.Subtype = "What is your molecular subtype?",
  TumorGradeT = "What is your tumor T grade?",
  TumorGradeN = "What is your tumor N grade?",
  TumorGradeM = "What is your tumor M grade?"
)

# UI
# Surgery type: BCS/mastectomy (as in data, "0" for BCS, "1" for mastectomy)
iu <- fluidPage(
  titlePanel("Breast Cancer Recurrence Risk Predictor"),
  br(),
  p("Our prognostic model is designed for clinicians assisting patients diagnosed with breast cancer. It's intended for use after patients have completed neoadjuvant therapy and surgery. This tool helps predict the likelihood of cancer recurrence within three years, providing valuable insight for selecting appropriate adjuvant therapy options.", style = "font-size:16px;"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter your information:"),
      strong("Patient information (not used in prediction):"),
      numericInput("patient_age", "Age:", value = 50, min = 0, max = 120, step = 1),
      br(),
      strong("Surgery type:"),
      radioButtons(
        "surgery_type",
        NULL,
        choices = list(
          "Breast-Conserving Surgery (BCS)" = "0",
          "Mastectomy" = "1"
        )
      ),
      br(),
      strong("Select all treatments before surgery:"),
      radioButtons("Neoadjuvant.Chemotherapy", predictor_questions$Neoadjuvant.Chemotherapy, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      radioButtons("Neoadjuvant.Radiation.Therapy", predictor_questions$Neoadjuvant.Radiation.Therapy, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      radioButtons("Neoadjuvant.Endocrine.Therapy.Medications", predictor_questions$Neoadjuvant.Endocrine.Therapy.Medications, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      radioButtons("Neoadjuvant.Anti.Her2.Neu.Therapy", predictor_questions$Neoadjuvant.Anti.Her2.Neu.Therapy, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      br(),
      strong("Select all treatments after surgery:"),
      radioButtons("Adjuvant.Chemotherapy", predictor_questions$Adjuvant.Chemotherapy, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      radioButtons("Adjuvant.Radiation.Therapy", predictor_questions$Adjuvant.Radiation.Therapy, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      radioButtons("Adjuvant.Endocrine.Therapy.Medications", predictor_questions$Adjuvant.Endocrine.Therapy.Medications, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      radioButtons("Adjuvant.Anti.Her2.Neu.Therapy", predictor_questions$Adjuvant.Anti.Her2.Neu.Therapy, choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      br(),
      strong("Tumor and molecular information:"),
      radioButtons(
        "mol_subtype",
        NULL,
        choices = list(
          "Luminal-Like" = 0,
          "ER/PR Positive, HER2 Positive" = 1,
          "HER2 Positive" = 2,
          "Triple Negative" = 3
        )
      ),
      radioButtons("TumorGradeT", predictor_questions$TumorGradeT, choices = as.list(seq(num_limits$TumorGradeT$min, num_limits$TumorGradeT$max)), inline = TRUE),
      radioButtons("TumorGradeN", predictor_questions$TumorGradeN, choices = as.list(seq(num_limits$TumorGradeN$min, num_limits$TumorGradeN$max)), inline = TRUE),
      radioButtons("TumorGradeM", predictor_questions$TumorGradeM, choices = as.list(seq(num_limits$TumorGradeM$min, num_limits$TumorGradeM$max)), inline = TRUE),
      br(),
      actionButton("predict_btn", "Get My Recurrence Probability", class = "btn-primary")
    ),
    mainPanel(
      h3("Your Predicted Probability of Recurrence (3 years):"),
      verbatimTextOutput("prob_output"),
      br(),
      uiOutput("risk_message"),
      br(),
      br(), # Add extra space before the pictogram
      plotOutput("waffle_plot", height = "400px"), # Increase height for more space
      br(), br() # Add extra space after the pictogram
    )
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$predict_btn, {
    # Build a one-row data frame with all integer columns, matching training
    df_input <- data.frame(
      Definitive.Surgery.Type = as.integer(input$surgery_type),
      Neoadjuvant.Chemotherapy = as.integer(input$Neoadjuvant.Chemotherapy),
      Neoadjuvant.Radiation.Therapy = as.integer(input$Neoadjuvant.Radiation.Therapy),
      Neoadjuvant.Endocrine.Therapy.Medications = as.integer(input$Neoadjuvant.Endocrine.Therapy.Medications),
      Neoadjuvant.Anti.Her2.Neu.Therapy = as.integer(input$Neoadjuvant.Anti.Her2.Neu.Therapy),
      Adjuvant.Chemotherapy = as.integer(input$Adjuvant.Chemotherapy),
      Adjuvant.Radiation.Therapy = as.integer(input$Adjuvant.Radiation.Therapy),
      Adjuvant.Endocrine.Therapy.Medications = as.integer(input$Adjuvant.Endocrine.Therapy.Medications),
      Adjuvant.Anti.Her2.Neu.Therapy = as.integer(input$Adjuvant.Anti.Her2.Neu.Therapy),
      Mol.Subtype = as.integer(input$mol_subtype),
      TumorGradeT = as.integer(input$TumorGradeT),
      TumorGradeN = as.integer(input$TumorGradeN),
      TumorGradeM = as.integer(input$TumorGradeM),
      stringsAsFactors = FALSE
    )
    # Use model.matrix to match training encoding
    mm <- model.matrix(~ . - 1, data = df_input)
    # Align columns to match training
    mm_aligned <- matrix(0, nrow = 1, ncol = length(xgb_colnames))
    colnames(mm_aligned) <- xgb_colnames
    common_cols <- intersect(colnames(mm), xgb_colnames)
    mm_aligned[1, common_cols] <- mm[1, common_cols]
    dnew <- xgb.DMatrix(data = mm_aligned)
    prob <- predict(xgb_model, dnew)
    output$prob_output <- renderText(sprintf("%.1f%%", prob * 100))
    output$risk_message <- renderUI({
      n_risk <- round(prob * 100)
      HTML(sprintf("<span style='color:#FFC0CB'><b>Out of 100 people like you, %d may have a recurrence</b></span>", n_risk))
    })
    # Waffle pictogram
    output$waffle_plot <- renderPlot({
      n_risk <- round(prob * 100)
      n_safe <- 100 - n_risk
      colors <- if (prob < 0.2) {
        c("#E0E0E0", "#FFC5D0") # gray, new light pink
      } else if (prob < 0.5) {
        c("#E0E0E0", "#FB4570") # gray, dark pink
      } else {
        c("#E0E0E0", "#BA0F30") # gray, red
      }
      waffle::waffle(
        c("No Recurrence" = n_safe, "Recurrence" = n_risk),
        rows = 10,
        colors = colors,
        use_glyph = "female", # girl emoji
        glyph_size = 10
      )
    })
  })
}

shinyApp(ui = iu, server = server)
