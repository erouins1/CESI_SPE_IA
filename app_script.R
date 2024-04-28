library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(splines)
library(zoo)
library(Metrics)
library(ggplot2)

# Récupération des données traitées par la pipeline stockées dans le fichier data.csv
load_full_data <- function() {
  # Charger les données traitées à partir du fichier CSV
  full_data <- read_csv("TreatedData/data.csv")
  # Retourner les données traitées
  return(full_data)
}

# Fonction de prédiction du poids fœtal en utilisant le modèle polynomial
predict_foetal_weight_poly <- function(Maternal_age, Maternal_weight, Maternal_height, Parity, Sex, Gestational_age) {
  # Charger le modèle polynomial stocké dans le fichier rds
  model <- readRDS("./Models/p_c_model.rds")
  # Transformer le résultat de la prédiction en un vecteur numérique
  predictions <- vector("numeric", length(Gestational_age))
  # Réaliser une boucle sur les âges gestationnels pour prédire le poids fœtal
  for (i in seq_along(Gestational_age)) {
    # Vérifier si les paramètres requis sont présents
    data <- data.frame(
      Maternal_age = as.numeric(Maternal_age[i]),
      Maternal_weight = as.numeric(Maternal_weight[i]),
      Maternal_height = as.numeric(Maternal_height[i]),
      Parity = as.numeric(Parity[i]),
      Sex = as.factor(Sex[i]),
      Gestational_age = Gestational_age[i]
    )
    # Prédire le poids fœtal en utilisant le modèle polynomial
    predictions[i] <- predict(model, newdata = data)
  }
  # Exporter les prédictions
  return(predictions)
}

# Fonction de prédiction du poids fœtal en utilisant le modèle linéaire
predict_foetal_weight_linear <- function(Maternal_age, Maternal_weight, Maternal_height, Parity, Sex, Gestational_age) {
  # Charger le modèle linéaire stocké dans le fichier rds
  model <- readRDS("./Models/linear_model.rds")
  # Vérifier si les âges gestationnels sont présents
  if (length(Gestational_age) == 0) {
    stop("Gestational_ages is missing or empty.")
  }

  # Transformer le résultat de la prédiction en un vecteur numérique
  predictions <- vector("numeric", length(Gestational_age))
  # Réaliser une boucle sur les âges gestationnels pour prédire le poids fœtal
  for (i in seq_along(Gestational_age)) {
    # Vérifier si les paramètres requis sont présents
    if (is.null(Maternal_age[i]) || is.null(Maternal_weight[i]) || is.null(Maternal_height[i]) || is.null(Parity[i]) || is.null(Sex[i]))  {
      stop("One of the required parameters is missing a value at index: ", i)
    }
    # Créer un dataframe pour les paramètres d'entrée
    data <- data.frame(
      Maternal_age = as.numeric(Maternal_age[i]),
      Maternal_weight = as.numeric(Maternal_weight[i]),
      Maternal_height = as.numeric(Maternal_height[i]),
      Parity = as.numeric(Parity[i]),
      Sex = as.factor(Sex[i]),
      Gestational_age = Gestational_age[i]
    )
    # Prédire le poids fœtal en utilisant le modèle linéaire
    predictions[i] <- predict(model, newdata = data)
  }
  # Exporter les prédictions
  return(predictions)
}

# Fonction pour tracer les percentiles de croissance, les valeurs réelles et la valeur prédite
plot_percentiles <- function(data, data_scatter, predicted_weight, title, color) {
  # Vérifier si la colonne "Weight_Predicted" est présente dans les données et si oui, calculer les percentiles
  if("Weight_Predicted" %in% names(data)) {
    data %>%
      # Regrouper les données par l'âge gestationnel pour calculer les statistiques de poids
      group_by(Gestational_age) %>%
      summarise(
        Weight = mean(Weight_Predicted, na.rm = TRUE),
        Percentile1 = quantile(Weight_Predicted, 0.01, na.rm = TRUE),
        Percentile3 = quantile(Weight_Predicted, 0.03, na.rm = TRUE),
        Percentile10 = quantile(Weight_Predicted, 0.1, na.rm = TRUE),
        Percentile90 = quantile(Weight_Predicted, 0.9, na.rm = TRUE),
        Percentile97 = quantile(Weight_Predicted, 0.97, na.rm = TRUE),
        Percentile99 = quantile(Weight_Predicted, 0.99, na.rm = TRUE)
      ) %>%
      # Tracer les points de données réels et prédits ainsi que les lignes de percentiles
      ggplot(aes(x = Gestational_age)) +
      geom_point(data = data_scatter, aes(y = Weight), color = "black", size = 1)+
      geom_point(data = predicted_weight, aes(y = Foetal_Weight), color = "red", size = 1)+
      # geom_line(aes(y = Weight), color = color, size = 1.2) +
      geom_line(aes(y = Percentile1), color = "pink", linetype = "dashed") +
      geom_line(aes(y = Percentile3), color = "blue", linetype = "dashed") +
      geom_line(aes(y = Percentile10), color = "green", linetype = "dashed") +
      geom_line(aes(y = Percentile90), color = "orange", linetype = "dashed") +
      geom_line(aes(y = Percentile97), color = "red", linetype = "dashed") +
      geom_line(aes(y = Percentile99), color = "purple", linetype = "dashed") +
      labs(title = title, y = "Predicted Weight", x = "Gestational Age")
  } else {
    stop("Weight_Poly_C column not found in the provided data.")
  }
}

# Fonction pour calculer le coefficient de détermination R^2
calculate_r2 <- function(actual, predicted) {
  # Calculer la somme des carrés résiduels et la somme des carrés totaux
  ss_res <- sum((actual - predicted) ^ 2)
  ss_tot <- sum((actual - mean(actual)) ^ 2)
  # Calculer le coefficient de détermination R^2
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}

# Fonction pour calculer les erreurs de prédiction MAE
count_extreme_percentiles <- function(percentile_data, count_data) {
  if (!is.numeric(percentile_data) || !is.numeric(count_data)) {
    stop("Both inputs must be numeric vectors.")
  }
  # Calculer les 10ème et 90ème percentiles
  p10 <- quantile(percentile_data, 0.1)
  p90 <- quantile(percentile_data, 0.9)
  # Compter les valeurs en dessous du 10ème et au-dessus du 90ème percentile
  count_below_p10 <- sum(count_data < p10)
  count_above_p90 <- sum(count_data > p90)
  # Calculer la quantité totale de valeurs
  total_count <- length(count_data)
  # Calculer les pourcentages de valeurs en dessous du 10ème et au-dessus du 90ème percentile
  percent_below_p10 <- (count_below_p10 / total_count) * 100
  percent_above_p90 <- (count_above_p90 / total_count) * 100
  # Retourner les pourcentages calculés
  return(list(below_10th_percentile = percent_below_p10,
              above_90th_percentile = percent_above_p90))
}

# Interface utilisateur de l'application
ui <- dashboardPage(
  # En-tête de l'application
  dashboardHeader(title = "Foetal Weight Prediction"),
  # Sidebar contenant les paramètres d'entrée pour la prédiction du poids fœtal
  dashboardSidebar(
    tags$head(
      # Style pour centrer les titres et les étiquettes
      tags$style(HTML("
        .sidebar .box-header .box-title {
          text-align: center;
          display: block;
        }
        .form-group > label {
          color: black;
          visibility: visible;
          display: block;
          text-align: center;
        }
      "))
    ),
    # Boîte contenant les paramètres d'entrée
    box(
      width = "100%",
      title = "Input Parameters",
      sliderInput("MaternalAgeSlider", "Maternal Age (years)", min = 14, max = 60, value = 30),
      sliderInput("MaternalWeightSlider", "Maternal Weight (kg)", min = 40, max = 150, value = 70),
      sliderInput("MaternalHeightSlider", "Maternal Height (cm)", min = 120, max = 230, value = 165),
      selectInput("MaternalParity", "Maternal Parity", choices = c(0, 1, 2, 3, 4, 5)),
      selectInput("ChildGender", "Child Gender", choices = c("Male", "Female")),
      sliderInput("GestationalAgeSlider", "Gestational Age (week)", min = 15, max = 45, value = 38),
    )
  ),
    # Corps de l'application contenant les graphiques et les résultats de prédiction
  dashboardBody(
    fluidRow(
      width= 12,
      box(title = "R^2 Score Polynomial Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("r2Poly")),
      box(title = "Mean Absolute Error Polynomial Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("maePoly")),
      box(title = "R^2 Score Linear Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("r2Linear")),
          box(title = "Mean Absolute Error Linear Model", status = "primary", solidHeader = TRUE, width = 3, textOutput("maeLinear")),
    ),
    fluidRow(
      width = 12,
        box(width = 6, plotlyOutput("c_p_model")),
        box(width = 6, plotlyOutput("linear_model"))
    ),
    fluidRow(
        box(width = 6, textOutput("OverExtremesPoly")),
        box(width = 6, textOutput("OverExtremesLinear"))
    ),
    fluidRow(
      width = 12,
      box(width = 6, plotlyOutput("result_compare_plot_poly")),
      box(width = 6, plotlyOutput("result_compare_plot_linear")),
    ),
    fluidRow(
      width = 12,
      box(width = 4, plotlyOutput("maternal_age_baby_weight")),
      box(width = 4, plotlyOutput("residualsPlotLinear")),
      box(width = 4, plotlyOutput("residualsPlotPoly")),
    ),
  )
)

# Serveur de l'application
server <- function(input, output, session) {
  # Fonction pour prédire le poids fœtal en utilisant le modèle polynomial avec les paramètres d'entrée de l'utilisateur
  predicted_point_poly <- reactive({
      # Vérifier si les paramètres d'entrée sont présents
      if(is.null(input$MaternalAgeSlider) || is.null(input$MaternalWeightSlider) ||
        is.null(input$MaternalHeightSlider) || is.null(input$MaternalParity) ||
        is.null(input$ChildGender) || is.null(input$GestationalAgeSlider)) {
        return(NULL)
      }
    # Prédire le poids fœtal en utilisant le modèle polynomial
      predicted_weight <- predict_foetal_weight_poly(
        Maternal_age = input$MaternalAgeSlider,
        Maternal_weight = input$MaternalWeightSlider,
        Maternal_height = input$MaternalHeightSlider,
        Parity = input$MaternalParity,
        Sex = input$ChildGender,
        Gestational_age = input$GestationalAgeSlider
      )
      # Vérifier si le poids prédit est vide
      if (length(predicted_weight) == 0) {
        return(NULL)
      }
    # Retourner les résultats de prédiction
      data.frame(
        Gestational_age = input$GestationalAgeSlider,
        Foetal_Weight = exp(predicted_weight),
        Actual_Weight = load_full_data()$Weight
      )
  })

  # Fonction pour prédire le poids fœtal en utilisant le modèle linéaire avec les paramètres d'entrée de l'utilisateur
  predicted_point_linear <- reactive({
    # Vérifier si les paramètres d'entrée sont présents
    if(is.null(input$MaternalAgeSlider) || is.null(input$MaternalWeightSlider) ||
      is.null(input$MaternalHeightSlider) || is.null(input$MaternalParity) ||
      is.null(input$ChildGender) || is.null(input$GestationalAgeSlider)) {
      return(NULL)
    }
    # Prédire le poids fœtal en utilisant le modèle linéaire
    predicted_weight <- predict_foetal_weight_linear(
      Maternal_age = input$MaternalAgeSlider,
      Maternal_weight = input$MaternalWeightSlider,
      Maternal_height = input$MaternalHeightSlider,
      Parity = input$MaternalParity,
      Sex = input$ChildGender,
      Gestational_age = input$GestationalAgeSlider
    )

    # Vérifier si le poids prédit est vide
    if (length(predicted_weight) == 0) {
      return(NULL)
    }

    # Retourner les résultats de prédiction
    data.frame(
      Gestational_age = input$GestationalAgeSlider,
      Foetal_Weight = exp(predicted_weight),
      Actual_Weight = load_full_data()$Weight
    )
  })

  # Fonction pour prédire le poids foetal pour toutes les données du jeu de données en utilisant le modèle polynomial
  predicted_points_poly <- reactive({
    # Charger les données complètes
    data <- load_full_data()
    # Boucler à travers les données et prédire le poids fœtal
    predicted_weight <- predict_foetal_weight_poly(
                                              data$Maternal_age,
                                              data$Maternal_weight,
                                              data$Maternal_height,
                                              data$Parity,
                                              data$Sex,
                                              data$Gestational_age)
    # Retourner les résultats de prédiction
    data.frame(
      Maternal_age = data$Maternal_age,
      Maternal_weight = data$Maternal_weight,
      Maternal_height = data$Maternal_height,
      Parity = data$Parity,
      Sex = data$Sex,
      Gestational_age = data$Gestational_age,
      Foetal_Weight = exp(predicted_weight),
      Actual_Weight = data$Weight,
      Residuals = (data$Weight - exp(predicted_weight)))
  })

  # Fonction pour prédire le poids foetal pour toutes les données du jeu de données en utilisant le modèle linéaire
  predicted_points_linear <- reactive({
    # Charger les données complètes
    data <- load_full_data()
    # Boucler à travers les données et prédire le poids fœtal
    predicted_weight <- predict_foetal_weight_linear(
                                              data$Maternal_age,
                                              data$Maternal_weight,
                                              data$Maternal_height,
                                              data$Parity,
                                              data$Sex,
                                              data$Gestational_age)
    # Retourner les résultats de prédiction
    data.frame(
      Maternal_age = data$Maternal_age,
      Maternal_weight = data$Maternal_weight,
      Maternal_height = data$Maternal_height,
      Parity = data$Parity,
      Sex = data$Sex,
      Gestational_age = data$Gestational_age,
      Foetal_Weight = exp(predicted_weight),
      Actual_Weight = data$Weight,
      Residuals = (data$Weight - exp(predicted_weight)))
  })

  # Affichage des résidus pour le modèle polynomial
  output$residualsPlotPoly <- renderPlotly({
    # Récupérer les données de prédiction pour le modèle polynomial
    df <- predicted_points_poly()
    # Vérifier si les données sont vides
    if (is.null(df)) {
      return(NULL)
    }
    # Créer un graphique de dispersion pour les résidus
    plot <- plot_ly(data = df, x = ~Gestational_age, y = ~Residuals,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'rgba(255, 65, 54, 0.8)')) %>%
      layout(title = "Residuals by Gestational Age for Polynomial Model",
             xaxis = list(title = "Gestational Age (weeks)"),
             yaxis = list(title = "Residuals"))
    # Retourner le graphique
    return(plot)
  })

  # Affichage des résidus pour le modèle linéaire
  output$residualsPlotLinear <- renderPlotly({
    # Récupérer les données de prédiction pour le modèle linéaire
    df <- predicted_points_linear()
    # Vérifier si les données sont vides
    if (is.null(df)) {
      return(NULL)
    }
    # Créer un graphique de dispersion pour les résidus
    plot <- plot_ly(data = df, x = ~Gestational_age, y = ~Residuals,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'rgba(255, 65, 54, 0.8)')) %>%
      layout(title = "Residuals by Gestational Age for Linear Model",
             xaxis = list(title = "Gestational Age (weeks)"),
             yaxis = list(title = "Residuals"))
    # Retourner le graphique
    return(plot)
  })

  # Création de groupes d'age gestationnel et de quartiles d'age maternel
  processed_data <- reactive({
    # Vérifier si les données sont présentes
    req(load_full_data())
    # Créer des groupes d'âge gestationnel et de quartiles d'âge maternel
    load_full_data() %>%
      mutate(Gestational_Age_Group = cut(Gestational_age, breaks = c(0, 22, 33, 36, 45), labels = c("0-22", "23-33", "34-36", "37+")),
             Maternal_Age_Quartile = ntile(Maternal_age, 4))
  })

  # Calcul de la moyenne du poids par groupe d'age gestationnel et quartile d'age maternel
  outcomes_by_group <- reactive({
    # Vérifier si les données sont présentes
    req(processed_data())
    # Calculer la moyenne du poids par groupe d'âge gestationnel et quartile d'âge maternel
    processed_data() %>%
      group_by(Gestational_Age_Group, Maternal_Age_Quartile) %>%
      summarise(Weight = mean(Weight, na.rm = TRUE), .groups = 'drop')
  })

  # Affichage du poids moyen par groupe d'age gestationnel et quartile d'age maternel
  output$maternal_age_baby_weight <- renderPlotly({
    # Vérifier si les données sont présentes
    req(outcomes_by_group())
    df <- outcomes_by_group()
    # Vérifier si les données sont vides
    if(nrow(df) == 0) {
      return(NULL)
    }
    # Convertir les variables en caractères pour éviter les erreurs de type
    df$Gestational_Age_Group <- as.character(df$Gestational_Age_Group)
    df$Maternal_Age_Quartile <- as.character(df$Maternal_Age_Quartile)
    # Créer un graphique à barres pour afficher le poids moyen par groupe d'âge gestationnel et quartile d'âge maternel
    plot <- plot_ly(data = df, x = ~Gestational_Age_Group, y = ~Weight, type = 'bar', color = ~Maternal_Age_Quartile) %>%
      layout(title = "Mean Weight by Gestational Age Group and Maternal Age Quartile",
             xaxis = list(title = "Gestational Age Group"),
             yaxis = list(title = "Mean Weight"),
             barmode = 'group')

    # Retourner le graphique
    return(plot)
  })

  # Charger les modèles de prédiction stockés dans les fichiers rds
  model_p_c <- readRDS("./Models/p_c_model.rds")
  model_linear <- readRDS("./Models/linear_model.rds")

  # Affichage des graphiques de prédiction de poids fœtal pour le modèle polynomial
  output$c_p_model <- renderPlotly({
    # Récupérer les données de prédiction pour le modèle polynomial
    predicted_df <- predicted_point_poly()
    # Charger les données complètes
    all_data <- load_full_data()
    # Stocker les données dans une variable
    data <- all_data
    # Créer un vecteur de l'âge gestationnel
    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)
    # Créer un cadre de données pour les prédictions
    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex),
      Group = unique(all_data$Group))
    # Prédire le poids fœtal en utilisant le modèle polynomial
    all_data$Weight_Predicted <- exp(predict(model_p_c, newdata = all_data))
    # Créer un graphique des percentiles de croissance, des valeurs réelles et des valeurs prédites
    p <- plot_percentiles(all_data, data, predicted_df, "Polynomial Model: Growth Percentiles", "black")
    # Retourner le graphique
    p
  })

  # Affichage des graphiques de prédiction de poids fœtal pour le modèle linéaire
  output$linear_model <- renderPlotly({
    # Récupérer les données de prédiction pour le modèle linéaire
    predicted_df <- predicted_point_linear()
    # Charger les données complètes
    all_data <- load_full_data()
    # Stocker les données dans une variable
    data <- all_data
    # Créer un vecteur de l'âge gestationnel
    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)
    # Créer un cadre de données pour les prédictions
    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.9, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex))
    # Prédire le poids fœtal en utilisant le modèle linéaire
    all_data$Weight_Predicted <- exp(predict(model_linear, newdata = all_data))
    # Créer un graphique des percentiles de croissance, des valeurs réelles et des valeurs prédites
    p <- plot_percentiles(all_data, data, predicted_df, "Linear Model: Growth Percentiles", "black")
    # Retourner le graphique
    p
  })

  # Affichage du graphique de comparaison entre les poids prédits et réels pour le modèle polynomial
  output$result_compare_plot_poly <- renderPlotly({
    # Récupérer les données de prédiction pour le modèle polynomial
    predicted_points <- predicted_points_poly()
    # Créer un graphique de dispersion pour comparer les poids prédits et réels
    p <- ggplot(predicted_points, aes(x = Actual_Weight, y = Foetal_Weight)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      ggtitle("Predicted vs Actual Weights using polynomial model") +
      xlab("Actual Weight") +
      ylab("Predicted Weight")
    # Retourner le graphique
    p
  })

  # Affichage des graphiques de comparaison des poids prédits et réels pour le modèle linéaire
  output$result_compare_plot_linear <- renderPlotly({
    # Récupérer les données de prédiction pour le modèle linéaire
    predicted_points <- predicted_points_linear()
    # Créer un graphique de dispersion pour comparer les poids prédits et réels
    p <- ggplot(predicted_points, aes(x = Actual_Weight, y = Foetal_Weight)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      ggtitle("Predicted vs Actual Weights using linear model") +
      xlab("Actual Weight") +
      ylab("Predicted Weight")
    # Retourner le graphique
    p
  })


  # Calcul du coefficient de détermination R^2 pour le modèle polynomial
  r2_poly <- reactive({
    # Récupérer les données de prédiction pour le modèle polynomial
    predicted_points <- predicted_points_poly()
    # Vérifier si les données sont vides
    if (is.null(predicted_points)) {
      return(NA)
    }
    # Calculer le coefficient de détermination R^2 pour le modèle polynomial en utilisant les poids prédits et réels
    actual <- predicted_points$Actual_Weight
    predicted <- predicted_points$Foetal_Weight
    calculate_r2(actual, predicted)
  })

  # Calcul du coefficient de détermination R^2 pour le modèle linéaire
  r2_linear <- reactive({
    # Récupérer les données de prédiction pour le modèle linéaire
    predicted_points <- predicted_points_linear()
    # Vérifier si les données sont vides
    if (is.null(predicted_points)) {
      return(NA)
    }
    # Calculer le coefficient de détermination R^2 pour le modèle linéaire en utilisant les poids prédits et réels
    actual <- predicted_points$Actual_Weight
    predicted <- predicted_points$Foetal_Weight
    calculate_r2(actual, predicted)
  })

  # Affichage du coefficient de détermination R^2 pour le modèle polynomial
  output$r2Poly <- renderText({
    # Récupérer le coefficient de détermination R^2 pour le modèle polynomial
    r2_score <- r2_poly()
    # Vérifier si le coefficient de détermination est vide
    if (is.na(r2_score)) {
      "R^2 Score: N/A"
    } else {
      # Afficher le coefficient de détermination R^2 pour le modèle polynomial
      paste("R^2 Score for Polynomial Model:", format(r2_score, digits = 4))
    }
  })

  # Affichage du coefficient de détermination R^2 pour le modèle linéaire
  output$r2Linear <- renderText({
    # Récupérer le coefficient de détermination R^2 pour le modèle linéaire
    r2_score <- r2_linear()
    # Vérifier si le coefficient de détermination est vide
    if (is.na(r2_score)) {
      "R^2 Score: N/A"
    } else {
      # Afficher le coefficient de détermination R^2 pour le modèle linéaire
      paste("R^2 Score for Linear Model:", format(r2_score, digits = 4))
    }
  })

  # Calcul de l'erreur de prédiction MAE pour le modèle polynomial
  output$maePoly <- renderText({
    # Récupérer les données de prédiction pour le modèle polynomial
    predicted_points <- predicted_points_poly()
    # Vérifier si les données sont vides
    if (is.null(predicted_points)) {
      return(NA)
    }
    # Stocker les poids prédits et réels
    actual <- predicted_points$Actual_Weight
    predicted <- predicted_points$Foetal_Weight
    # Afficher l'erreur de prédiction MAE pour le modèle polynomial
    paste("MAE Score for Polynomial Model:", format(mae(actual, predicted), digits = 4))
  })

  # Calcul de l'erreur de prédiction MAE pour le modèle linéaire
  output$maeLinear <- renderText({
    # Récupérer les données de prédiction pour le modèle linéaire
    predicted_points <- predicted_points_linear()
    # Vérifier si les données sont vides
    if (is.null(predicted_points)) {
    return(NA)
    }
    # Stocker les poids prédits et réels
    actual <- predicted_points$Actual_Weight
    predicted <- predicted_points$Foetal_Weight
    # Afficher l'erreur de prédiction MAE pour le modèle linéaire
    paste("MAE Score for Linear Model:", format(mae(actual, predicted), digits = 4))
  })

  # Calcul des pourcentages de bébés en dessous du 10ème et au-dessus du 90ème percentile pour le modèle polynomial
  output$OverExtremesPoly <- renderText({
    # Charger les données complètes
    all_data <- load_full_data()
    # Créer un vecteur de l'âge gestationnel
    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)
    # Créer un cadre de données pour les prédictions
    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex))
    # Prédire le poids fœtal en utilisant le modèle polynomial
    all_data$Weight_Predicted <- exp(predict(model_p_c, newdata = all_data))
    # Calculer les pourcentages de bébés en dessous du 10ème et au-dessus du 90ème percentile
    count_extremes <- count_extreme_percentiles(all_data$Weight_Predicted, all_data$Weight)
    # Afficher les pourcentages calculés
    paste("Percentage of babies below 10th percentile: ", count_extremes$below_10th_percentile, "\n",
          "Percentage of babies above 90th percentile: ", count_extremes$above_90th_percentile)
  })

  # Calcul des pourcentages de bébés en dessous du 10ème et au-dessus du 90ème percentile pour le modèle linéaire
  output$OverExtremesLinear <- renderText({
    # Charger les données complètes
    all_data <- load_full_data()
    # Créer un vecteur de l'âge gestationnel
    gestational_age_range <- range(all_data$Gestational_age, na.rm = TRUE)
    # Créer un cadre de données pour les prédictions
    all_data <- expand.grid(
      Gestational_age = seq(gestational_age_range[1], gestational_age_range[2], by = 0.5),
      Maternal_age = quantile(all_data$Maternal_age, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_weight = quantile(all_data$Maternal_weight, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Maternal_height = quantile(all_data$Maternal_height, probs = c(0.01, 0.03, 0.1, 0.97, 0.99), na.rm = TRUE),
      Parity = unique(all_data$Parity),
      Sex = unique(all_data$Sex))
    # Prédire le poids fœtal en utilisant le modèle linéaire
    all_data$Weight_Predicted <- exp(predict(model_linear, newdata = all_data))
    # Calculer les pourcentages de bébés en dessous du 10ème et au-dessus du 90ème percentile
    count_extremes <- count_extreme_percentiles(all_data$Weight_Predicted, all_data$Weight)
    # Afficher les pourcentages calculés
    paste("Percentage of babies below 10th percentile: ", count_extremes$below_10th_percentile, "\n",
          "Percentage of babies above 90th percentile: ", count_extremes$above_90th_percentile)
  })
}

# Lancer l'application shiny
shinyApp(ui = ui, server = server)
