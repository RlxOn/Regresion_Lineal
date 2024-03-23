#Regresion lineal

#Bibliotecas ===================================================================
pacman::p_load(ggplot2, tidyverse, lmtest, gridExtra, nortest, broom)
set.seed(1989) #Taylor's Version

#Funcion regresion lineal ======================================================
regresion_lineal <- function(formula, data, prop = 0.66, p_value = 0.05){
  #Inicializamos variables
  prop <- prop
  p_value <- p_value
  data <- data
  
  #Construimos el conjunto de entrenamiento y el de prueba
  train_test <- function(data, prop){
    #Verificamos que p este entre 0 y 1.
    if(prop <= 0 | prop >= 1){
      stop("La proporcion debe estar entre 0 y 1")
    }
    
    #Generamos los indices para el conjunto de prueba y entrenamiento
    indices <- 1:dim(data)[1]
    ind_train <- sample(indices, ceiling(prop * length(indices)))
    ind_test <- setdiff(indices, ind_train)
    
    #Guardamos los df de prueba y de entrenamiento
    train_set <- data[ind_train,]
    test_set <- data[ind_test,]
    
    #Guardamos en lista para regresarla como argumento
    list_set <- list(train_set, test_set)
    names(list_set) <- c("train_set", "test_set")
    
    return(list_set)
    
  }
  
  tt_sets <- train_test(data, prop)
  train_set <- tt_sets$train_set
  test_set <- tt_sets$test_set
  
  #Creamos la regresion 
  lmodel <- lm(formula, data = train_set)
  
  #Obtenemos los intervalos de confianza
  conf_int <- confint(lmodel, level = 1 - p_value)
  
  #Creamos la tabla de coeficientes
  coef_tab <- broom::tidy(lmodel) %>%
    mutate(Lower = conf_int[,1],
           Upper = conf_int[,2],
           is_signf = ifelse(p.value < p_value, TRUE, FALSE))
  
  print(coef_tab)
  
  #Creamos la tabla de metricas de la regresion
  measure_tab <- broom::glance(lmodel)
  
  print(measure_tab)
  
  #Supuestos de los residuales
  residuales <- residuals(lmodel)
  ajustados <- fitted(lmodel)
  observados <- lmodel$model[,1]
  x = 1:length(residuales)
  
  df_rf <- data.frame(x, residuales, ajustados, observados)
  
  #Pruebas
  ad <- ad.test(residuales)$p.value
  cvm <- cvm.test(residuales)$p.value
  lillie <- lillie.test(residuales)$p.value
  bp <- bptest(lmodel)$p.value
  dw <- dwtest(lmodel)$p.value
  
  tests_tab <- data.frame(
    Tests = c("Anderson-Darling", "Cramer-von Mises", "Lilliefors", "Breusch-Pagan",
              "Durbin-Watson"),
    p_value = c(ad, cvm, lillie, bp, dw)
  ) %>% as_tibble()
  
  print(tests_tab)
  
  #QQplot normalidad
  qq_plot <- ggplot(df_rf, aes(sample = residuales)) +
    stat_qq(size = 3) +
    geom_abline(intercept = mean(residuales), slope = sd(residuales), color = "#6c0000", linewidth = 1.5, alpha = 0.6) +
    labs(title = "Normal Q-Q Plot Residuales",
         x = "Cuantiles teoricos",
         y = "Cuantiles observados") + theme_minimal()
  
  #Densidad residuales
  dens_plot <- ggplot(data = NULL, aes(x = residuales)) + geom_density(fill = "#000059", col = "#000059") + 
    theme_minimal() + labs(title = "Densidad Residuales", x = "", y = "")
  
  #Ajustados vs predichos
  pf_plot <- ggplot(data = NULL, aes(x = residuales, y = ajustados)) + geom_point(size = 3) + geom_smooth() + 
    labs(title = "Ajustados vs Residuales", x = "Ajustados", y = "Residuales") + theme_minimal()
  
  #Observados y ajustados
  of_plot <- ggplot() + geom_line(data = df_rf, aes(x = x, y = ajustados, col = "Ajustados"), linewidth = 1.5, alpha = 0.6) + 
    geom_line(data = df_rf, aes(x = x, y = observados, col = "Observados"), linewidth = 1.5, alpha = 0.6) + 
    labs(title = "Ajustados y Observados", x = "", y = "", color = "") +
    scale_color_manual(values = c("#810035", "#314366"),
                       labels = c("Ajustados", "Observados")) +
    theme_minimal()
  
  gridExtra::grid.arrange(qq_plot, dens_plot, pf_plot, of_plot, ncol = 2)
  
  #Hacemos el predict
  pred_tab <- predict.lm(lmodel, newdata = test_set, se.fit = TRUE, interval = "prediction",
                         level = p_value)
  
  #Finalmente, devolvemos todo en una lista
  lm_list <- list(train_set, test_set, coef_tab, measure_tab, df_rf[,2:4], tests_tab, pred_tab)
  names(lm_list) <- c("train_set", "test_set", "coefficients", "performance", "values", "tests" , "predict")
  
  return(lm_list)
}
