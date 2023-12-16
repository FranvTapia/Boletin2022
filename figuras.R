#### SIMULACION BOLETIN BIOESTADISTICA ####


# FUNCION PARA CALCULAR EL TAMAÑO DE MUESTRA
p.size <- function(p,a,e){
  z <- qnorm(1 - (a / 2))
  n <- (z^2 * p * (1 - p)) / e^2
  return(ceiling(n))
}

p.size(0.5,0.05,0.05) # 385
# MAXIMA VARIANZA
# 5% DE ERROR ABSOLUTO
# 95% DE CONFIANZA

# FUNCION PARA CALCULAR EL IC

ci.prop <- function(sample, a) {
  p <- mean(sample)
  se <- sqrt(p * (1 - p) / length(sample))
  margen_error <- qnorm(1 - a/ 2) * se
  intervalo_confianza <- round(c(p - margen_error, p + margen_error), 3) * 100
}
# FUNCION PARA CHECKEAR SI LA DISTANCIA ENTRE P HAT Y LOS LIMITES ES MAYOR AL ERROR
check_distance <- function(x, p_hat, error) {
  abs(x - p_hat) > error*100
}

# ESCENARIO 1 P A PRIORI = P
# POBLACION CONCUERDA CON EL P DEFINIDO EN EL CALCULO DE TAMAÑO MUESTRAL
set.seed(123)
error1 <- 0.5
poblacion <- rbinom(100000, 1,0.5)
muestra1 <- sample(poblacion, 385)
mean(poblacion) # P = 49.7% 
p_hat = round(mean(muestra1),3)*100 # p-hat = 55.8%
e1_5 <- ci.prop(muestra1, 0.05)
e1_5[1]
escenario1 <- data.frame(x = "P en TM = P",
                         p_hat = p_hat,
                         low = e1_5[1],
                         up = e1_5[2])
check_distance(e1_5[1], p_hat, error1)
# Function to check if the distance is greater than error1

# Number of simulations
num_simulations <- 1000

# Initialize a counter to track the number of times the condition is met
count_condition_met <- 0

# Run the simulation
for (i in 1:num_simulations) {
  muestra_sim <- sample(poblacion, 385)
  p_hat_sim <- mean(muestra_sim)
  e1_5_sim <- ci.prop(muestra_sim, 0.05)
  
  if (check_distance(e1_5_sim[1], p_hat_sim, error1) | check_distance(e1_5_sim[2], p_hat_sim, error1)) {
    count_condition_met <- count_condition_met + 1
  }
}

# Calculate the proportion of times the condition is met
proportion_condition_met <- count_condition_met / num_simulations

proportion_condition_met # 95.6%

# ESCENARIO 2: P A PRIORI < P
tm2 <- p.size(0.2, 0.05, 0.05)
set.seed(123)
poblacion <- rbinom(100000, 1,0.5)
muestra2 <- sample(poblacion, tm2)
mean(poblacion) # P = 49.7% 
p_hat2 <- round(mean(muestra2),3)*100 # p-hat = 47.6%
e2_5 <- ci.prop(muestra2, 0.05)
e1_5[1]
escenario1 <- data.frame(x = "P en TM = P",
                         p_hat = p_hat2,
                         low = e1_5[1],
                         up = e1_5[2])
check_distance(e2_5[1], p_hat2, error1)  


# Number of simulations
num_simulations <- 1000

# Initialize a counter to track the number of times the condition is met
count_condition_met2 <- 0

# Run the simulation
for (i in 1:num_simulations) {
  muestra_sim2 <- sample(poblacion, tm2)
  p_hat_sim2 <- mean(muestra_sim2)
  e2_5_sim <- ci.prop(muestra_sim2, 0.05)
  
  if (check_distance(e2_5_sim[1], p_hat_sim2, error1) | check_distance(e2_5_sim[2], p_hat_sim2, error1)) {
    count_condition_met2 <- count_condition_met2 + 1
  }
}

# Calculate the proportion of times the condition is met
proportion_condition_met2 <- count_condition_met2 / num_simulations

proportion_condition_met2 # 96.2%

################################# 
# ESCENARIO 3: P A PRIORI > 0.5 #
#################################

tm3 <- p.size(0.7, 0.05, 0.05)
set.seed(123)
muestra3 <- sample(poblacion, tm3)
mean(poblacion) # P = 49.7% 
p_hat3 <- round(mean(muestra3),3)*100 # p-hat = 52%
e3_5 <- ci.prop(muestra3, 0.05)

escenario3 <- data.frame(x = "P en TM > P",
                         p_hat = p_hat3,
                         low = e3_5[1],
                         up = e3_5[2])
check_distance(e3_5[1], p_hat3, error1)  


# Number of simulations
num_simulations <- 1000

# Initialize a counter to track the number of times the condition is met
count_condition_met3 <- 0

# Run the simulation
for (i in 1:num_simulations) {
  muestra_sim3 <- sample(poblacion, tm3)
  p_hat_sim3 <- mean(muestra_sim3)
  e3_5_sim <- ci.prop(muestra_sim3, 0.05)
  
  if (check_distance(e3_5_sim[1], p_hat_sim3, error1) | check_distance(e3_5_sim[2], p_hat_sim3, error1)) {
    count_condition_met3 <- count_condition_met3 + 1
  }
}

# Calculate the proportion of times the condition is met
proportion_condition_met3 <- count_condition_met3 / num_simulations

proportion_condition_met3 # 95.1%

###########################################

set.seed(145)
poblacion2 <- rbinom(100000, 1, 0.05)
mean(poblacion2) #P = 4.9%
tm4 <- p.size(0.5,0.05,0.05)
muestra4 <- sample(poblacion2, tm4)
p_hat4 <- round(mean(muestra4),3)*100 # 5.5%
e4_5 <- ci.prop(muestra4, 0.05)
  ggplot(IC, aes(p, p)) +  # Especificamos una variable constante para el eje y (1)
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = up)) +
  ggtitle("Estimación Puntual y Intervalo de Confianza") +
  xlab("Estimación Puntual (p)") +
  ylab("Intervalo de Confianza") +
  theme_minimal()


  boxLabels=c("Admission 1","Admission 2","Admission 3")
  df_res<-data.frame( x = "",
                     hr=c(tab_int_ci_res[,1],tab_int_ci_res[,3],tab_int_ci_res[,5]),
                     low = c(0.85,0.91,0.92),
                     up = c(0.98,1.15,1.28))
  
  ggplot2::ggplot(df_res, aes(x = boxLabels, y = hr)) +
    geom_hline(yintercept = 1, alpha=0.8, linetype = 2) +
    geom_point(size = 2, color = "gray30")+
    geom_errorbar(aes(ymin=low, ymax=up), width=.2,alpha=.65,
                  position=position_dodge(.9)) +
    ylab("Hazard Ratio") +
    xlab("") +
    theme_sjplot() +
    scale_y_continuous(breaks=c(0.8,0.95,1,1.3), limits = c(0.8,1.3))+
    theme(legend.position = "bottom")+
    theme(
      axis.title.y = element_text(size=13),
      strip.text.x = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank())
  
  
  
  # Generar una secuencia de errores absolutos desde 0.01 hasta 0.2
  library(ggplot2)
  errores <- seq(0.01, 0.1, by=0.01)
  tamanos <- sapply(errores, function(e) p.size(0.5, 0.05, e))
  
  data <- data.frame(ErrorAbsoluto = errores, TamañoMuestra = tamanos)
  library(ggplot2)
  
  # Establecer la paleta de colores
  color_linea <- "#2C3E50" # Un azul oscuro
  color_punto <- "#E74C3C" # Un rojo claro
  
  # Crear el gráfico
  ggplot(data, aes(x = ErrorAbsoluto, y = TamañoMuestra)) +
    geom_line(color = color_linea, size = 1.2) + 
    geom_point(color = color_punto, size = 3) + 
    geom_text(aes(label = TamañoMuestra), vjust = -1, size = 3.5) + # Añadir etiquetas
    labs(subtitle = "P a priori de 0.5 y un nivel de confianza del 95%",
         x = "Error Absoluto",
         y = "Tamaño de Muestra")+
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5),
      axis.title.x = element_text(size = 15, face = "bold"),
      axis.title.y = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 1)
    ) +
    scale_x_continuous(breaks = errores, labels = round(errores, 2))
  

  
##Grafica final
  # Funciones proporcionadas
  p.size <- function(p, a, e) {
    z <- qnorm(1 - (a / 2))
    n <- (z^2 * p * (1 - p)) / e^2
    return(ceiling(n))
  }
  
  ci.prop <- function(p, n, a) {
    se <- sqrt(p * (1 - p) / n)
    margen_error <- qnorm(1 - a/ 2) * se
    intervalo_confianza <- round(c(p - margen_error, p + margen_error), 3)
    return(intervalo_confianza)
  }
  
  # Definir Estimación Puntual de P
  Estimacion_Puntual_P <- 0.5
  
  # Diferentes valores de P a priori
  P_a_priori <- seq(0.1, 0.9, 0.1)
  
  # Calcular el tamaño de muestra y el intervalo de confianza para cada P a priori
  results <- sapply(P_a_priori, function(p) {
    n <- p.size(p, 0.05, 0.05)
    ic <- ci.prop(Estimacion_Puntual_P, n, 0.05)
    return(c(n, ic))
  })
  
  # Crear dataframe
  df <- data.frame(P_a_priori = P_a_priori, 
                   n = results[1,], 
                   ic_inferior = results[2,], 
                   ic_superior = results[3,])
  df$ancho_ic <- df$ic_superior - df$ic_inferior
  
  # Calcular el doble del error absoluto
  doble_error_absoluto <- 2 * 0.05  # El error absoluto tiene un ancho máximo de 0.10
  
  #Grafico final intervalos de confianza
  ggplot(df, aes(x = as.factor(P_a_priori), y = Estimacion_Puntual_P)) +
    geom_point(aes(color = "Estimación Puntual de P"), size = 3) +
    geom_errorbar(aes(ymin = ic_inferior, ymax = ic_superior), width = 0.1) +
    geom_hline(yintercept = c(0.45, 0.55), linetype = "dashed", color = "red") +
    geom_text(aes(label = sprintf("Ancho: %.2f", ancho_ic), y = ic_inferior - 0.05), hjust = 0.5, vjust = 1) + 
    labs(y = "Proporción estimada",
         x = "P a priori",
         title = "Análisis de Intervalos de Confianza: Impacto de la Discrepancia entre P a Priori y
         Estimación Puntual en Comparación con el Error Absoluto") +
    scale_color_manual(name = "", values = c("Estimación Puntual de P" = "blue")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    # Lineas que represetan el error
    geom_segment(aes(x = max(as.numeric(as.factor(df$P_a_priori))) + 0.2, xend = max(as.numeric(as.factor(df$P_a_priori))) + 0.2,
                     y = 0.5, yend = 0.55), color = "red", size = 0.75) +
    
    geom_segment(aes(x = max(as.numeric(as.factor(df$P_a_priori))) + 0.2, xend = max(as.numeric(as.factor(df$P_a_priori))) + 0.2,
                     y = 0.5, yend = 0.45), color = "red", size = 0.75) +
    
    geom_segment(aes(x = max(as.numeric(as.factor(df$P_a_priori))) + 0.2 - 0.05, xend = max(as.numeric(as.factor(df$P_a_priori))) + 0.2 + 0.05,
                     y = 0.5, yend = 0.5), color = "red", size = 0.75) +
    
    # Etiquetas de Eo
    annotate("text", x = max(as.numeric(as.factor(df$P_a_priori))) + 0.2 + 0.05, y = 0.525, label = "Ea", hjust = 0, vjust = 0.5, color = "red") +
    
    annotate("text", x = max(as.numeric(as.factor(df$P_a_priori))) + 0.2 + 0.05, y = 0.475, label = "Ea", hjust = 0, vjust = 0.5, color = "red")