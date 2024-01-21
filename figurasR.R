#### SIMULACIÓN BOLETIN BIOESTADISTICA ####
packages_needed <- c("dplyr", "ggplot2")

for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# FUNCIÓN PARA CALCULAR EL TAMAÑO DE MUESTRA
p.size <- function(p,a,e){
  z <- qnorm(1 - (a / 2))
  n <- (z^2 * p * (1 - p)) / e^2
  return(ceiling(n))
}

p.size(0.3,0.05,0.05) # 385
# MAXIMA VARIANZA
# 5% DE ERROR ABSOLUTO
# 95% DE CONFIANZA

# FUNCION PARA CALCULAR EL IC

ci.prop <- function(sample, a) {
  p <- mean(sample)
  se <- sqrt(p * (1 - p) / length(sample))
  margen_error <- qnorm(1 - a/ 2) * se
  intervalo_confianza <- c(p - margen_error, p + margen_error)
}

# FIGURA 1

valores_error_absoluto <- seq(0.1, 0.01, by = -0.01)

# Calcular el tamaño de muestra necesario
n_muestra <- sapply(valores_error_absoluto, function(e) p.size(0.5, 0.05, e))

# Crear el dataframe con los valores en orden decreciente
data_plot1 <- data.frame(error_absoluto = valores_error_absoluto, n_muestra = n_muestra)

# Crear el gráfico
ggplot(data_plot1, aes(x = error_absoluto, y = n_muestra)) +
  geom_line() +
  geom_point(color = "red") +
  geom_text(aes(label = n_muestra), vjust = -0.5, color = "blue") +
  scale_x_continuous(breaks = valores_error_absoluto, labels = valores_error_absoluto) +
  coord_cartesian(xlim = c(0.1, 0.01)) + # Establece los límites para el eje X dentro del rango de datos
  theme_minimal() +
  xlab("Error Absoluto") +
  ylab("Tamaño de Muestra Requerido")

# POBLACION CONCUERDA CON EL P DEFINIDO EN EL CALCULO DE TAMAÑO MUESTRAL
set.seed(123)
error1 <- 0.5
poblacion <- rbinom(100000, 1,0.2)
muestra1 <- sample(poblacion, 284)
mean(poblacion) # P = 49.7% 
p_hat = round(mean(muestra1),3)*100 # p-hat = 55.8%
e1_5 <- ci.prop(muestra1, 0.05)
escenario1 <- data.frame(x = "P en TM = P",
                         p_hat = p_hat,
                         low = e1_5[1],
                         up = e1_5[2])
escenario1$p_hat-escenario1$low
escenario1$up - escenario1$p_hat

# En este escenario la distancia entre la estimación puntual y los 
# límites del intervalo no supera el error absoluto.
# Ahora vamos a repetir el experimento 10.000 veces

set.seed(123)
n_simulaciones <- 10000
size_muestra <- 246
p_real <- 0.2
poblacion <- rbinom(100000, 1, p_real)

resultados <- data.frame(p_hat = numeric(n_simulaciones),
                         lower_bound = numeric(n_simulaciones),
                         upper_bound = numeric(n_simulaciones))

for (i in 1:n_simulaciones) {
  muestra <- sample(poblacion, size_muestra)
  p_hat <- mean(muestra)
  intervalo <- ci.prop(muestra, 0.05)
  resultados[i, ] <- c(p_hat, intervalo[1], intervalo[2])
}

# Cálculo de la distancia entre p_hat y los límites del intervalo
resultados$dist_lower = resultados$p_hat - resultados$lower_bound
resultados$dist_upper = resultados$upper_bound - resultados$p_hat

resultados <- resultados %>% 
  mutate(prop_ea = ifelse(dist_lower >0.05,1,0))
prop.table(table(resultados$prop_ea))

# EL 46% TIENE UNA DISTANCIA SUPERIOR ENTRE LA ESTIMACIÓN PUNTUAL Y EL LIMITE
# DEL INTERVALO

# REPETIMOS EL EXPERIMENTO 10.000 VECES CAMBIANDO EL VALOR DE LA PROPORCIÓN
set.seed(1234)
n_simulaciones <- 10000
props <- seq(0.01, 0.9, by = 0.01)
error_absoluto <- 0.05
alfa <- 0.05
n_muestra <- sapply(props, function(p) p.size(props, alfa, error_absoluto))
proporciones_excedidas <- numeric(length(props))

for (i in 1:length(props)) {
  p_real <- props[i]
  size_muestra <- p.size(p_real, alfa, error_absoluto)
  poblacion <- rbinom(100000, 1, p_real)
  
  excedidas <- 0
  
  for (j in 1:n_simulaciones) {
    muestra <- sample(poblacion, size_muestra)
    p_hat <- mean(muestra)
    intervalo <- ci.prop(muestra, alfa)
    
    amplitud <- intervalo[2] - intervalo[1]
    if (amplitud > 2 * error_absoluto) {
      excedidas <- excedidas + 1
    }
  }
  
  proporciones_excedidas[i] <- excedidas / n_simulaciones
}

data_plot <- data.frame(p_real = props, proporciones_excedidas = proporciones_excedidas, n_muestra = n_muestra)
data_plot$n_muestra <- data_plot$n_muestra[, 1]

# FIGURA 2
ggplot(data_plot, aes(x = p_real, y = proporciones_excedidas)) +
  geom_line() + # Usa geom_line() para conectar los puntos con líneas
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1)) + # Ajusta según tus datos
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.1)) +
  xlab("Proporción Real") +
  ylab("Proporción de Intervalos que Exceden el Doble del Error Absoluto")


# FIGURA 3
ggplot(data_plot, aes(x = n_muestra, y = proporciones_excedidas)) +
  geom_line() + # Usa geom_line() para conectar los puntos con líneas
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.1)) +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 50)) +
  xlab("Tamaño de Muestra") +
  ylab("Proporción de Intervalos que Exceden el Doble del Error Absoluto")
