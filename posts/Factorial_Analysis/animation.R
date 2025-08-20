library(psych)
library(ggplot2)
library(gganimate)
library(dplyr)
library(tibble)
library(scales)
library(purrr)
library(tidyr)

# Simular datos
set.seed(123)
sim <- sim.structure(fx = matrix(c(.7, .6, .5, 0, 0, 0,
                                   0, 0, 0, .7, .6, .5), ncol = 2),
                     n = 300)
datos <- sim$observed

# Análisis factorial
fa_1     <- fa(datos, nfactors = 1, rotate = "none")
fa_2     <- fa(datos, nfactors = 2, rotate = "none")
fa_2rot  <- fa(datos, nfactors = 2, rotate = "varimax")

# Cargas
L1     <- as.matrix(fa_1$loadings)
L2     <- as.matrix(fa_2$loadings)
L2_rot <- as.matrix(fa_2rot$loadings)
vars   <- rownames(L2)

# CORRECCIÓN FUNDAMENTAL: Nueva función de interpolación
interpolar_flechas <- function(Lstart, Lend, pasos, etapa, desc) {
  map_df(seq_len(nrow(Lstart)), function(i) {
    var_name <- rownames(Lstart)[i]
    start_vec <- Lstart[i, ]
    end_vec <- Lend[i, ]
    
    map_df(0:pasos, function(p) {
      alpha <- p / pasos
      pos <- (1 - alpha) * start_vec + alpha * end_vec
      
      tibble(
        variable = var_name,
        F1 = pos[1],
        F2 = if(length(pos) > 1) pos[2] else 0,
        frame = p + etapa * 100,
        etapa = etapa,
        descripcion = desc
      )
    })
  })
}

pasos_por_etapa <- 40

inter_0a1 <- interpolar_flechas(
  matrix(0, nrow = 6, ncol = 1, dimnames = list(vars)),
  as.matrix(L1),
  pasos_por_etapa, 1, "1. Estimación del primer factor"
)

inter_1a2 <- interpolar_flechas(
  cbind(L1, 0),
  L2,
  pasos_por_etapa, 2, "2. Incorporación del segundo factor"
)

inter_rot <- interpolar_flechas(
  L2,
  L2_rot,
  pasos_por_etapa, 3, "3. Rotación varimax"
)

# Combinar datos
anim_data <- bind_rows(inter_0a1, inter_1a2, inter_rot) %>%
  mutate(
    variable = factor(variable),
    etapa_label = factor(etapa, labels = unique(descripcion))
  ) %>%
  group_by(variable, frame) %>%
  mutate(comunalidad = F1^2 + F2^2) %>%
  ungroup()

# Paleta de colores
colores <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928")

# Crear gráfico CORREGIDO
p <- ggplot(anim_data, aes(x = 0, y = 0, xend = F1, yend = F2)) +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  # UNA FLECHA POR VARIABLE
  geom_segment(aes(color = variable, group = interaction(variable, frame)),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"), 
               size = 1.3, show.legend = FALSE) +
  # Etiqueta por variable
  geom_text(aes(x = F1 * 1.1, y = F2 * 1.1, label = variable, color = variable),
            size = 4.5, fontface = "bold", show.legend = FALSE) +
  # Círculo de referencia
  annotate("path", 
           x = cos(seq(0, 2*pi, length.out = 100)),
           y = sin(seq(0, 2*pi, length.out = 100)),
           color = "gray60", size = 0.5, alpha = 0.3) +
  # Etiqueta de etapa
  geom_label(aes(x = -1.2, y = -1.2, label = descripcion),
             fill = "white", alpha = 0.8, size = 5, 
             hjust = 0, vjust = 0, label.size = 0) +
  # Ajustes visuales
  coord_fixed(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3), clip = "off") +
  scale_color_manual(values = colores) +
  labs(
    title = "Evolución del Espacio Factorial",
    subtitle = "Transformación de cargas factoriales durante el análisis",
    caption = "Cada flecha representa una variable observada\nAnimación muestra estimación, extracción y rotación"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    plot.caption = element_text(hjust = 0.5, color = "gray50"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Transición por frame
  transition_states(frame, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out') +
  enter_fade() +
  exit_fade()

# Calcular número total de frames únicos
n_frames <- length(unique(anim_data$frame))

# PARÁMETROS DE DURACIÓN AJUSTABLES
segundos_transicion <- 4    # Duración de cada transición principal
segundos_pausa_final <- 4   # Pausa al final
fps <- 12                   # Cuadros por segundo (reducido para mayor suavidad)

# Calcular frames necesarios
frames_transicion <- n_frames
frames_pausa_final <- segundos_pausa_final * fps
frames_totales <- frames_transicion + frames_pausa_final
duracion_total <- frames_totales / fps  # En segundos

# Renderizar animación con duración extendida
animate(p, 
        nframes = frames_totales,
        fps = fps,
        width = 800,
        height = 800,
        duration = duracion_total,
        renderer = gifski_renderer(loop = TRUE),
        end_pause = frames_pausa_final,
        rewind = FALSE)
# Guardar animación
animacion_path <- "animacion_espacio_factorial.gif"
anim_save(animacion_path, animation = last_animation())
