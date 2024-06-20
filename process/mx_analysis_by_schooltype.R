###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



# libraries
library(tidyverse)
library(styleBreton)
library(ggstatsplot)
library(sjPlot)
library(quantreg)
library(emmeans)
library(ggridges)



# data acquisition
load("data/students_22.RData")


# data mexico
students_mx <- 
  students_clean |> 
  filter(CNT == "Mexico") |> 
  janitor::clean_names()


# variable selection
vari <- 
  c("anxmat", "mathpers", "cogacmco", "st255q01ja", "st004d01t")


# dataset for analysis
st_mx <- 
  students_mx |> 
  mutate(type = case_when(str_detect(stratum, "Private") ~ "privada",
                          str_detect(stratum, "Public") ~ "pública")) |> 
  select(type, all_of(vari), pv1math, pv1read, pv1scie) |> 
  rename(books = st255q01ja,
         gender = st004d01t) |> 
  mutate(math_level = case_when(
    pv1math <= 232.9999 ~ "Below 1c",
    between(pv1math, 233, 294.9999) ~ "1c",
    between(pv1math, 295, 357.9999) ~ "1b",
    between(pv1math, 358, 419.9999) ~ "1a",
    between(pv1math, 420, 481.9999) ~ "2",
    between(pv1math, 482, 544.9999) ~ "3",
    between(pv1math, 545, 606.9999) ~ "4",
    between(pv1math, 607, 668.9999) ~ "5",
    pv1math >= 669 ~ "6",
    .default = "No Level")) |> 
  mutate(math_level = factor(math_level, 
                             levels = c("Below 1c", "1c", 
                                        "1b", "1a", 
                                        "2", "3", "4", 
                                        "5", "6")))

write_rds(st_mx, "data/students_mx.rds")

# table per type
st_mx |> 
  (\(x) table(x$type))() |> 
  prop.table()

# find percentage of students in math_level 2 or below
st_mx |> 
  filter(math_level %in% c("Below 1c", "1c", "1b", "1a")) |> 
  nrow() / nrow(st_mx)



# exploration
skimr::skim(st_mx)


# overall average score in math
students_clean |> 
  summarize(media_math = mean(PV1MATH, na.rm = TRUE))

# mex average score in math
st_mx |> 
  summarize(media_math = mean(pv1math, na.rm = TRUE))


# visualization of math scores by school type
st_mx |> 
  ggplot(aes(x = type, y = pv1math, color = type, fill = type)) +
  geom_violin(color = "grey45", fill = "grey90",
              alpha = 0.1) +
  geom_jitter(width = 0.35,
              alpha = 0.15) +
  stat_summary(geom = "pointrange", 
               fun = "mean",
               color = "darkred") +
  labs(title = "Muestra Mexicana",
       y = "Puntaje en Matemáticas",
       x = "Tipo de Escuela",
       caption = "Fuente: OECD, 2022 PISA Results <br>
       Juan L. Bretón, PMP | @juanlbreton") +
  theme_breton() +
  theme(legend.position = "none")


# histogram per type of school
st_mx |> 
  group_by(type) |> 
  plot_frq(pv1math,
           type = "histogram",
           show.mean = TRUE,
           normal.curve = TRUE) |> 
  plot_grid()


# first sample
set.seed(3236)
samp_01 <- 
  st_mx |> 
  group_by(type) |> 
  slice_sample(n = 650) |> 
  ungroup() 


# normality check for math score by type of school
samp_01 |>
  group_by(type) |> 
  dlookr::normality(pv1math)


# density per type of school
samp_01 |> 
  ggplot(aes(x = pv1math, fill = type, color = type)) +
  geom_density(alpha = 0.3, show.legend = FALSE) +
  geom_vline(xintercept = 420, 
             color = "grey45",
             linewidth = 2.5,
             alpha = 0.4) +
  facet_grid(rows = vars(type)) +
  theme_breton()


# mann-whitney u test per type of school
ggbetweenstats(data = samp_01,
               x = type,
               y = pv1math,
               type = "nonparametric") +
  labs(title = "El tipo de escuela incide en el aprendizaje de las Matemáticas",
       # subtitle = "Muestra de escuelas mexicanas",
       y = "Puntaje en Matemáticas",
       x = "Tipo de escuela",
       caption = "Fuente: OECD, 2022 PISA Results, México <br>
       Modelaje y visualización: Juan L. Bretón, PMP | @juanlbreton") +
  theme_breton()

ggsave(filename = "figures/type_math01.jpg", device = "jpeg", dpi = "retina")

# interpretation of effect
effectsize::interpret_rank_biserial(0.43)


# quantile visualization
st_mx |> 
  ggplot(aes(x = pv1math, y = type,
             fill = factor(after_stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantile_lines = TRUE, 
                      quantiles = c(0.1, 0.5, 0.9)) +
  labs(title = "El desempeño de los estudiantes se desfasa según el tipo de escuela",
       subtitle = "Diferencias en el puntaje de Matemáticas por nivel de desempeño",
       x = "Puntaje en Matemáticas",
       y = "Tipo de escuela",
       fill = "Cuantil") +
  theme_breton() +
  theme(legend.position = "top")

# regression per type of school
qt_10 <- rq(pv1math ~ type, data = st_mx, tau = 0.1)
qt_50 <- rq(pv1math ~ type, data = st_mx, tau = 0.5)
qt_90 <- rq(pv1math ~ type, data = st_mx, tau = 0.9)

qt_50 |> 
  emmeans(~ type)


# comparison of models
plot_models(qt_10, qt_50, qt_90,
            show.values = TRUE,
            m.labels = c("Cuantil 10", "Cuantil 50", "Cuantil 90"),
            legend.title = "Modelo") +
  labs(title = "La diferencia entre escuela pública y privada afecta más a estudiantes con desempeños medios",
       subtitle = "Diferencias en el puntaje de Matemáticas",
       y = "Diferencia de puntos con respecto a las escuelas privadas") +
  theme_breton() +
  theme(legend.position = "top")


ggsave(filename = "figures/type_math02.jpg", device = "jpeg", dpi = "retina")


# comparison with number of books
st_mx |> 
  group_by(type) |> 
  plot_frq(books) |> 
  plot_grid()

books_eff_lm <- 
  lm(pv1math ~ books, 
     data = st_mx)

plot_model(books_eff_lm, 
           type = "pred", 
           terms = "books")

books_eff_lm |> 
  emmeans(~ books) |> 
  as_tibble()


# quantile regression
books_eff_qt <- 
  rq(pv1math ~ books, 
     data = st_mx)

# marginals means of scores
books_eff_qt |> 
  emmeans(~ books) |> 
  as_tibble()

# predicted math scores
plot_model(books_eff_qt, 
           type = "pred", 
           terms = "books",
           show.values = TRUE, 
           colors = "darkred") +
  geom_hline(yintercept = 420,
             alpha = 0.15,
             linewidth = 3.5,
             color = "grey60") +
  annotate(geom = "text",
           label = "Línea base de participación en la sociedad",
           family = "Encode Sans Condensed",
           size = 3.0,
           x = 1.90,
           y = 420,
           color = "grey45") +
  labs(title = "Tener libros en el hogar aumenta el conocimiento y las habilidades matemáticas",
       subtitle = "Estimación de puntaje en razón de la presencia de libros",
       x = "Número de libros existentes en el hogar",
       y = "Puntaje en Matemáticas",
       caption = "Fuente: OECD, 2022 PISA Results, México <br>
       Modelaje y visualización: Juan L. Bretón, PMP | @juanlbreton") +
  theme_breton()

ggsave(filename = "figures/type_math03.jpg", device = "jpeg", dpi = "retina")


# points increased by books
books_eff_qt |> 
  emmeans(pairwise ~ books)

# visualization
plot_model(books_eff_qt, 
           show.values = TRUE, 
           width = 0.1,
           colors = "darkred") +
  labs(title = "¿Cuánto mejora el aprendizaje de las Matemáticas cuando hay libros en el hogar?",
       subtitle = "Estimación de puntos de incremento",
       y = "Puntos de incremento respecto a 0 libros en el hogar",
       x = NULL,
       caption = "Fuente: OECD, 2022 PISA Results, México <br>
       Modelaje y visualización: Juan L. Bretón, PMP | @juanlbreton") +
  theme_breton()

ggsave(filename = "figures/type_math04.jpg", device = "jpeg", dpi = "retina")


# model with interactions
books_eff_all <- 
  rq(pv1math ~ books * type, 
     data = st_mx) 

books_eff_all |> 
  emmeans(pairwise ~ type | books)

books_eff_all |> 
  plot_model(show.values = TRUE,
             type = "pred",
             colors = c("#E18B0E", "#2A8A10"),
             terms = c("books", "type")) +
  geom_hline(yintercept = 420,
             alpha = 0.15,
             linewidth = 3.5,
             color = "grey60") +
  annotate(geom = "text",
           label = "Línea base",
           family = "Encode Sans Condensed",
           size = 3.0,
           x = 0.90,
           y = 420,
           color = "grey45") +
  labs(title = "La existencia de libros en el hogar mejora el aprendizaje de las Matemáticas",
       subtitle = "Estimación de puntaje en Matemáticas por tipo de escuela",
       x = "Libros existentes en el hogar",
       y = "Puntaje estimado",
       caption = "Fuente: OECD, 2022 PISA Results, México <br>
       Modelaje y visualización: Juan L. Bretón, PMP | @juanlbreton",
       color = "Tipo de escuela") +
  theme_breton() +
  theme(legend.position = "top")


ggsave(filename = "figures/type_math05.jpg", 
       device = "jpeg", 
       dpi = "retina")







