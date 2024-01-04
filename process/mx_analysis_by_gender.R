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
                                        "5", "6")),
         baseline = if_else(pv1math > 419.9999,
                            1, 0))


# eda
skimr::skim(st_mx)


# table of gender
st_mx |> 
  (\(x) table(x$gender))() |> 
  prop.table()

# overall score in math 
students_clean |> 
  janitor::clean_names() |>
  summarize(math_score = mean(pv1math, na.rm = TRUE),
            .by = st004d01t) |> 
  knitr::kable(digits = 2, 
               col.names = c("gender", "math score"),
               format = "pipe")

# mexico score in math
gender_scores <- 
  st_mx |>
  summarize(math_score = mean(pv1math, na.rm = TRUE),
            .by = gender)
  
gender_scores |> 
  knitr::kable(digits = 2, 
               col.names = c("gender", "math score"),
               format = "pipe")

# histogram of scores 
st_mx |> 
  ggplot(aes(x = pv1math, fill = gender)) +
  geom_histogram(aes(y = after_stat(density)),
                 color = "white",
                 bins = 50,
                 alpha = 0.75) +
  geom_vline(xintercept = 420,
             linewidth = 2.2,
             alpha = 0.2) +
  geom_vline(data = gender_scores,
             aes(xintercept = math_score),
             color = "grey45") +
  facet_grid(rows = vars(gender)) +
  theme_breton() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(233, 669)) +
  scale_fill_manual(values = c("darkgreen", "steelblue")) +
  labs(title = "Histogramas por Género",
       subtitle = "Matemáticas en estudiantes mexicanos",
       y = NULL,
       x = "Puntaje")



# hypothesis test on mean score in math by gender
set.seed(558)
gender_sample <- 
  st_mx |> 
  filter(gender == "Female" | gender == "Male") |> 
  group_by(gender) |>
  slice_sample(n = 1900) |> 
  ungroup()


# normality check
gender_sample |> 
  group_by(gender) |> 
  dlookr::normality(pv1math)

# mann whitney test
# difference is not significant
ggbetweenstats(data = gender_sample,
               x = gender,
               y = pv1math,
               type = "nonparametric") +
  labs(title = "El género tiene muy poca incidencia en el aprendizaje de las Matemáticas",
       # subtitle = "Muestra de escuelas mexicanas",
       y = "Puntaje en Matemáticas",
       x = "Género",
       caption = "Fuente: OECD, 2022 PISA Results, México <br>
       Modelaje y visualización: Juan L. Bretón, PMP | @juanlbreton") +
  theme_breton()

ggsave("figures/gen_math_hypo.jpg", device = "jpeg", dpi = "retina")

# interpretation
effectsize::interpret_rank_biserial(-0.07)

wilcox.test(gender_sample |> 
              filter(gender == "Female") |> 
              select(pv1math) |> pull(),
            gender_sample |> 
              filter(gender == "Male") |> 
              select(pv1math) |> pull())


# table by level
col_lev1 <- 
  c("#005F8B", "#007EB9", "#B1DDFC", "#DDF1FF",
    "#EDEDED", "#DFDFDF", "#939393", "#424242")


st_mx |> 
  # summarize(media = mean(baseline)) |> 
  count(math_level) |> 
  mutate(pct = n / sum(n),
         cummu = cumsum(pct))

res_gen <- 
  st_mx |> 
  count(gender, math_level) |> 
  group_by(gender) |> 
  mutate(pct = n / sum(n),
         cummu = cumsum(pct)) |> 
  ungroup()


res_gen |> 
  ggplot(aes(y = gender,
             weight = n,
             fill = factor(math_level,
                           levels = rev(levels(math_level))))) +
  geom_bar(position = "fill",
           width = 0.65) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_grid(rows = vars(gender), scales = "free_y") +
  scale_fill_manual(values = col_lev1) +
  labs(title = "Distribución por Nivel de Desempeño en Matemáticas",
       subtitle = "Por género de estudiantes de México",
       y = NULL,
       x = "Proporción de estudiantes",
       fill = "Nivel PISA",
       caption = "Fuente: OECD, 2022 PISA Results, México <br>
       Modelaje y visualización: Juan L. Bretón, PMP | @juanlbreton") +
  theme_breton()

ggsave("figures/gen_propor01.jpg", device = "jpeg", dpi = "retina")


col_lev_up <- 
  c("#005F8B", "#007EB9", "#B1DDFC", "#DDF1FF")


st_mx |> 
  filter(baseline == 1) |> 
  count(gender, math_level) |> 
  ggplot(aes(x = n,
             y = gender, 
             fill = factor(math_level,
                           levels = rev(levels(math_level))))) +
  geom_col(position = "stack", width = 0.65) +
  scale_fill_manual(values = col_lev_up) +
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(rows = vars(gender), scales = "free_y") +
  theme_breton() -> p1
  

col_lev_do <- 
  c("#424242", "#939393", "#DFDFDF", "#EDEDED")
 
st_mx |> 
  filter(baseline != 1) |> 
  count(gender, math_level) |> 
  mutate(n = n * -1) |> 
  ggplot(aes(x = n,
             y = gender, 
             fill = math_level)) +
  geom_col(position = "stack", width = 0.65) +
  scale_fill_manual(values = col_lev_do) +
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(rows = vars(gender), scales = "free_y") +
  theme_breton() -> p2


ggpubr::ggarrange(p2, p1,
                  nrow = 1)

st_mx |> 
  filter(baseline == 1) |> 
  ggplot(aes(y = math_level)) +
  geom_bar(aes(x = after_stat(count / sum(count))),
           position = "stack") +
  facet_wrap(~ gender, ncol = 1)
























