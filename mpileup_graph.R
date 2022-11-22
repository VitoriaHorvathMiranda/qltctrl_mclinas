library(tidyverse)
library(data.table)
library(dtplyr)

coverage_all <- fread(file = "/home/vitoria/covarage_site_allPops.tsv",
      sep = "\t", header = FALSE)

bad_variable_names <- str_c("V", 1:18)

variable_names <- c("chrom", "position", "ref", "ESC97", "CMD97B", "WVT97",
                    "MFL97", "RVA97", "SVT09", "CMD10", "SNC10", "JFL10",
                    "CMA97", "HMA09", "HMA17", "MCT09", "MCT17", "MCT97")

setnames(coverage_all, bad_variable_names, variable_names)

#torna o dado tidy
#transforma as populações em valores da variavel population
coverage_all <- melt(coverage_all,
                     measure = 4:18,
                     value.name = "coverage") 
  
setnames(coverage_all, "variable", "population")

coverage_all[, .(max(position), min(position)), keyby = chrom]


# cria uma coluna nova com o numero da janela
windows <- seq(1, 32073450, by = 40000)
coverage_all[, window := cut(position, windows)] 


window_coverage <- coverage_all[, .(mean_coverage = mean(coverage, na.rm = TRUE)), keyby = c("chrom", "population", "window")]


# por posicao -------------------------------------------------------------

#teste so com o cromossomo 2L para ver se o tamanho da janela esta bom
window_coverage[chrom == "2L"] %>%
  as_tibble() %>%
  ggplot() +
  geom_line(aes(group = population, x = window, y = mean_coverage, color = population))
  
coverage_graph <- as_tibble(window_coverage) %>%
  ggplot() +
  geom_line(aes(group = population, x = window, y = mean_coverage, color = population)) +
  facet_wrap(~ chrom, nrow = 2)+
  labs(x = "position", y = "average coverage")

coverage_graph +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0, 90))


# histograma --------------------------------------------------------------

window_coverage %>%
  ggplot() +
  geom_histogram(aes(mean_coverage)) +
  facet_wrap(~ population, nrow = 3)

