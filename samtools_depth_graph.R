library(tidyverse)
library(data.table)

coverage_all <- fread(file = "all_samtools_depth.tsv",
                      sep = "\t", header = FALSE)

bad_variable_names <- str_c("V", 1:17)

variable_names <- c("chrom", "position", "ESC97", "CMD97B", "WVT97",
                    "MFL97", "RVA97", "SVT09", "CMD10", "SNC10", "JFL10",
                    "CMA97", "HMA09", "HMA17", "MCT09", "MCT17", "MCT97")

setnames(coverage_all, bad_variable_names, variable_names)

chrom_size <- coverage_all[, .(min(position), max(position)), keyby = chrom]
setnames(chrom_size, c("V1", "V2"), c("min", "max"))
write_csv(chrom_size, "./summary/chrom_size.csv")

ggplot(data = chrom_size) +
  geom_histogram(aes(x = V2)) +
  labs(x = "size") 

ggsave("./plots/hist_chrom_size.png", height = 5, width = 10, dpi = 350)


# bed_chrom ---------------------------------------------------------------
strange_chrom <- coverage_all[!chrom %in% c("2L", "2R", "3R", "3L", "X")]

tidy_strange_chrom <- melt(strange_chrom,
                     measure = 3:17,
                     value.name = "coverage") 

setnames(tidy_strange_chrom, "variable", "population")

head(tidy_strange_chrom)

hist_coverage_badChron <- tidy_strange_chrom %>%
  ggplot() +
  geom_histogram(aes(coverage)) +
  facet_wrap(~ population, nrow = 3) +
  ggtitle("cromossomos não usuais")

ggsave("./plots/hist_coverage_badChron.png",
       plot = hist_coverage_badChron,
       height = 7, width = 11, dpi = 400)

hist_coverageYzoom_badChron <- hist_coverage_badChron +
  coord_cartesian(y = c(0, 100000)) +
  ggtitle("cromossomos não usuais - y-zoom")

ggsave("./plots/hist_coverageYzoom_badChron.png",
       plot = hist_coverageYzoom_badChron,
       height = 7, width = 10)

badchrom_pop_summary <- tidy_strange_chrom[, .(max(coverage), mean(coverage)), keyby = population]
setnames(badchrom_pop_summary, c("V1", "V2"), c("max", "mean"))
write_csv(badchrom_pop_summary, "./summary/badchrom_pop_summary.csv")

stats_per_chrom <- tidy_strange_chrom[, .(max(coverage), mean(coverage)), keyby = chrom] 
setnames(stats_per_chrom, c("V1", "V2"), c("max", "mean"))
setkey(stats_per_chrom, mean)

tail(stats_per_chrom)


# 2L 2R 3L 3R X -----------------------------------------------------------

usual_chrom <- coverage_all[chrom %in% c("2L", "2R", "3R", "3L", "X")]

tidy_usual_chrom <- melt(usual_chrom,
                           measure = 3:17,
                           value.name = "coverage") 

setnames(tidy_usual_chrom, "variable", "population")


# cria uma coluna nova com o numero da janela
windows <- seq(1, 32073450, by = 40000)
tidy_usual_chrom[, window := cut(position, windows)] 

#calcula a media da cobertura por janela
window_coverage <- tidy_usual_chrom[, .(mean_coverage = mean(coverage, na.rm = TRUE)), keyby = c("chrom", "population", "window")]

window_coverage %>% 
ggplot() +
  geom_histogram(aes(mean_coverage)) +
  facet_wrap(~ population, nrow = 3)

ggsave("./plots/hist_mean_coverage_usualChrom.png",
       height = 5, width = 8)


coverage_graph <- as_tibble(window_coverage) %>%
  ggplot() +
  geom_line(aes(group = population, x = window, y = mean_coverage, color = population)) +
  facet_wrap(~ chrom, nrow = 2)+
  labs(x = "position", y = "average coverage")

coverage_graph +
  scale_x_discrete(breaks = NULL) +
  coord_cartesian(ylim = c(0, 90))

ggsave("./plots/mean_coverage_usualChrom_perPosition.png",
       height = 4, width = 8)
