library(tidyverse)
library(data.table)
library(dtplyr)

coverage_all <- fread(file = "/home/vitoria/all_samtools_depth.tsv",
                      sep = "\t", header = FALSE)

bad_variable_names <- str_c("V", 1:17)

variable_names <- c("chrom", "position", "ESC97", "CMD97B", "WVT97",
                    "MFL97", "RVA97", "SVT09", "CMD10", "SNC10", "JFL10",
                    "CMA97", "HMA09", "HMA17", "MCT09", "MCT17", "MCT97")

setnames(coverage_all, bad_variable_names, variable_names)

chrom_size <- coverage_all[, .(min(position), max(position)), keyby = chrom]

ggplot(data = chrom_size) +
  geom_histogram(aes(x = V2)) +
  labs(x = "size")


strange_chrom <- coverage_all[!chrom %in% c("2L", "2R", "3R", "3L", "X")]

tidy_strange_chrom <- melt(strange_chrom,
                     measure = 3:17,
                     value.name = "coverage") 

setnames(tidy_strange_chrom, "variable", "population")

head(tidy_strange_chrom)

tidy_strange_chrom %>%
  ggplot() +
  geom_histogram(aes(coverage)) +
  facet_wrap(~ population, nrow = 3) +
  ggtitle("cromossomos não usuais - no-zoom")

tidy_strange_chrom %>%
  ggplot() +
  geom_histogram(aes(coverage)) +
  facet_wrap(~ population, nrow = 3) +
  coord_cartesian(x = c(0, 100)) +
  ggtitle("cromossomos não usuais - x-zoom")

tidy_strange_chrom %>%
  ggplot() +
  geom_histogram(aes(coverage)) +
  facet_wrap(~ population, nrow = 3) +
  coord_cartesian(y = c(0, 1000)) +
  ggtitle("cromossomos não usuais - y-zoom")

tidy_strange_chrom[, .(max(coverage), mean(coverage)), keyby = population]
tidy_strange_chrom[, .(max(coverage), mean(coverage)), keyby = chrom] |>
  setkey()


