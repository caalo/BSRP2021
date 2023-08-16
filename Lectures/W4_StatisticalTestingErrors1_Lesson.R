library(tidyverse)
theme_set(theme_bw())

#generate data
set.seed(1111)

n_cell_lines <- 50


#H0 is true - no difference in gene expression - mean value of 5. 
gene_exp <- data.frame(gene = character(),
                       cell_line = character(),
                       exp = numeric())
reps = 100
#run 100 technical replicates
for(i in 1:reps) {
  WT_exp <- rnorm(n_cell_lines, mean = 5, sd = 1)
  MT_exp <- rnorm(n_cell_lines, mean = 5, sd = 1)
  df <- data.frame(gene = paste0("reps ", i), 
                   cell_line = c(rep("WT", n_cell_lines), rep("MT", n_cell_lines)),
                   exp = c(WT_exp, MT_exp))
  gene_exp <- rbind(df, gene_exp)
}


ggplot(gene_exp %>% filter(gene %in% paste0("reps ", 1:10)), aes(x = cell_line, y = exp)) + geom_boxplot() + facet_wrap(~gene)


run_t_test <- function(group, values) {
  df <- data.frame(group = group, values = values)
  test <- t.test(values~group, data = df)
  #returns:
  tibble(p_value = test$p.value,
         effect_size = test$estimate[2] - test$estimate[1])
}

gene_exp_test <- gene_exp %>% 
  group_by(gene) %>% 
  summarize(run_t_test(cell_line, exp))

ggplot(gene_exp_test, aes(x = effect_size)) + geom_histogram(bins = 30)
ggplot(gene_exp_test, aes(x = p_value)) + geom_histogram(bins = 30) + geom_vline(xintercept = .05)



                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                