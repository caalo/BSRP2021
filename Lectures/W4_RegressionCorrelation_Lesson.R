setwd("~/Documents/Broad_Institute_BSRP/2021/")
load("CCLE_data_w_dependency.Rdata")
library(tidyverse)
library(corrplot)
#for correlation and regression lecture

#correlation
r = round(cor(expression$KRAS, expression$PIK3CA), 2)
ggplot(expression, aes(KRAS, PIK3CA)) + geom_point() + labs(x = "KRAS", y = "PIK3CA", title = paste0("r = ", r)) + 
  scale_x_continuous(limits = c(0, 8)) + scale_y_continuous(limits = c(0, 8)) + theme_bw()

ggplot(expression, aes(KRAS, PIK3CA)) + geom_point() + labs(x = "KRAS", y = "PIK3CA", title = paste0("r = ", r)) + 
  scale_x_continuous(limits = c(0, 8)) + scale_y_continuous(limits = c(0, 8)) +
  geom_vline(xintercept = mean(expression$KRAS), color = "blue") +
  geom_hline(yintercept = mean(expression$PIK3CA), color = "blue") + theme_bw()

expression_subset = expression %>% select(KRAS, NRAS, HRAS)
expression_mtx = cor(expression_mtx, use = "pairwise.complete.obs")
corrplot(expression_mtx, method="circle", type = "upper")

#regression
ggplot(expression, aes(KRAS, PIK3CA)) + geom_point() + labs(x = "KRAS", y = "PIK3CA") + 
  scale_x_continuous(limits = c(0, 8)) + scale_y_continuous(limits = c(0, 8)) +
  geom_smooth(method='lm', formula= y~x) + theme_bw()

pik3ca_model = lm(PIK3CA~KRAS, expression)
summary(pik3ca_model)

#regression residual
qplot(pik3ca_model$model$KRAS, pik3ca_model$residuals) +  labs(x = "KRAS", y = "Residuals") +
  scale_x_continuous(limits = c(0, 8)) +  scale_y_continuous(limits = c(-3, 3)) + theme_bw()


KRAS_mutations = mutation %>% filter(Hugo_Symbol == "KRAS" & Variant_Classification == "Missense_Mutation")
KRAS_mutations_CL = KRAS_mutations %>% group_by(DepMap_ID) %>% summarize(N_KRAS_mutations = n())
KRAS_expression = expression %>% select(DepMap_ID, KRAS) %>% rename(KRAS_expression = KRAS)
analysis = metadata %>% left_join(KRAS_mutations_CL, by = "DepMap_ID") %>% 
  left_join(KRAS_expression, by = "DepMap_ID")
analysis = analysis %>% mutate(KRAS_mutated = !is.na(N_KRAS_mutations))
KRAS_dependency = dependency %>% select(DepMap_ID, KRAS) %>% rename(KRAS_dependency = KRAS)
analysis = analysis %>% left_join(KRAS_dependency)


r = round(cor(analysis$KRAS_expression, analysis$KRAS_dependency, use = "complete.obs"), 2)

ggplot(analysis, aes(KRAS_expression, KRAS_dependency)) + geom_point() + labs(x = "KRAS expression", y = "KRAS dependency", title = paste0("r = ", r)) + 
  geom_abline(slope = 1) + geom_smooth(method='lm', formula= y~x) + theme_bw()

depModel = lm(KRAS_dependency~KRAS_expression, analysis)

qplot(depModel$model$KRAS_expression, depModel$residuals)
