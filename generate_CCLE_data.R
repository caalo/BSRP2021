metadata = read.csv('sample_info.csv', stringsAsFactors = F)
mutation = read.csv('CCLE_mutations.csv', sep="\t", stringsAsFactors = F)
expression = read.csv("CCLE_expression.csv", stringsAsFactors = F, as.is = T, check.names = F)
knockdown = read.csv("Achilles_gene_effect.csv", stringsAsFactors = F)
names(expression)[1] = "DepMap_ID"
colnames(expression) = gsub("(.*)\040\\(.*", "\\1", colnames(expression)) #get rid of (id)
dependency = read.csv("Achilles_gene_effect.csv", stringsAsFactors = F, as.is = T, check.names = F)
colnames(dependency) = gsub("(.*)\040\\(.*", "\\1", colnames(dependency)) #get rid of (id)
save(expression, mutation, dependency, metadata, file = "../CCLE_data_w_dependency.Rdata")
save(expression, mutation, metadata, file = "../CCLE_data.Rdata")

RNA_reads = read.csv('CCLE_RNAseq_reads.csv', row.names = 1)
colnames(RNA_reads) = gsub("(.*)\\.\\..*", "\\1", colnames(RNA_reads))
RNA_reads = t(RNA_reads)
RNA_reads = RNA_reads[, !duplicated(colnames(RNA_reads))]
RNA_reads = as.data.frame(RNA_reads) 
save(RNA_reads, file = "../CCLE_RNAseq_reads.Rdata")
