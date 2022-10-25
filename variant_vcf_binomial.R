library(tidyverse)
#data: https://figshare.com/articles/dataset/Whole_Exome_Data_VCF_files/13696750/1

v <- read.csv('~/Downloads/JAS_N36.GATK.snp.vcf', sep = '\t')
v2 <- v %>%
  separate(JAS_N36, c("GT","AD","DP","GQ","PL"), ':') %>%
  filter(GT == '0/1') %>%
  separate(AD, c("AD0","AD1"), ',') %>%
  mutate(ratio = round(100*as.integer(AD0) / (as.integer(AD0) + as.integer(AD1)), digits = 0))

ggplot(v2, aes(ratio)) + geom_histogram(bins = 100)

hist(rbinom(length(v2$ratio), 100, prob = .5))

chisq.test(rbinom(length(v2$ratio), 100, prob = .5), v2$ratio) #doesn't work - always changes
