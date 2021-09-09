library(clusterProfiler)
library(org.Mm.eg.db)

figs = list()
for (i in 1:25) {
  geneid = read.csv(paste('core',format(i),'.txt',sep=''), header=FALSE)
  eg = bitr(geneid$V1, fromType="SYMBOL", toType="ENTREZID", 
            OrgDb="org.Mm.eg.db")
  results = enrichGO(
    eg$ENTREZID,
    OrgDb='org.Mm.eg.db',
    keyType = "ENTREZID",
    ont = "ALL",
    pvalueCutoff = 0.05,
    pAdjustMethod = "BH",
    qvalueCutoff = 0.2,
    minGSSize = 10,
    maxGSSize = 500,
    readable = TRUE,
    pool = FALSE
  )

  dotplot(results, showCategory=20)
  # fig = dotplot(results, showCategory=20)
  # ggsave(paste('core',format(i),'.png',sep=''), fig, dpi=600)
  # write.csv(as.data.frame(results),file = paste('core',format(i),'.csv',sep=''))
}

