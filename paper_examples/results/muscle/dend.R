install.packages('pheatmap')
library(pheatmap)
install.packages("factoextra")
library(factoextra)



data = read.table('../../figures/muscle/expr.txt')
df = as.matrix(data)
#df[which(df < 5, arr.ind = T)] = 5
#df[which(df > 15, arr.ind = T)] = 15
dfs = apply(df, 1, scale)
dfs = t(dfs)
dfs[which(dfs < -2, arr.ind = T)] = -2
dfs[which(dfs > 2, arr.ind = T)] = 2
s = apply(dfs[,1:27], 1, sum)
dfs2 = dfs[order(s),]
pheatmap(dfs2,
         show_rownames = F,
         show_colnames = F,
         cluster_cols = F,
         cluster_rows=F,
         filename='expr_sort.pdf',
         height=6,
         width=5,
         scale = "none",
         color =colorRampPalette(c("#8854d0", "#ffffff","#fa8231"))(100),
         clustering_distance_cols = 'euclidean', 
         clustering_method = 'single',
)

colnames(dfs_avg) = c(0, 0.5, 1, 2, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5,
7, 7.5, 8, 8.5, 9, 9.5, 10, 11, 12, 13, 14,
16, 20, 30, 40)

dfs = t(apply(df, 1, scale))
dfs_1 = dfs[,seq(1, 54,by=2)]
dfs_2 = dfs[,seq(2, 54,by=2)]
dfs_avg = dfs_1 + dfs_2
dfs_bind = rbind(dfs_1, dfs_2)

dfdist = dist(t(dfs_avg))
dfhc = hclust(dfdist, method='single')
fviz_dend(dfhc)
fviz_dend(dfhc, k = 4, 
          cex = 0.5, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = TRUE          
)

colnames(dfs_bind) = c(0, 0.5, 1, 2, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5,
                      7, 7.5, 8, 8.5, 9, 9.5, 10, 11, 12, 13, 14,
                      16, 20, 30, 40)
dfdist = dist(t(dfs_bind))
dfhc = hclust(dfdist, method='complete')
fviz_dend(dfhc)
fviz_dend(dfhc, k = 3, 
          cex = 1,
          lwd = 0.8,
          #xlab = 'Day',
          ylab = '',
          k_colors = c("#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = F         
)