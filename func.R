# funções (Gráficos e tabelas descritivas): ================================== #

## Gráfico de boxplot -------------------------------------------------------- #

graf_box<-function(var_num,var_cat,legx="x",legy="y",title=""){
  a<-data.frame(Var1=var_num,Var2=var_cat)
  ggplot(a, aes(x = Var2, y = Var1,fill=Var2))+
    geom_boxplot(show.legend = F,size=0.35,outlier.size = 0.9) +
    scale_fill_brewer(palette = "Paired", direction = 1) +
    labs(title = title,
         x = legx,
         y = legy) +
    theme_bw(base_size = 12) +
    theme(panel.grid = ggplot2::element_blank(),
          plot.title = ggplot2::element_blank())
}

## Tabela descritiva --------------------------------------------------------- #

medidas_quant <- function(df){
  df_num <- df[, sapply(df, is.numeric)]
  res <- lapply(df_num, function(x){
    p_shapiro <- shapiro.test(x)$p.value
    p_shapiro_fmt <- ifelse(p_shapiro < 0.001, "<0.001", round(p_shapiro, 3))
    c(n = sum(!is.na(x)),
      Mín = round(min(x, na.rm = TRUE), 3),
      Mediana = round(median(x, na.rm = TRUE), 3),
      Média = round(mean(x, na.rm = TRUE), 3),
      Máx = round(max(x, na.rm = TRUE), 3),
      DP = round(sd(x, na.rm = TRUE), 3),
      p_Shapiro = p_shapiro_fmt,
      `NA's` = sum(is.na(x)))
  })
  res_df <- as.data.frame(do.call(rbind, res))
  res_df$Variável <- rownames(res_df)
  rownames(res_df) <- NULL
  res_df <- res_df[, c("Variável", colnames(res_df)[1:8])]
  return(res_df)
}

