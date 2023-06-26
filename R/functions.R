#' functions file
#' @export
#'

#library(ggplot2)
# library(ggpubr)
# library(RVAideMemoire)
# library(nortest)
# library(dplyr)
# library(car)


# ---------------- Reading csv file ----------------

read_file = function(datapath,header,sep){
  file = utils::read.csv(datapath,header = header, sep= sep)
  return(file)
}

# -------------- Data type analysis -----------

data_type = function(df){
  columnas = base::names(data)
  data_types = base::sapply(df, class)
  dfo <- base::data.frame(Variable = columnas, TipoDato = data_types)
  return(dfo)
}


# ---------- Missing Values in each column  ------------

na_counts = function(df){
  matrix_na = base::is.na(df)
  valores_faltantes = base::colSums(matrix_na)
  dframe = base::as.data.frame(valores_faltantes)
  return(dframe)
}

# ------- Convertir a factor -----------
convert_to_factor = function(df,colname){
  df[[colname]] = base::as.factor(df[[colname]])
  return(df)
}

# ------- Histogram --------
histogram = function(df,groupcol,colname,inpbins){
  columna = df[[colname]]
  group_col = df[[groupcol]]
  columna_numerica = as.numeric(columna)
  if(groupcol == 'No agrupar'){
    return(ggplot2::ggplot(df, ggplot2::aes(x = columna_numerica)) + ggplot2::geom_histogram(alpha = 0.5, position = "identity", bins = inpbins))
  }
  else{
    return(ggplot2::ggplot(df, ggplot2::aes(x = columna_numerica)) + ggplot2::geom_histogram( bins = inpbins) + labs(title='Histogram',x=colname)+   ggplot2::facet_grid({{groupcol}}) + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . , name = groupcol, breaks = NULL, labels = NULL)) )
    }
}
# ------ BOX PLOT --------

my_boxplot = function(df,colname,groupcol){
  df <- stats::na.omit(df)
  columna = df[[colname]]
  columna_numerica = base::as.numeric(columna)
  group_col = df[[groupcol]]

  if(groupcol == 'No agrupar'){
    return(ggplot2::ggplot(data=df, ggplot2::aes_string(x=columna_numerica, fill=group_col, color=group_col)) +
              ggplot2::geom_boxplot() + ggplot2::coord_flip() + ggplot2::stat_boxplot(geom = "errorbar",width = 0.25) )
  }
  else{
    return(ggplot2::ggplot(df, ggplot2::aes(x = group_col, y =columna_numerica, color=group_col)) + ggplot2::geom_boxplot())
  }
}


#------- QQ PLOT --------
my_qqplot = function(df,colname,groupcol){
  df <- stats::na.omit(df)
  columna = df[[colname]]
  columna_numerica = base::as.numeric(columna)
  group_col = df[[groupcol]]

  if(groupcol == 'No agrupar'){
    grafico <- stats::qqnorm(columna, pch = 1, frame = FALSE)
    linea <- stats::qqline(columna, col = "steelblue", lwd = 2)
    return(grafico)
    return(linea)
  }
  else{
    grafico <- ggplot2::ggplot(df, ggplot2::aes(sample = columna_numerica, color=factor(group_col))) + ggplot2::stat_qq() + ggplot2::stat_qq_line()
    return(grafico)
  }
}

# ----------- SHAPIRO _ WILK HYPOTHESIS TESTING ---------------
shapiro_w = function(df,colname,groupcol){
  if (groupcol == "No agrupar") {
    return(stats::shapiro.test(df[[colname]]))
  }
  else{
    df_resultados = base::data.frame(Test = base::character(),Grupo = base::numeric(),Estadistico = base::numeric(),Pvalor=base::numeric())
    fila_nueva = data.frame(Test = base::character(),Grupo = base::numeric(),Estadistico = base::numeric(),Pvalor=base::numeric())
    columna_numerica = df[[colname]]
    columna_group = base::as.factor(df[[groupcol]])
    grupos <- base::unique(columna_group)
    resultados <- base::vector("list", base::length(grupos))
    for (i in base::seq_along(grupos)) {
      subgrupo <- base::subset(df,columna_group == grupos[i])
      resultados[[i]] <- stats::shapiro.test(subgrupo[[colname]])
      fila_nueva = base::data.frame(Test = resultados[[i]]$method,Grupo = i,Estadistico = resultados[[i]]$statistic,Pvalor=resultados[[i]]$p.value)
      df_resultados <- base::rbind(df_resultados, fila_nueva)
    }
    return(df_resultados)
  }
}


# # --------- Lilliefors -------------
# lilliefors = function(df,colname,groupcol){
#   if (groupcol == "No agrupar") {
#     return(lillie.test(df[[colname]]))
#   }
#   else{
#     df_resultados = data.frame(Test = character(),Grupo = numeric(),Estadistico = numeric(),Pvalor=numeric())
#     fila_nueva = data.frame(Test = character(),Grupo = numeric(),Estadistico = numeric(),Pvalor=numeric())
#     columna_numerica = df[[colname]]
#     columna_group = as.factor(df[[groupcol]])
#     grupos <- unique(columna_group)
#     resultados <- vector("list", length(grupos))
#     for (i in seq_along(grupos)) {
#       subgrupo <- subset(df,columna_group == grupos[i])
#       resultados[[i]] <- lillie.test(subgrupo[[colname]])
#       fila_nueva = data.frame(Test = resultados[[i]]$method,Grupo = i,Estadistico = resultados[[i]]$statistic,Pvalor=resultados[[i]]$p.value)
#       df_resultados <- rbind(df_resultados, fila_nueva)
#     }
#     return(df_resultados)
#   }
# }
#
#
# # --------- Test de Levene ---------
# levene = function(dataf, nombrecol, groupocol){
#     columna_numerica = dataf[[nombrecol]]
#     columna_group = as.factor(dataf[[groupocol]])
#     data <- data.frame(columna_numerica,columna_group)
#     test = leveneTest(data$columna_numerica,data$columna_group)
#     return(test)
#   }
# # ------ Test ... ---------
# my_test = function(df,colname,groupcol){
#   df <- na.omit(df)
#   #columna = df[[colname]]
#   #columna_numerica = as.numeric(columna)
#   columna_group = as.factor(df[[groupcol]])
#   grupos <- unique(columna_group)
#   #resultados <- vector("list", length(grupos))
#   subgrupo1 <- subset(df,columna_group == grupos[[1]])
#   subgrupo2 <- subset(df,columna_group == grupos[[2]])
#   # Realizar la prueba t
#   resultados <- t.test(subgrupo1[[colname]],subgrupo2[[colname]], var.equal = FALSE)
#   return(resultados)
# }
