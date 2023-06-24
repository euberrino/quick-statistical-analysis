library(ggplot2)
library(ggpubr)
library(RVAideMemoire)
library(nortest)
library(dplyr) 
library(car)


# ---------------- Leer csv ----------------

read_file = function(datapath,header,sep){
  file = read.csv(datapath,header = header, sep= sep)
  return(file)
}

# -------------- tipos de datos -----------

data_type = function(df){
  columnas = names(data)
  data_types = sapply(df, class)
  dfo <- data.frame(Variable = columnas, TipoDato = data_types)
  write.csv(dfo, '/Users/malenaalvarezrottemberg/Desktop/ITBA/BIOESTADISTICA/output_df.csv')
  return(dfo)
}


# ---------- NAs  ------------
# Armar una funci?n que devuelva en formato dataframe
# la cantidad de NAs presentes por columna 

na_counts = function(df){
  matrix_na = is.na(df) 
  valores_faltantes = colSums(matrix_na) 
  dframe = as.data.frame(valores_faltantes)
  return(dframe)
}

# ------- Convertir a factor -----------
convert_to_factor = function(df,colname){
  df[[colname]] = as.factor(df[[colname]])
  return(df)
}

# ------- Histograma --------
histogram = function(df,groupcol,colname,inpbins){
  columna = df[[colname]]
  group_col = df[[groupcol]]
  columna_numerica = as.numeric(columna) 
  if(groupcol == 'No agrupar'){ 
    return(ggplot(df, aes(x = columna_numerica)) + geom_histogram(alpha = 0.5, position = "identity", bins = inpbins))
  }
  else{
    return(ggplot(df, aes(x = columna_numerica, fill = group_col, colour = group_col)) + geom_histogram(alpha = 0.5, position = "identity", bins = inpbins))
    }
}
# ------ BOX PLOT --------

my_boxplot = function(df,colname,groupcol){
  df <- na.omit(df)
  columna = df[[colname]]
  columna_numerica = as.numeric(columna)
  group_col = df[[groupcol]]
  
  if(groupcol == 'No agrupar'){
    return(ggplot(data=df, aes_string(x=columna_numerica, fill=group_col, color=group_col)) +
              geom_boxplot() + coord_flip() + stat_boxplot(geom = "errorbar",width = 0.25) )
  }
  else{
    return(ggplot(df, aes(x = group_col, y =columna_numerica, color=group_col)) + geom_boxplot())
  }
}


#------- QQ PLOT --------
my_qqplot = function(df,colname,groupcol){
  df <- na.omit(df)
  columna = df[[colname]]
  columna_numerica = as.numeric(columna)
  group_col = df[[groupcol]]
  
  if(groupcol == 'No agrupar'){
    grafico <- qqnorm(columna, pch = 1, frame = FALSE)
    linea <- qqline(columna, col = "steelblue", lwd = 2)
    return(grafico)
    return(linea)
  }
  else{
    grafico <- ggplot(df, aes(sample = columna_numerica, color=factor(group_col))) + stat_qq() + stat_qq_line()
    return(grafico)
  }
}

# ----------- SHAPIRO _ WILK ---------------
shapiro_w = function(df,colname,groupcol){
  if (groupcol == "No agrupar") {
    return(shapiro.test(df[[colname]]))
  }
  else{
    df_resultados = data.frame(Test = character(),Grupo = numeric(),Estadistico = numeric(),Pvalor=numeric())
    fila_nueva = data.frame(Test = character(),Grupo = numeric(),Estadistico = numeric(),Pvalor=numeric())
    columna_numerica = df[[colname]]
    columna_group = as.factor(df[[groupcol]])
    grupos <- unique(columna_group)
    resultados <- vector("list", length(grupos))
    for (i in seq_along(grupos)) {
      subgrupo <- subset(df,columna_group == grupos[i])
      resultados[[i]] <- shapiro.test(subgrupo[[colname]])
      fila_nueva = data.frame(Test = resultados[[i]]$method,Grupo = i,Estadistico = resultados[[i]]$statistic,Pvalor=resultados[[i]]$p.value)
      df_resultados <- rbind(df_resultados, fila_nueva)
    }
    return(df_resultados)
  }
}


# --------- Lilliefors -------------
lilliefors = function(df,colname,groupcol){
  if (groupcol == "No agrupar") {
    return(lillie.test(df[[colname]]))
  }
  else{
    df_resultados = data.frame(Test = character(),Grupo = numeric(),Estadistico = numeric(),Pvalor=numeric())
    fila_nueva = data.frame(Test = character(),Grupo = numeric(),Estadistico = numeric(),Pvalor=numeric())
    columna_numerica = df[[colname]]
    columna_group = as.factor(df[[groupcol]])
    grupos <- unique(columna_group)
    resultados <- vector("list", length(grupos))
    for (i in seq_along(grupos)) {
      subgrupo <- subset(df,columna_group == grupos[i])
      resultados[[i]] <- lillie.test(subgrupo[[colname]])
      fila_nueva = data.frame(Test = resultados[[i]]$method,Grupo = i,Estadistico = resultados[[i]]$statistic,Pvalor=resultados[[i]]$p.value)
      df_resultados <- rbind(df_resultados, fila_nueva)
    }
    return(df_resultados)
  }
}


# --------- Test de Levene ---------
levene = function(dataf, nombrecol, groupocol){
    columna_numerica = dataf[[nombrecol]]
    columna_group = as.factor(dataf[[groupocol]])
    data <- data.frame(columna_numerica,columna_group)
    test = leveneTest(data$columna_numerica,data$columna_group)
    return(test)
  }
# ------ Test ... ---------
my_test = function(df,colname,groupcol){
  df <- na.omit(df)
  #columna = df[[colname]]
  #columna_numerica = as.numeric(columna)
  columna_group = as.factor(df[[groupcol]])
  grupos <- unique(columna_group)
  #resultados <- vector("list", length(grupos))
  subgrupo1 <- subset(df,columna_group == grupos[[1]])
  subgrupo2 <- subset(df,columna_group == grupos[[2]])
  # Realizar la prueba t
  resultados <- t.test(subgrupo1[[colname]],subgrupo2[[colname]], var.equal = FALSE)
  return(resultados)
}
