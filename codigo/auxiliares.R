# Auxiliares, nomeadores, etc

format_weights_risk <- function(weight, risk){
  if (weight == "equal") {
    pesos_title <- "Pesos: Constantes"
  } else if (weight=='sharpe'){
    pesos_title <- "Pesos: Sharpe maximo"
  } else if (weight=='GMV'){
    pesos_title <- "Pesos: GMV"
  } else if (weight=='min_SD_SA_R_Market'){
    pesos_title <- "Pesos:  minimo SD que atinge o retorno do mercado"
  } else if (weight=='value'){
    pesos_title <- "Pesos: Segundo o valor total de mercado de cada ativo"
  }
  if (risk=='StdDev'){
    risco_title <- 'Forma de Risco: desvio padrao dos retornos'
  } else if (risk == 'VaR'){
    risco_title <- 'Forma de Risco: Perda de Cauda Estimada (P=5%)'
  }
  stri_enc_toutf8(paste(risco_title, pesos_title, sep='\n'))
}


gera_subtitulo <- function(sample, timeout, weights, risk){
  '
  Auxiliar para gerar o subtítulo dos gráficos
  '
  if (risk == 'StdDev'){
    primeira_linha <- 'Medias dos desvios padroes do retorno dos portfolios calculados para cada tamanho de portfolio\n'
  } else if (risk == 'VaR'){ 
    primeira_linha <- 'Medias da perda de cauda estimada a P(5%) do retorno dos portfolios calculados para cada tamanho de portfolio\n'
    }
    if (sample){
    sample_title <- paste('Ate', as.character(sample), "portfolios calculados para cada ponto\n")
  } else {sample_title <- ""}
  if (timeout) {
    timeout_title <- paste("Tempo maximo para o cálculo de cada ponto:", as.character(timeout), " minutos\n")
  } else {timeout_title <- ""}
  pesos_risk_title <- format_weights_risk(weights, risk)
  
  subtitle <- paste(
    primeira_linha, timeout_title, sample_title, pesos_risk_title,
    sep = ''
  )
  return(stri_enc_toutf8(subtitle))
}


gera_filename <- function(out_path, ext = 'png', infos){
  '
  auxiliar para gerar os filenames
  '
  filename <- out_path
  for (info in infos){
    filename <- paste(filename, info, sep = '_')
  }
  filename <-  paste(filename, ext, sep = '.')
  return(filename)
}


market_return_value <- function(market, sizes){
  weights_vector <- size %>%
    subset(tickers_sa %in% colnames(market)) %>%
    mutate(pesos_efetivos = market_cap/sum(market_cap)) %>%
    select(tickers_sa, pesos_efetivos)
  rownames(weights_vector) <- weights_vector$tickers_sa
  weights_vector <- weights_vector %>%
    select(pesos_efetivos) %>%
    t()
  rownames(weights_vector) <- '2007-09-30'

  returns <- Return.portfolio(
    R = market, weights = weights_vector, rebalance_on = 'months'
  )
  r = Return.cumulative(returns)
  desvpad  <- StdDev(
    R = returns
  )
  result <- tibble(
    mkt_returns = r,
    mkt_desvpad = desvpad
  )
}

