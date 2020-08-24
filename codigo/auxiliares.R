# Auxiliares, nomeadores, etc

format_weights_risk <- function(weight, risk){
  if (weight == "equal") {
    pesos_title <- "Pesos: Constantes"
  } else if (weight=='sharpe'){
    pesos_title <- "Pesos: Sharpe maximo"
  } else if (weight=='GMV'){
    pesos_title <- "Pesos: GMV"
  } else if (weight=='min_SD_SA_R_Market'){
    pesos_title <- "Pesos:  menor risco que atinge o retorno do mercado"
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


market_return_equally <- function(market, size='', risk){
  '
  Portfólio cujos ativos tem pesos constantes
  '
  dados_pf <- market
  desvpad  <- StdDev(
    R = dados_pf,
    portfolio_method = 'component'
  )  # Se nenhum peso é dado, ele supoe equally weighted
  returns <- Return.portfolio(
    R = dados_pf
  )
  r = Return.cumulative(returns)
  if (risk=='StdDev'){
    risco <- desvpad$StdDev
  }else if (risk == 'VaR'){  # Calculando o ETL
    var <- -ETL(
      R=returns, p=0.95, method='gaussian'
    )
    risco <- var[[1]]
  }

  result <- tibble(
    mkt_returns = r,
    mkt_risk = risco
  )
}


market_return_value <- function(market, sizes, risk){
  '
  Gera o retorno do portfólio de mercado value weighted
  Como não temos o valor de mercado das empressas em cada ponto, não usamos
  ao invés disso, usamos a market_return_equally
  '
  weights_vector <- size %>%
    subset(tickers_sa %in% colnames(market)) %>%
    mutate(pesos_efetivos = market_cap/sum(market_cap)) %>%
    select(tickers_sa, pesos_efetivos)
  rownames(weights_vector) <- weights_vector$tickers_sa
  weights_vector <- weights_vector %>%
    select(pesos_efetivos) %>%
    t()
  rownames(weights_vector) <- '2004-01-01'

  returns <- Return.portfolio(
    R = market, weights = weights_vector, rebalance_on = 'months'
  )
  r = Return.cumulative(returns)
  desvpad  <- StdDev(
    R = returns
  )
  var <- -ETL(
    R=returns, p=0.95, method='gaussian'
    )
  if (risk=='VaR'){
    metr_risco <- var[[1]]
  } else if (risk=='StdDev'){
    metr_risco <- desvpad
  }

  result <- tibble(
    mkt_returns = r,
    mkt_risk = metr_risco
  )
}


salva_tabela_dados <- function(out_path, curva_convergencia){
  '
  Gera a tabela de desvpads comparados
  Salva a tabela tabela e os dados calculados
  '
  
#  filename_tabela <- paste(
#    out_path, '/tabela', '.png', sep=""
#  )
#  gg_save(filename_tabela)
  # TODO gerar tabela
  
  # Output dos dados brutos
  filename_agregados <- paste(
    out_path, '/dados_agregados', '.csv', sep=""
  )
  dados_agregados <- curva_convergencia %>%
    select(-c(desvpads, samples)) %>%
    write.table(file = filename_agregados)

  filename_desvpads <- paste(
    out_path, '/dados_desvpads', '.csv', sep=""
  )
  sample_size <- length(curva_convergencia$desvpads[[20]])  # Descobre o samplesize de forma introspective
  for (nivel in 1:length(curva_convergencia$desvpads)){   # tentei fazer em map, falhei
    # Adiciona FALSE à lista de desvpads para que todas fiquem do mesmo tamanho e sejam comparáveis p/ tibblar
    desvpads_nivel <- curva_convergencia$desvpads[[nivel]]
    print(nivel)
    if (length(desvpads_nivel) < sample_size){
      print('corrigindo')
      false_list <- rep(FALSE, sample_size - length(desvpads_nivel))
      curva_convergencia$desvpads[[nivel]] <- c(desvpads_nivel, false_list)
    }
  }
  desvpads <- curva_convergencia$desvpads %>%
    as_tibble(.name_repair='minimal') %>%
    write.table(file = filename_desvpads)
}

