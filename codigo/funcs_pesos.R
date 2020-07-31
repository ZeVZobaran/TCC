# FUnções para formas "exóticas" de pesagem de portfólio

source('auxiliares.R')
# Equally weighted


#pf <- c("ABEV3.SA", 'PETR3.SA', 'BBAS3.SA')
#t = gen_value(market, pf, risk='VaR', size=size)
#pf = info_levels$samples[[2]][[1]]


gen_equally <- function(market, pf, risk, size=''){
  '
  Portfólio cujos ativos tem pesos constantes
  '
  dados_pf <- market[, pf]
  desvpad  <- StdDev(
    R = dados_pf,
    portfolio_method = 'component'
  )  # Se nenhum peso é dado, ele supoe equally weighted
  returns <- Return.portfolio(
    R = dados_pf
  )

  if (risk=='StdDev'){
    desvpad$StdDev
  }else if (risk == 'VaR'){  # Calculando o ETL
     var <- -ETL(
       R=returns, p=0.95, method='gaussian'
     )
     var[[1]]
  }
}

# Value
gen_value <- function(market, pf, risk, size){
  '
  Portfólios cujos ativos são pesados segundo o
  market cap das empresas que ele contem
  '
  # Com só uma açao e full investment o peso é sempre 1
  # Passamos direto para a gen_equally por economia de processamento
  if (length(pf) == 1){
    return(gen_equally(market, pf, risk, size))
  }
  dados_pf <- (market[, pf])
  weights_vector <- size %>%
    subset(tickers_sa %in% pf) %>%
    mutate(pesos_efetivos = market_cap/sum(market_cap)) %>%
    select(tickers_sa, pesos_efetivos)
  rownames(weights_vector) <- weights_vector$tickers_sa
  weights_vector <- weights_vector %>%
    select(pesos_efetivos) %>%
    t()
  rownames(weights_vector) <- '2007-09-30'
  
#  desvpad  <- StdDev(
#    R = dados_pf, weights = weights_vector, portfolio_method = 'component'
#  )
  returns <- Return.portfolio(
    R = dados_pf, weights = weights_vector, rebalance_on = 'months'
  )
  r = Return.cumulative(returns)
  desvpad <- StdDev(
    R = returns
    )

  if (risk=='StdDev'){
    desvpad
  }else if (risk == 'VaR'){  # Calculando o VaR na mão
    var <- -ETL(
      R=returns, p=0.95, method='gaussian'
    )
    var[[1]]
  }
}


# min_SD_SA_R_Market
gen_minSD_SA_RMarket <- function(market, pf, risk, size){
  'Portfólios cujos pesos satisfazem o problema
  min(Risco) S.A retorno=retorno do mercado
  '
  #TODO entender direito pq não atingimos nunca r
  # Com só uma açao e full investment o peso é sempre 1
  # Passamos direto para a gen_equally por economia de processamento
  if (length(pf) == 1){
    return(gen_equally(market, pf, risk, size))
  }
  #HACK no meio tempo do TODO acima, ficamos com o R observado do Ibov
  R = 0.015  # Retono do IBOV no período
  # Retorno do mercado value weighted
  r <- market_return_value(market, size, risk)$mkt_returns
  
  dados_pf <- (market[, pf])
  pf_spec <- portfolio.spec(pf)
  pf_spec <- add.constraint(
    portfolio = pf_spec,
    type = "full_investment"
  )
  pf_spec <- add.constraint(
    portfolio = pf_spec,
    type = "long_only"
    )
  pf_spec <- add.constraint(
    portfolio = pf_spec,
    type = "return",
    return_target = r
    )
  if (risk=="StdDev") {
    pf_spec <- add.objective(
      portfolio = pf_spec,
      type = "risk",
      name = "StdDev"
      )
  } else if (risk=="VaR"){
    pf_spec <- add.objective(
      portfolio = pf_spec,
      type = "risk",
      name = "ETL",
      arguments = list(p=0.95)
    )
    
  }
  opt_pf <- optimize.portfolio(
    market,
    portfolio=pf_spec,
    optimize_method="ROI",
    trace=TRUE
    )
  opt_pf$objective_measures[[1]]
}


# GMV

gen_GMV <- function(market, pf, risk, size=''){
  '
  Portfólios de variância mínima global (GMV)
  '
  # Com só uma açao e full investment o peso é sempre 1
  # Passamos direto para a gen_equally por economia de processamento
  if (length(pf) == 1){
    return(gen_equally(market, pf, risk, size))
  }
  dados_pf <- (market[, pf])
  pf_spec <- portfolio.spec(pf)
  pf_spec <- add.constraint(
    portfolio = pf_spec,
    type = "full_investment"
  )
  pf_spec <- add.constraint(
    portfolio = pf_spec,
    type = "long_only"
  )

  if (risk=="StdDev") {
    pf_spec <- add.objective(
      portfolio = pf_spec,
      type = "risk",
      name = "StdDev"
    )
  } else if (risk=="VaR"){
    pf_spec <- add.objective(
      portfolio = pf_spec,
      type = "risk",
      name = "ETL",
      arguments = list(p=0.95)
    )
  }

  opt_pf <- optimize.portfolio(
    market,
    portfolio=pf_spec,
    optimize_method="ROI",
    trace=TRUE
  )
  opt_pf$objective_measures[[1]]
}

# Sharpe

gen_sharpe <- function(market, pf, risk, size=''){
  '
  Portfólios que maximizam o Sharpe (utilização de risco)
  '
  # Com só uma açao e full investment o peso é sempre 1
  # Passamos direto para a gen_equally por economia de processamento
  if (length(pf) == 1){
    return(gen_equally(market, pf, risk, size))
  }
  dados_pf <- (market[, pf])
  pf_spec <- portfolio.spec(pf)
  pf_spec <- add.constraint(
    portfolio = pf_spec,
    type = "full_investment"
  )
  pf_spec <- add.constraint(
    portfolio = pf_spec,
    type = "long_only"
  )
  if (risk=="StdDev") {
    pf_spec <- add.objective(
      portfolio = pf_spec,
      type = "risk",
      name = "StdDev"
    )
  } else if (risk=="VaR"){
    pf_spec <- add.objective(
      portfolio = pf_spec,
      type = "risk",
      name = "ETL",
      arguments = list(p=0.95)
    )
  }
  pf_spec <- add.objective(
    portfolio = pf_spec,
    type = "return",
    name = "mean"
  )
  
  opt_pf <- optimize.portfolio(
    market,
    portfolio=pf_spec,
    optimize_method="ROI",
    maxSR="TRUE",
    trace=TRUE
  )
  opt_pf$objective_measures[[1]]
}

