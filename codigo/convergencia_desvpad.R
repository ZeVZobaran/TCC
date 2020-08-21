library(xlsx)
library(tidyverse)
library(tidyquant)
library(ggpubr)
library(gridExtra)
library(ROI)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(stringi)
source('funcs_pesos.R')
source('auxiliares.R')
# SE EU ESTIVER AQUI DEU BOM

normality_by_moments <- function(sample, sig = 1){
  'Testes de normalidade como o Shapiro e o JB falham em amostras muito grandes
   Visto que qualquer desvio da curtose ou simetria ficam significantes
   Ao invés de usar o teste inteiro, criamos uma alternativa
   que checa não se excess kurt e skew são == 0, mas sim se são "pequenos o bastante"
   "o bastante" é definido como menor do que sig em módulo
   sig é settado pra 1
  '
  if (length(sample) < 500){
    shapiro_val = shapiro.test(sample)[[2]]
    result <- (shapiro_val > 0.05)
    return(result)
  }  # Se a amostra for pequena o bastante, basta rodar o shapiro
  excess_kurt <- abs(kurtosis(sample, na.rm = TRUE))
  excess_skew <- abs(skewness(sample, na.rm = TRUE))
  result <- (excess_kurt < sig)&(excess_kurt < sig)
  return(result)
}


get_stats_benchmark(assets){
  '
  recebe uma lista de assets
  retorna o tempo estimado para passa-la na gera_stats_pf e gera_stats_nivel
  '
  #TODO
}


# Processamento de dados

gera_samples <- function(market, timeout=False, constant_sample_size=False){
  '
  Recebe o timeout desejado (em minutos) e um zoo com os dados de retorno do mercado
  Retorna as samples de portfolio por nível, respeitando o timeout
  Se constant_sample_size == TRUE, ao invés disso usa a sample size minima que respeita o timeout
  (Ou seja, a de mercado/2) e retorna samples desse tamanho para todos os níveis
  se constant_sample_size for definido, ignora o timeout e faz todos com a sample pedida
  '
  tickers <- colnames(market)
  mkt_size <- ncol(market) # Tamanho do mercado
  info_levels <- tibble(
    level = numeric(),
    max = numeric(),
    sampled = numeric(),
    pct_sampled = numeric(),
    samples = list(),
    timeout = numeric()
  )  # Container para as samples geradas no for abaixo
  for (n in {1:mkt_size}){
    print(n)
    max_pfs <- choose(mkt_size, n)  # Número máximo de pfs possíveis nesse nível
    last_samples <- NULL  # Última sample criada. Instanciada como NULL
    estimated_time <- 0  # Instancia como 0 caso timeout não seja passado
    
    # Se sample size for passado logo de cara, não precisamo do for
    # Escolhemos direto o sample size (max pfs se a passada for maior do que
    # os pofs possíveis no nível) e já retornamos as samples.
    if (is.numeric(constant_sample_size)){
      sample_size <- min(c(constant_sample_size, max_pfs))
      pfs_in_sample = sample_size
      last_samples <- replicate(pfs_in_sample, sort(sample(tickers, size=n))) %>%
        as_tibble()
      # Sort logo na base pra que tudo que for feito esteja sempre em ordem alfabética
      # Faz com que não se precise sortar em outros lugares
      if (n==1){last_samples <- t(last_samples)}
    } else { # TODO, benchmarking ainda não implementado
      pfs_in_sample <- 1  # Quantos pfs vamos criar para passar no benchmark
      while (pfs_in_sample<max_pfs){
        sampled_pfs <- replicate(pfs_in_sample, sample(tickers, size=n)) %>%
          as_tibble()
        if (n==1){sampled_pfs <- t(sampled_pfs)}
        estimated_time <- get_stats_benchmark(sampled_pfs)
        if (estimated_time>timeout){
          break
        }
        last_samples <- sampled_pfs
        # FIXME TODA ESSA SESSÂO NÃO ESTÁ IMPLEMENTADA
      }
      pfs_in_sample <- pfs_in_sample+1
    }
    info_levels <- add_row(info_levels,
                           level = n,
                           max = max_pfs,
                           sampled = pfs_in_sample,
                           pct_sampled = pfs_in_sample/max_pfs,
                           samples = list(last_samples),
                           timeout = estimated_time
    ) # Adiciona o gerado ao df de saída
  }
  return(info_levels)
}


gera_stats_sample <- function(sample, market, weights, risk, size, return=FALSE){
  '
  recebe uma sample de pfs (só os tickers das ações) e o mercado,
  gera as estatísticas de cada pf e da sample como um todo e outputa
  '
  level <- length(sample[[1]]) # Descobre qual é o nível (p/ o output)
  print(level)
  
  if (weights=='equal'){
    func_weights <- gen_equally
  } else if (weights=='min_SD_SA_R_Market'){
    func_weights <- gen_minSD_SA_RMarket
  } else if (weights == 'GMV'){
    func_weights <- gen_GMV
  } else if (weights == 'sharpe'){
    func_weights <- gen_sharpe
  } else if (weights == 'value'){
    func_weights <- gen_value # TODO
  }  # Selector da função para calcular pesos

  level_desvpads <- tibble(
    desvpads = numeric()  # Tibble de saída dos desvpads
  )

  for (pf in sample){
    desvpad <- func_weights(market, pf, risk, size)
    level_desvpads = add_row(
      level_desvpads,
      desvpads = desvpad
      )
    }

  mean_desvpad <- mean(level_desvpads$desvpads, na.rm = TRUE)
  sd_desvpads <- sqrt(var(level_desvpads$desvpads, na.rm = TRUE))
  p025_desvpad <- qnorm(0.025, mean = mean_desvpad, sd = sd_desvpads)
  p975_desvpad <- qnorm(0.975, mean = mean_desvpad, sd = sd_desvpads)
  # Normalidade
  safe_normality <- safely(normality_by_moments)
  norm_check <- safe_normality(level_desvpads$desvpads)
  if (is.null(norm_check$error)){
    normal <- (norm_check$result)
  } else {
    normal <- FALSE
  }  # No nível 37 não dá pra gerar normal (só um pf); precisa de um workaround
  # TODO Adicionar retornos/outputar pfs ótimos?
  stats_nível <- tibble(
    level = level,
    weights = weights,
    desvpads = list(level_desvpads$desvpads),
    mean_desvpad = mean_desvpad,
    p025_desvpad = p025_desvpad,
    p975_desvpad = p975_desvpad,
    sd_desvpads = sd_desvpads,
    normal = normal
  ) # weights - tipo de calculo de peso (por enquanto só euql weighted)
  # normal - TRUE se passa no teste de shapiro, else FALSE 
}


gera_pontos <- function(info_levels, market, weights, risk, size){
  '
  Recebe todas as samples, retorna uma tibble com o df para a curva
  Nele, cada linha é um nível (ou seja, uma quantidade de ações)
  usa a gera_stats como uma geradore de rows pra tibble final
  info_levels deve ser o output de gera_samples
  '
  curva_convergencia <- map(
    info_levels$samples, gera_stats_sample,
    market, weights, risk,  size) %>%
    # Passa cada nível pela gera_stats_sample
    bind_rows() %>%
    left_join(info_levels, by = 'level')
  return(curva_convergencia)
}

# Plotters

plota_curva <- function(
  market, curva_convergencia, weights, size, risk, out_path = TRUE,
  sample = FALSE, timeout = FALSE, ext = 'png', save = TRUE
){
  '
  Recebe o output de gera_stats_sample, gera e outputa a curva de convergência
  Se save, salva. Se não, printa
  '
  out_path <- file.path(
    out_path, paste(as.character(sample), 'portfólios_calculados', sep='_')
    )
  dir.create(out_path, showWarnings = FALSE)
  if (risk == 'StdDev'){
    tipo_risco <- 'Desvio Padrão'
  } else if (risk == 'VaR'){
    tipo_risco <- 'Estimated Tail Loss'
  }
  forma_risco_mercado <- 'pesos segundo o o valor de cada ativos'
  mkt_risk <- market_return_value(market, size, risk)$mkt_risk
  if (weights == 'equal'){
    mkt_risk <- tail(curva_convergencia$mean_desvpad, 1)
    forma_risco_mercado <- 'pesos constantes'
    
  }  # Caso particular - No equal, queremos comparar com o risco máximo em PFs iguals
  # Visto que não é propriamente uma técnica de otimização de PF
  # Nos outros casos, comparamos com o risco do PF value-weighted, que é mais próximo
  # da forma de composição da maior parte dos índices
  subtitulo <- gera_subtitulo(sample, timeout, weights, risk)
  data_graph <- curva_convergencia %>% # Tranformações para o gráfico ficar mais bonito
    mutate(
      normal_text = ifelse(
        normal,
        'Excesso de curtose e assimetria < 1 em módulo',
        'Excesso de curtose e assimetria > 1 em módulo'
        ),
      amostral_text = ifelse((pct_sampled==1),
                             'Ponto calculado a partir da população completa de portfólios possíveis',
                             'Ponto e intervalos calculados por amostragem'),
      p025_amostral = ifelse((pct_sampled==1), NA, p025_desvpad),
      p975_amostral = ifelse((pct_sampled==1), NA, p975_desvpad),
      upper_sd =  ifelse((pct_sampled==1), NA, mean_desvpad + sd_desvpads),
      lower_sd =  ifelse((pct_sampled==1), NA, mean_desvpad - sd_desvpads),
    )
  # Cria o gráfico da curva de convergência
  ggplot(data = data_graph, mapping = aes(x = level)) + 
    geom_point(mapping = aes(y = mean_desvpad, color = normal_text, shape = amostral_text)) +
    geom_line(mapping = aes(y = p025_amostral, linetype = "P[0.025, 0.975]")) +
    geom_line(mapping = aes(y = p975_amostral, linetype = "P[0.025, 0.975]")) +
    geom_ribbon(mapping = aes(
      ymax = (upper_sd),
      ymin = (lower_sd),
      fill = '±1SD'
    ),
    alpha = 0.3) +
    geom_hline(mapping = aes(
      yintercept = mkt_risk, linetype = paste(
        tipo_risco[[1]], ' do mercado, ',
        forma_risco_mercado, sep="")
    )) +
    labs(x = "Tamanho do portfólio (Quantidade de ações contidas)", y = tipo_risco[[1]]) +
    labs(title = paste(
      "Convergência do ", 
      tipo_risco[[1]], 
      " de portfólios aleatórios para o do mercado",
      sep=""
      )) +
    labs(subtitle = subtitulo) +
    labs(color = NULL) +
    labs(shape = NULL) +
    labs(fill = NULL) +
    labs(linetype = NULL) +
    labs(line='t') +
    theme_minimal()
  filename <- file.path(
    out_path, paste('curva_convergencia.', ext, sep="")
    )
  ggsave(filename, width=450, height=200, units = 'mm')
  salva_tabela_dados(
  filename, curva_convergencia
  )
}


graficos_normalidade <- function(samples, out_path, weight, risk, ext='.png'){
  '
  Recebe uma lista de samples, retorna o qq, fdp e fpa dela e salva
  Usado pra gerar os gráficos de cada nível da curva
  Cria os gráficos individuais e os gráficos em um lattice unificado
  '
  if (risk == 'StdDev'){
    tipo_risco <- c('Desvio Padrão', 'Desvios Padrões')
  } else if (risk == 'VaR'){
    tipo_risco <- c('Value at Risk', 'VaRs dos portfólios')
  }
  level <- 1
  for (sample in samples){
    sample_name <- paste(
      as.character(length(sample)),
      'portfólios_calculados/',
      sep = '_'
      )
    nivel_name <- paste('nivel', as.character(level), sep = '_')
    out_path_nivel = file.path(out_path, nivel_name, sample_name)
    dir.create(file.path(out_path, nivel_name))
    dir.create(out_path_nivel, showWarnings = FALSE)
    # Filenames
    filename_qq <- paste(out_path_nivel, '/qq_plot', ext, sep = '')
    filename_fdp <- paste(out_path_nivel, '/fdp_plot', ext, sep = '')
    filename_fpa <- paste(out_path_nivel, '/fpa_plot', ext, sep = '')
    filename_lattice <- paste(out_path_nivel, '/comp', ext, sep = '')
    #TODO arrumar nome dos eixos dos gráficos e os títulos
    qq <- ggqqplot(sample) + 
      labs(x = "Distribuição Teórica", y = 'Distribuição Amostral') +
      labs(title = paste("Quantis Amostrais vs Normais\n")) 
    ggsave(filename_qq, width=400, height=200, units = 'mm')
    
    fpa <- ggdensity(sample) + 
      labs(x = tipo_risco[[2]], y = 'Densidade') +
      labs(title = paste(
        "Distribuição de Probabilidade dos", tipo_risco[[2]], "\n"
        ))
    ggsave(filename_fdp, width=400, height=200, units = 'mm')
    
    cdf <- ggecdf(sample) + 
      labs(x = "Desvio Padrão Observado", y = '% das Observações') +
      labs(title = paste(
        "Função de Probabilidade Acumulada dos Desvios Padrões \n"))
    ggsave(filename_fpa, width=400, height=200, units = 'mm')
    
    pesos_risk_title <- format_weights_risk(weight, risk)
    ggsave(filename_lattice,
           grid.arrange(cdf, fpa, qq, nrow=3,
                        top=grid::textGrob(paste(
                          'Informações de Normalidade\n',
                          as.character(length(sample)),
                          ' portfólios calculados; ',
                          as.character(level),
                          ' ações por portfólio\n',
                          pesos_risk_title,
                          sep = ''
                          ),
                          x = 0.01, hjust = 0,
                          gp = grid::gpar(fontsize = 14, fontface = 'bold'))
                        ),
           width=210, height=297, units = 'mm'
    )
    
    level <- level + 1

  }
}


# Teste sem timeout:
load("C:/Users/josez/Desktop/Economia/FEA/TCC/dados/IbovData_2.RDS") # Dados do IBOV já tydied
market <- IBOV_Returns_Final
size <- IBOV_Data %>%  # Processamentos básicos com size para os PFs valueweighted
  mutate(market_cap = Qtde * share) %>%
  mutate(tickers_sa = paste(Ticker, '.SA', sep=""))

sample_sizes <- c(500, 5000, 50000)  # Rodando o cheio, 500 e 50k
#sample_sizes <- c(5)
weights <- c('value', 'equal', 'sharpe', 'GMV', 'min_SD_SA_R_Market')
risks <- c('StdDev', 'VaR')
out_path <- 'C:/Users/josez/Desktop/Economia/FEA/TCC/graficos'

for (sample_size in sample_sizes){
  print(sample_size)
  info_levels <- gera_samples(
    market = market,
    timeout = FALSE,
    constant_sample_size = sample_size
  )
  for (risk in risks){
    for (weight in weights){
      print(
        paste(
          'Rodando para ', as.character(sample_size), ' portfólios por nível',
          sep = ""
        )
      )
      print(paste("Métrica de risco: ", risk, sep = ""))
      print(paste('Algoritmo de pesos: ', weight, sep = ""))
      desvpads_por_nivel <- gera_pontos(
        info_levels = info_levels,
        market = market,
        weights = weight,
        risk = risk,
        size = size
      )

      out_path_weight <- file.path(out_path, risk, weight)
      plota_curva(
        market = market,
        curva_convergencia = desvpads_por_nivel,
        sample = sample_size,
        size = size,
        out_path = file.path(out_path_weight, 'curva_convergencia/'),
        weights = weight,
        risk = risk
      )
      
      graficos_normalidade(
        samples = desvpads_por_nivel$desvpads,
        out_path = file.path(out_path_weight, 'normalidade'),
        weight = weight, risk = risk
      )
    }
  }
}
# TODO adicionar aleatorização do T0
