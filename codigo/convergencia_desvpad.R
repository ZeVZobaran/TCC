library(xlsx)
library(tidyverse)
library(ggpubr)
# Auxiliares

gera_subtitulo <- function(sample, timeout, weights){
  '
  Auxiliar para gerar o subtítulo dos gráficos
  '
  if (sample){
    sample_title <- paste(as.character(sample), "portifólios simulados para cada ponto;")
  } else {sample_title <- ""}
  if (timeout) {
    timeout_title <- paste("Tempo máximo para o cálculo de cada ponto:", as.character(timeout), " minutos; ")
  } else {timeout_title <- ""}
  if (weights == "equal") {
    pesos_title <- "Estrutura de pesos: constantes"
  } else {pesos_title <- ""}
  subtitle <- paste(timeout_title, sample_title, pesos_title)
  return(subtitle)
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


normality_by_moments <- function(sample, sig = 1){
  '  Testes de normalidade como o Shapiro e o JB falham em amostras muito grandes
   Visto que qualquer esvio da curtose ou simetria ficam significantes
   Ao invés de usar o teste inteiro, criamos uma alternativa
   que checa não se kurt e skew são == 0, mas sim se são "pequenos o bastante"
   "o bastante" é definido como menor do que sig em módulo
   sig é settado pra 1
  '
  if ((length(sample)) < 500){
    shapiro_val = shapiro.test(sample)
    result <- (shapiro_val > 0.05)
    return(result)
  }  # Se a amostra for pequena o bastante, basta rodar o shapiro
  excess_kurt <- abs(kurtosis(sample))
  excess_skew <- abs(skewness(sample))
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
      last_samples <- replicate(pfs_in_sample, sample(tickers, size=n)) %>%
        as_tibble()
      if (n==1){last_samples <- t(last_samples)}
    } 
    else { # TODO, benchmarking ainda não implementado
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
        pfs_in_sample <- pfs_in_sample+1
      }
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


gera_stats_sample <- function(sample, market, weights = 'equal', return=FALSE){
  '
  recebe uma sample de pfs (só os tickers das ações) e o mercado,
  gera as estatísticas de cada pf e da sample como um todo e outputa
  '
  level <- length(sample[[1]]) # Descobre qual é o nível (p/ o output)
  print(level)
  if (weights=='equal'){
    weights_vector <- rep(1/level, level)
  } #TODO Pensar em outras formas de pesagem (em particular, value e optimal)
  level_desvpads <- tibble(
    desvpads = numeric()
  )
    for (pf in sample){
    covvar <- as.matrix(market[, pf]) %>%
      cov() # Para cada portfolio samplado, gera a matriz de covvar e o desvpad do pf (abaixo)
    desvpad <-sqrt(t(weights_vector)%*%covvar%*%weights_vector)
    level_desvpads = add_row(level_desvpads,
                             desvpads=desvpad
    )
  } # Adiciona o desvpad do pf ao container
  mean_desvpad <- mean(level_desvpads$desvpads)
  sd_desvpads <- sqrt(var(level_desvpads$desvpads))
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


gera_pontos <- function(info_levels, market){
  '
  Recebe todas as samples, retorna uma tibble com o df para a curva
  Nele, cada linha é um nível (ou seja, uma quantidade de ações)
  usa a gera_stats como uma geradore de rows pra tibble final
  info_levels deve ser o output de gera_samples
  '
  curva_convergencia <- map(info_levels$samples, gera_stats_sample, market) %>%
    # Passa cada nível pela gera_stats_sample
    bind_rows() %>%
    left_join(info_levels, by = 'level')
  return(curva_convergencia)
}



# Plotters

plota_curva <- function(
  market, curva_convergencia, out_path = TRUE, weights = 'equal',
  sample = FALSE, timeout = FALSE, ext = 'pdf', save = TRUE
  ){
  '
  Recebe o outrput de gera_stats_sample, gera e outputa a curva de convergência
  Se save, salva. Se não, printa
  '
  mkt_desvpad = tail(curva_convergencia$mean_desvpad, n=1) 
  subtitulo <- gera_subtitulo(sample, timeout, weights)
  ggplot(data = curva_convergencia, mapping = aes(x = level)) + 
    geom_point(mapping = aes(y = mean_desvpad, color = normal)) +
    geom_line(mapping = aes(y = p025_desvpad, linetype = "P[0.025, 0.975]")) +
    geom_line(mapping = aes(y = p975_desvpad, linetype = "P[0.025, 0.975]")) +
    geom_ribbon(mapping = aes(
      ymax = (mean_desvpad + sd_desvpads),
      ymin = (mean_desvpad - sd_desvpads),
      fill = '±1SD'
      ),
      alpha = 0.3) +
    geom_hline(mapping = aes(
      yintercept = mkt_desvpad, linetype = 'Desvio padrão do mercado'
      )) +
    labs(x = "Quantidade de ações no portifólio", y = 'Desvio Padrão') +
    labs(title = "Convergência do desvio padrão de portifólios aleatórios para o do mercado") +
    labs(subtitle = subtitulo) +
    labs(color = 'Kurt e Assym < 1 em módulo') +
    labs(fill = NULL) +
    labs(linetype = NULL) +
    labs(line='t') +
    theme_minimal()
  # TODO salvar resultado
  if (save){
    filename <- gera_filename(
    out_path, ext, list(as.character(sample), 'timeout', as.character(timeout))
    )
    ggsave(filename)
    }
}


graficos_normalidade <- function(samples, out_path, ext='pdf'){
  '
  Recebe uma lista de samples, retorna o qq, fdp e fpa dela e salva
  Usado pra gerar os gráficos de cada nível da curva
  '
  level <- 1
  for (sample in samples){
    nivel_name = paste('nivel', as.character(level), sep = '_')
    out_path_nivel = file.path(out_path, nivel_name)
    out_path_nivel = paste(out_path_nivel, '/', sep='')
    dir.create(out_path_nivel, showWarnings = FALSE)
    ggqqplot(sample)
    filename_qq <- gera_filename(
      out_path_nivel, ext, list('qq', as.character(length(sample)))
    )
    ggsave(filename_qq)

    ggdensity(sample)
    filename_fdp <- gera_filename(
      out_path_nivel, ext, list('fdp', as.character(length(sample)))
    )
    ggsave(filename_fdp)
    
    
    ggecdf(sample)
    filename_cdf <- gera_filename(
      out_path_nivel, ext, list('cdf', as.character(length(sample)))
    )
    ggsave(filename_cdf)
    
    level <- level + 1
    # TODO outputtar a tibble em RDS
  }
}



# Teste sem timeout:
load("C:/Users/josez/Desktop/Economia/FEA/TCC/dados/IbovData.RDS") # Dados do IBOV já tydied
market <- IBOV_Returns_Final
sample_size <- 5000
out_path <- 'C:/Users/josez/Desktop/Economia/FEA/TCC/graficos'

info_levels <- gera_samples(
  market = market,
  timeout = FALSE,
  constant_sample_size = sample_size
)

desvpads_por_nivel <- gera_pontos(
  info_levels = info_levels,
  market = market
)

plota_curva(
  market = market,
  curva_convergencia = desvpads_por_nivel,
  sample = sample_size,
  out_path = file.path(out_path, 'curva_convergencia/curva_convergencia')
)

graficos_normalidade(
  samples = desvpads_por_nivel$desvpads,
  out_path = file.path(out_path, 'normalidade')
)
