gerar_sequencia <- function(qtd_num, 
                            target_assimetria,
                            target_curtose,
                            inicio = 1)
{
  
  probs <- rep(1/qtd_num, qtd_num)
  sequencia <- sample(inicio:(inicio + qtd_num - 1),
                      size = qtd_num,
                      prob = probs)
  
  a <- assimetria(sequencia)
  k <- curtose(sequencia)
  a_target <- target_assimetria
  k_target <- target_curtose
  
  dist_abs_a <- abs(a_target - a$assimetria)
  dist_abs_k <- abs(k_target - k$curtose)
  dist_abs <- 0.2*dist_abs_a + dist_abs_k
  
  
  for(i in 1:50){
    p <- runif(qtd_num)
    p <- p / sum(p)
    probs_t <- 0.5 * p + 0.5 * probs
    sequencia_t <- sample(inicio:(inicio + qtd_num - 1),
                          size = qtd_num,
                          prob = probs_t,
                          replace = TRUE)
    
    a_t <- assimetria(sequencia_t)
    k_t <- curtose(sequencia_t)
    
    dist_abs_a_t <- abs(a_target - a_t$assimetria)
    dist_abs_k_t <- abs(k_target - k_t$curtose)
    dist_abs_t <- 0.5 * dist_abs_a_t + dist_abs_k_t
    
    if (dist_abs_t < dist_abs){
      probs <- probs_t
      a <- a_t
      k <- k_t
      dist_abs <- dist_abs_t 
      sequencia <-sequencia_t
    }
    
    
  }
  
  medidas <- medidas_posicao(sequencia)
  
  
  estatisticas <- c("N",
                    "Média",
                    "Desvio-Padrão", 
                    "Mediana",
                    "Assimetria",
                    "Class_assimetria",
                    "Q1","Q3","D1","D9",
                    "Curtose",
                    "Class_curtose",
                    "DIQ")
  valores <- c(length(sequencia),
               round(mean(sequencia), 2),
               round(sqrt((qtd_num-1)/qtd_num) * sd(sequencia), 2),
               medidas$q2,
               round(a$assimetria,2),
               a$classificacao,
               medidas$q1, medidas$q3, medidas$d1, medidas$d9,
               round(k$curtose,2),
               k$classificacao,
               medidas$q3 - medidas$q1
               )
  

  result <- list(sequencia = sequencia,
                 estatisticas_descritivas = data.frame(estatisticas, valores))
  result
  
}



