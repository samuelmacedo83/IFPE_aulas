
assimetria <-  function(vet){
  
  resp <- list(assimetria = 0,
               classificacao = "simétrico")
  n <- length(vet)
  sd_pop <- sqrt((n-1)/n) * sd(vet)
  # quartis e decis
  medidas <- medidas_posicao(vet)
  
  a <- 3 * (mean(vet) - medidas$q2) / sd_pop
  resp$assimetria = a
  
  if (a >= 1) {
    resp$classificacao = "assimétrico a direita"
  } 
  
  if (a > 0.5 & a < 1){
    resp$classificacao = "levemente assimétrico a direita"
  }
      
  if (a <= -1){
    resp$classificacao = "assimétrico a esquerda" 
  }
  
  if (a < -0.5 & a > -1) {
    resp$classificacao = "levemente assimétrico a esquerda"
  }
  resp
}

curtose <- function(vet){
  
  resp <- list(curtose = 0,
               classificacao = "mesocúrtico")
  
  # quartis e decis
  medidas <- medidas_posicao(vet)
  
  # calculo da curtose
  k <- (medidas$q3 - medidas$q1) / (2 * (medidas$d9 - medidas$d1))
  resp$curtose = k
  
  # classificando
  if (k > 0.27) {
    resp$classificacao = "platicúrtico"} 
  if (k < 0.25){
    resp$classificacao = "leptocúrtico"}
  
  resp
}


medidas_posicao <- function(vet){
  
  n <- length(vet)
  vet <- sort(vet) # ordenando
  
  # lista de saida
  resp <- list(q1 = 0, q2 = 0, q3 = 0,
               d1 = 0, d9 = 0)
  
  # posicoes dos quantis
  q1_pos = n * 0.25
  q2_pos = n * 0.5
  q3_pos = n * 0.75
  d1_pos = n * 0.1
  d9_pos = n * 0.9
  
  resp$q1 <- quantis(vet, q1_pos)
  resp$q2 <- median(vet)
  resp$q3 <- quantis(vet, q3_pos)
  resp$d1 <- quantis(vet, d1_pos)
  resp$d9 <- quantis(vet, d9_pos)
  
resp
}

quantis <- function(vet, posicao){
  
  if (ceiling(posicao) == floor(posicao)){
    quantil <- vet[posicao]
  } else {
    quantil <- 0.5 * vet[ceiling(posicao)] + 0.5 * vet[floor(posicao)]
  }
  
  quantil
}
