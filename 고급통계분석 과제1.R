
# 고통분 과제 1 

###############################################################################

# PROBLEM 1 

set.seed(1)
n <- 200
x <- seq(0, 1, length.out = n)
y <- sin(2 * pi * x) + rnorm(n, sd = 0.15)

df <- data.frame(x = x, y = y)

write.csv(df, "data/problem1.csv", row.names = FALSE)

library(ggplot2)

# 데이터 불러오기
df <- read.csv("data/problem1.csv")

# 시각화 (산점도 + 회귀곡선)
p <- ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "회귀 곡선 적합 예시",
       x = "x",
       y = "y") +
  theme_minimal()

# 그래프 출력
print(p)

ggsave("plots/problem1_plot.png", plot = p, width = 6, height = 4, dpi = 300)


# 251015 
# 깃허브 주소 ; 
# https://github.com/LJWstat/advanced-statistics-analyzation-/commit/60933d341454a4d62f1596c810dd67dda967df15


###############################################################################

# PROBLEM 2 


# (a) 

bubble_sort_ascend = function(list){
  n=length(list)
  for (j in (n-1):1)
  {
    for (i in 1: j )
    {
      if (list[i]>list[i+1])
      {
        # 두 수 교환
        temp = list[i]
        list[i] = list[i+1]
        list[i+1] = temp
      }
    }
  }
  return(list)
}

# 예제 
set.seed(1)
x = runif(10) ; x
bubble_sort_ascend(x)

# x = runif(10) ; x
# [1] 0.26550866 0.37212390 0.57285336
# [4] 0.90820779 0.20168193 0.89838968
# [7] 0.94467527 0.66079779 0.62911404 
# [10] 0.06178627
# > bubble_sort_ascend(x)
# [1] 0.06178627 0.20168193 0.26550866 
# [4] 0.37212390 0.57285336 0.62911404 
# [7] 0.66079779 0.89838968 0.90820779 
# [10] 0.94467527 


###############################################################################

# (b) 


n1 = numeric(0)
n2 = numeric(0)
n1 = append(n1, 1)
n1 = append(n1, 2)
n2 = append(n2, 4)
n2 = append(n2, 9)
n1;n2
pivot=3
list = c(n1,pivot,n2) ; list
#--------------------------------
list = c(5,4,3,2,1,9,8,7,6) ; list


# 
# quick_sort_ascend = function(list)
# {
#   n = length(list)
#   small = numeric(0)
#   large = numeric(0)
#   for (j in 1:(n-1)) 
#   {
#     # cat("j", j )
#     pivot = list[j]
#     # cat("pivot", pivot)
#     for (i in (j+1):(n)) 
#     {
#       # cat("i", i)
#       if (list[i]<pivot)
#       {
#         small = append(small, list[i])
#       }
#       else {
#         large = append(large, list[i])} 
#     }
#     # n_s = length(small)
#     # n_l = length(large)
#     list = c(small,pivot,large)
#   } 
#   return(list)
# }
# list = c(5,4,3,2,1,9,8,7,6) ; list
# quick_sort_ascend(list)

# 문제점 : 
# 1 for (i in (j+1):(n)) 루프 안에서 if와 else 문이 실행될 때마다 small과 large가 numeric(0)으로 초기화되어버림 
# -->  for 루프 시작 전에 한 번만 초기화해야 함함
# 2 for 루프가 한 번 돌고 난 뒤에 list의 값을 변경하여 재귀 호출이 아니라, 단순히 리스트를 덮어씌워버림 
# --> 재귀호출로 해야함 
# 분할에분할에분할에...이렇게 반복해야 하는데 지금은 한방향으로만 가고 있다. 
# --> 쪼개고거기서또쪼개고 이런식으로 하려면 재귀를 써야함. 
# (smal, large의 그룹에서 다시 각각 )


quick_sort_ascend = function(list) {   
  
  #  멈추는 조건   # 리스트에 원소가 1개 이하일 때 
  if (length(list) <= 1) {
    return(list)
  }
  
  pivot = list[1] # 처음을 1로 잡으면 됨. 어차피 재귀로 쪼개고쪼갤거니까 이동할 필요업음. 반복문 필요 없음. 
  
  rest_of_list = list[-1]
  
  small = rest_of_list[rest_of_list < pivot]
  large = rest_of_list[rest_of_list >= pivot] 
  
  #  재귀 호출로 각 그룹 정렬하기
  # 핵심! small과 large 그룹을 다시 quick_sort_ascend 함수에 넣기 
  # 이 과정이 계속 반복되면서 모든 그룹이 1개 원소가 될 때까지 쪼개짐. 
  sorted_small = quick_sort_ascend(small)
  sorted_large = quick_sort_ascend(large)
  
  return(c(sorted_small, pivot, sorted_large))
}

set.seed(1)
x = runif(10)
quick_sort_ascend(x)

# quick_sort_ascend(x)
# [1] 0.06178627 0.20168193 0.26550866
# [4] 0.37212390 0.57285336 0.62911404
# [7] 0.66079779 0.89838968 0.90820779
# [10] 0.94467527

###############################################################################

# PROBLEM 3 

# 수치 미분은 극한을 이용한 도함수의 정의에서 ℎ를 작은 값을 설정하여 근사시켜 계산한다. 구체적으로 한 점 𝑥에서 함수 𝑓 의 미분을 근사하는 방법은 다음과 같다

# (a) 수치 미분함수를 구현하시오. 이때 함수의 인자로는 대상 함수 f와 미분값이 계산되는 점인 x, h 값과, method를 설정한다. 이때 h=1e-6을 디폴트 옵션으로 사용하고 method 는 “forward”, “backward”, “central” 중에서 선택할 수 있도록 구현한다. 구현한 함수를 이용해 𝑓(𝑥) = 𝑐𝑜𝑠(𝑥) − 𝑥 함수를 [0, 2𝜋]에서 미분하고, 해석적인 도함수 계산과 비교하여

mathematical_deviation = function(f, x=seq(a,b,length=100), h=1e-6, method)
{
  if (method == "forward")
    dev.f = (f(x+h)-f(x))/h
  if (method == "backward")
    dev.f = (f(x)-f(x-h))/h
  if (method == "central")
    dev.f = (f(x+h)-f(x-h))/(2*h)
  return(dev.f)
}




f_cos = function(x)
{
  return(cos(x) - x)
}

xseq=seq(0, 2*pi, length=100)

fd_f_cos = mathematical_deviation(f_cos, x=xseq, method="forward") ; fd_f_cos
bd_f_cos = mathematical_deviation(f_cos, x=xseq, method="backward") ; bd_f_cos
cd_f_cos = mathematical_deviation(f_cos, x=xseq, method="central") ; cd_f_cos


# FORWARD 방법 
# 미분전 
plot(f_cos, xlim=c(-10,10)) # xlim!! 
# 미분후
plot(x=seq(0, 2*pi, length=100), y=fd_f_cos)

# BACKWARD 방법 
# 미분전 
plot(f_cos, xlim=c(-10,10)) # xlim!! 
# 미분후
plot(x=xseq, y=bd_f_cos)

# CENTRAL 방법 
# 미분전 
plot(f_cos, xlim=c(-10,10)) # xlim!! 
# 미분후
plot(x=xseq, y=cd_f_cos)


# 해석적 방법
d_f_cos = function(x) 
{
  return(-sin(x)-1)
}

plot(x=xseq, d_f_cos(x=xseq))

# plot의 함수 그래프 개형이 거의 동일하다. 



################################################################################

# (b) 

# newton_raphson = function(f, prime=NULL, x0, maxiter=100, h=1e-6, epsilon=1e-10) # fprime 이름 통일 
# {
#   if (is.null(fprime) == TRUE)  
#     fprime = mathematical_deviation(f, h, method="central", x=xseq)
#   for (t in 1:100)
#   {
#     xseq[t] = xseq[t-1] - f(xseq[t-1]) / fprime(xseq[t-1])
#   }
#   if ( abs(xseq[t]-xseq[t-1]) < epsilon )
#     break
#   return(xseq[t])
# }


# (c) 
# 
# # 수치미분방법(fprime=NULL)
# newton_raphson(f_cos, x0=0.5)
# # error : fprime(xseq[t - 1])에서 다음과 같은 에러가 발생했습니다: 함수 "fprime"를 찾을 수 없습니다
# newton_raphson(f_cos, x0=0.5, fprime = -sin(xseq)-1)
# # error : fprime(xseq[t - 1])에서 다음과 같은 에러가 발생했습니다: 함수 "fprime"를 찾을 수 없습니다




# fprime는 함수가 아니라 벡터라서 fprime([t-1])이라고 하면 error남. 

newton_raphson = function(f, fprime=NULL, x0, maxiter=100, h=1e-6, epsilon=1e-10) 
{
  if (is.null(fprime) == TRUE)  
    fprime = mathematical_deviation(f, h, method="central", x=xseq)
  for (t in 1:100)
  {
    xseq[t] = xseq[t-1] - f(xseq[t-1]) / fprime[t-1]
    if ( abs(xseq[t]-xseq[t-1]) < epsilon )
      break # if break문은 for문 안에 있어야함!!!!!!!! 
  }
  return(xseq[t])
}



# 수치미분방법(fprime=NULL)
# > newton_raphson(f_cos, x0=0.5)
# xseq[t] <- xseq[t - 1] - f(xseq[t - 1])/fprime[t - 1]에서 다음과 같은 에러가 발생했습니다: 
#   replacement has length zero
# 
# > newton_raphson(f_cos, x0=0.5, fprime = -sin(xseq)-1)
# xseq[t] <- xseq[t - 1] - f(xseq[t - 1])/fprime[t - 1]에서 다음과 같은 에러가 발생했습니다: 
#   replacement has length zero

###############################################################################

# PROBLEM 04 

# (a) 

dev_left_rectangle = function(f, a, b, n) 
{
  h = (b-a)/n
  sum=0
  x = numeric(100)
  for ( i in 0:(n-1) )
  {
    x[i] = a + i*h
    f = f(x[i])
    sum = sum + f
  }
  return(h*sum)
}


# (b) 

dev_trapezoid = function(f,a,b,n)
{
  h = (b-a)/n 
  sum=0
  x = numeric(100)
  for ( i in 0:(n-1) )
  {
    x[i] = a + i*h
    f = f(x[i])
    sum = sum + f
  }
  return(h/2 * (f(a)+2*sum+f(b)))
}

# (c) 

# dev_simpson = function(f, a, b, n) 
# {
#   h = (b-a)/n
#   sum1=0 
#   sum2=0
#   x = numeric(100)
#   for (i in 1:(n-1)) 
#   {
#     x[i] = a +ih 
#     f = f(x[i]) # f는 이제 함수가 아니라 숫자가 된다. 
#     if (i%2 == 1) 
#       sum 1 = sum1 + f
#    else (sum2 = sum2 + f)
#   }
#   return (h/3 * (f(a) + 4*sum1 + 2*sum2 + f(b)))
# }

# 그다음 줄인 if (i%2 == 1)은 문법적으로 문제가 없지만, f(x[i])를 실행하는 시점에 이미 f가 숫자로 바뀌어 있기 때문에 R 인터프리터가 혼란을 느껴 예상하지 못한 입력입니다.라는 오류를 발생시킴.



dev_simpson = function(f, a, b, n) {
  h = (b-a)/n
  sum1 = 0
  sum2 = 0
  x = numeric(n+1) # n+1개의 원소를 가져야 함함.
  
  for (i in 0:n) { # for 루프는 0부터 n까지 돌아야 함.
    x[i+1] = a + i*h 
  }
  
  for (i in 1:(n-1)) {
    if (i%%2 == 1) {
      fx_i = f(x[i+1])
      sum1 = sum1 + fx_i # 홀수
    } else {
      fx_i = f(x[i+1])
      sum2 = sum2 + fx_i # 짝수
    }
  }
  
  result = (h/3) * (f(x[1]) + 4*sum1 + 2*sum2 + f(x[n+1]))
  return(result)
}

dev_simpson(f, 0, pi, 100) 


# (d) 
# xseq = seq(0,pi,length=100)
# 
# f = function(x){return(sin(x))}
# 
# dev_left_rectangle(f, 0, pi, 100)

# 문제 :  결과가 그냥 
# dev_left_rectangle(f, 0, pi, 100)
# numeric(0) 으로 나옴
# dev_trapezoid(f, 0, pi, 100)
# numeric(0) 이것두 



# 수정한 답안 : ------------->> 

f = function(x){return(sin(x))}


# (a) 

dev_left_rectangle = function(f, a, b, n) 
{
  h = (b-a)/n
  sum=0
  x = numeric(n+1) 
  for ( i in 1:n ) # 0 인덱스가 없으므로    
  {
    x[i] = a + (i-1) *h # f0=f(x0)이고 x0 = a+0h이지만/ x0을 어쩔수 없이 x[1]로 해야겠음
    f = f(x[i])
    sum = sum + f
  }
  return(h*sum)
}



# (b)

dev_trapezoid = function(f,a,b,n)
{
  h = (b-a)/n 
  sum=0
  x = numeric(n+1) 
  for ( i in 1:n ) # 0 인덱스가 없으므로    
  {
    x[i] = a + (i-1) *h # f0=f(x0)이고 x0 = a+0h이지만/ x0을 어쩔수 없이 x[1]로 해야겠음
    f = f(x[i])
    sum = sum + f
  }
  return(h/2 * (f(a)+2*sum+f(b))) 
}



# (c) 

dev_simpson = function(f, a, b, n) {
  h = (b-a)/n
  sum1 = 0
  sum2 = 0
  x = numeric(n+1) # n+1개의 원소를 가져야 함.
  
  for (i in 0:n) { # 이렇게 해도 됨. # for 루프는 0부터 n까지 돌아야 함 .
    x[i+1] = a + i*h 
  }
  
  for (i in 1:(n-1)) {
    if (i%%2 == 1) {       
      fx_i = f(x[i+1])
      sum1 = sum1 + fx_i # 홀수
    } else {
      fx_i = f(x[i+1])
      sum2 = sum2 + fx_i # 짝수
    }
  }
  
  # 최종 결과 계산
  result = (h/3) * (f(x[1]) + 4*sum1 + 2*sum2 + f(x[n+1]))
  return(result)
}



# (d) 

dev_left_rectangle(f, 0, pi, 100)
dev_trapezoid(f, 0, pi, 100) 
dev_simpson(f, 0, pi, 100)


# (e) 

# 해석적으로 하면-->sin(x)를 적분하면 -cos(x) 

sol = -cos(pi)-(-cos(0))

for (n in c(10,30,60,100,150,200))
{
  rect_result = dev_left_rectangle(f, 0, pi, n)
  trap_result = dev_trapezoid(f, 0, pi, n) 
  simp_result = dev_simpson(f, 0, pi, n)
  results_list[[as.character(n)]] = c(
    "Left Rectangle" = rect_result,
    "Trapezoid" = trap_result,
    "Simpson" = simp_result
  )
}
print(results_list)

plot(x=c(10,30,60,100,150,200), y=results_list)


library(ggplot2)

results_df = data.frame()

for (n in n_values) {
  result_rect = dev_left_rectangle(f, 0, pi, n)
  result_trap = dev_trapezoid(f, 0, pi, n)
  result_simp = dev_simpson(f, 0, pi, n)
  
  new_row_rect = data.frame(n = n, Algorithm = "Left Rectangle", Result = result_rect)
  new_row_trap = data.frame(n = n, Algorithm = "Trapezoid", Result = result_trap)
  new_row_simp = data.frame(n = n, Algorithm = "Simpson", Result = result_simp)
  
  results_df = rbind(results_df, new_row_rect, new_row_trap, new_row_simp)
}

ggplot(results_df, aes(x = n, y = Result, color = Algorithm)) +
  geom_line(size = 1) +  
  geom_point(size = 3) + 
  labs(
    title = "Numerical Integration Results",
    x = "Number of Intervals (n)",
    y = "Integration Result"
  ) +
  theme_minimal()



################################################################################

# PROBLEM 5 

# (a) 

A = matrix(c(4,2,2,2,5,1,2,1,3), 3)

L_t = chol(A)
L = t(L_t)
L%*%L_t ; A ; tcrossprod(L)


# (b) 

forward = function(L,b) {
  n=length(b)
  z = numeric(n)
  for (i in 1:n) {
    z[i] = 1/L[i,i] * (b[i] - sum(L[i,1:(i-1)]*z[1:(i-1)] )) # !?! 
  }
  return(z)
}

# 예시
b=c(1,-2,3)

z = forward(L,b) ; z
z = forwardsolve(L,b) ; z

# 결과가 같다. 

# (c) 

# backward = function(L,z) {
#   n = length(z)
#   x = numeric(n)
#   for (i in n:1 )
#   {
#     x[i] = 1/L[i,i] * (z[i] - sum(L[(i+1):n,i] * x[(i+1):n]))
#   }
#   return(x)
# }

backward = function(L, z) {
  n=length(z)
  x=numeric(n)
  for (i in n:1) {
    if (i == n) {
      x[i]=z[i] / L[i, i]
    } else {
      # 열과 행 위치를 바꿔야 함
      x[i] = (z[i] - sum(L[i, (i + 1):n] * x[(i + 1):n])) / L[i, i]
    }
  }
  return(x)
}




x = backward(L,z) ;x
x = backsolve(L,z) ; x


# (d)


z = forward(L,b) ; z
z = forwardsolve(L,b) ; z
x = backward(L,z) ;x
x = backsolve(L,z) ; x
solve(A,b)





# PROBLEM 06

# (a) 

gaussian_kernel = function(x1, x2,rho=1)
{
  return(exp(-rho*(x1-x2)^2))
}

# (b) 모델 만들기 

# KRR_fit = function(x1, x2, y, lambda=0.0001) x1,x2 필요없이 X(행렬)만 있으면 됨! 
# {
#   for (i in 1:n)
#   {
#     for (j in 1:n)
#     {
#       K = gaussian_kernel(x1[i], x2[j])  이렇게하면 K값이 하나만 나옴! 
#       alpha = solve(K+lambda*diag(n))%*%y  for문 바깥에 써야함/n이 지정되지않음 
#       
#       f_hat = transpose(K)%*%alpha 여기서 계산하는거 아니고 예측함수에 써야한다. 
#       
#       return (f_hat)
#     }
#   }
#   
# }


KRR_fit = function(X, y, lambda=0.0001, rho=1) 
{
  n = length(X) 
  K = matrix(0, nrow=n, ncol=n) 
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      K[i,j] = gaussian_kernel(X[i], X[j], rho)  
    }  
  }  
  alpha = solve(K+lambda*diag(n))%*%y
  # 모델 객체를 리스트로 만들어 반환 
  model = list(X=X, y=y,alpha=alpha, rho=rho)
  
  return(model)
}

# (c) 예측하기 

KRR_predict = function(model, newdata)
{
  X = model$X
  alpha = model$alpha
  rho = model$rho
  n_new = length(newdata) # newdata의 샘플 수 
  
  # newdata와 traindata 간의 커널벡터 k(x,X)계산
  k_vec = empty_vecor(n) # 또 초기화해주기...
  for (i in 1:n)
  { 
    k_vec[i] = gaussian_kernel(newdata, X[i], rho)
  }
  # 예측값 계산 
  f_hat(x) = k_vec
  
}




predict.krr = function(object, newdata) {
  X_train = object$X
  alpha = object$alpha
  rho = object$rho
  
  n_new = length(newdata)
  
  predictions = numeric(n_new)
  
  for (i in 1:n_new) {
    k_vec = sapply(X_train, function(x_i) gaussian_kernel(newdata[i], x_i, rho = rho))
    
    predictions[i] = t(k_vec) %*% alpha
  }
  
  return(predictions)
}
