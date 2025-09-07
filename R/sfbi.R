# function for smooth boyce index - from Liu et al. (2024)
sfbi <- function(prd1, prd0, ktry=10) {
  p <- c(prd1, prd0)
  n1 <- length(prd1)
  n0 <- length(prd0)
  prd <- seq(min(p), max(p), length=n0)
  oc <- c(rep(1, n1), rep(0, n0))
  
  md_tp = mgcv::gam(oc ~ s(p,bs="tp",k=min(ktry,length(unique(p)))), family=binomial)
  prd_tp = predict(md_tp,newdata=data.frame(p=prd),type='response')
  md_cr = mgcv::gam(oc ~ s(p,bs="cr",k=min(ktry,length(unique(p)))), family=binomial)
  prd_cr = predict(md_cr,newdata=data.frame(p=prd),type='response')
  md_bs = mgcv::gam(oc ~ s(p,bs="bs",k=min(ktry,length(unique(p)))), family=binomial)
  prd_bs = predict(md_bs,newdata=data.frame(p=prd),type='response')
  md_ps = mgcv::gam(oc ~ s(p,bs="ps",k=min(ktry,length(unique(p)))), family=binomial)
  prd_ps = predict(md_ps,newdata=data.frame(p=prd),type='response')
  md_ad = mgcv::gam(oc ~ s(p, bs = "ad",k=min(ktry,length(unique(p)))), family=binomial)
  prd_ad = predict(md_ad,newdata=data.frame(p=prd),type='response')
  prd_m = (prd_tp + prd_cr + prd_bs + prd_ps + prd_ad)/5
  
  SBI_m <- cor(prd,prd_m,method="spearman")
  
  return(SBI_m)
}
