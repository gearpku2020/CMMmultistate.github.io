library(tidyverse)
library(readxl)
library(parallel)

setwd("D:/...")
# Load data
read.csv("msebmt_all.csv")->msebmt

allres = data.frame()

#### calculate PPV, NPV, Specificity, Sensitivity-----
for(i in 1:5){
  startmodel<-Sys.time()
  print(paste0("Start running model ", i,", please be patient..."))
  e1 <- filter(msebmt,trans==i)
  e <- e1
  nuu <- c('kdm_advance','phenoage_advance','frailty_3q', 'score2_impu', 'fhs_impu')
  modelres = data.frame()
  for(name in nuu){
    fit <- glm(status~e[,name]+sex+age,data=e,family="binomial")
    e$fit <- fit$fitted.values
    length(unique(e$fit))
    newx=sort(c(unique(e$fit),max(e$fit,na.rm=TRUE)+1))

    cl <- makeCluster(19)
    clusterExport(cl,'e');


    dt <- parLapply(cl, newx, function(cut){
      res=table(e$fit>=cut, e$status)
      if(nrow(res)==1){
        res1=matrix(c(0,0),nrow=1)
        temp=setdiff(c("TRUE","FALSE"),attr(res,"dimnames")[[1]][1])
        if(temp=="FALSE") res=rbind(res1,res)
        else res=rbind(res,res1)}

      sens=res[2,2]/sum(res[,2])
      spec=res[1,1]/sum(res[,1])
      ppv=res[2,2]/sum(res[2,])
      npv=res[1,1]/sum(res[1,])
      df = data.frame(x=cut,sens=sens,spec=spec,fpr=1-spec,ppv=ppv,npv=npv,sum=sens+spec)
      return(df)
    })
    stopCluster(cl)

    result <- bind_rows(dt)

    no=which.max(result$sum)
    result$x[no]
    paste("Sens:",sprintf("%03.1f",result[no,]$sens*100),"%\n",
          "Spec:",sprintf("%03.1f",result[no,]$spec*100),"%\n",
          "PPV:",sprintf("%03.1f",result[no,]$ppv*100),"%\n",
          "NPV:",sprintf("%03.1f",result[no,]$npv*100),"%\n",
          sep="")

    modelres = bind_rows(modelres, result[no,])
  }
  modeltime = Sys.time() - startmodel
  allres = bind_rows(allres, modelres)
  print(paste0("Model ", i, " runs successfully! It takes about ", round(as.numeric(modeltime, units = "hours"), 3)," hours."))
}


allres %>%
  mutate(trans = rep(paste0("trans",1:5),each = 5),
         nume = rep(nume,5)) %>%
  relocate(trans, .before = x) %>%
  relocate(nume, .before = x) -> allres1


###### for C-index #####---------------------------------
allindex = data.frame()
allname <- c("kdm_advance","phenoage_advance", 'fhs_impu', 'score2_impu',"frailty_3q")
for(j in 1:5){
  start_time<-Sys.time()
  print(paste0("Start running model ", j,", please be patient..."))
  msebmt1 <-  filter(msebmt,trans==j)
  modelres = data.frame()
  for(i in 1:length(allname)){
    print(paste0("Model ", j, " for ", allname[i], " is running..."))
    the.expression_allname <-  paste0('fit_allname <- coxph(Surv(time, status)~ ',
                                      allname[i],
                                      "+sex+age, data = msebmt1)"  )#+sex+age +BMI+healthy_alcohol+smoking_status+educ+healthy_PA+Ethnic+employment_fac+income_fac


    eval(parse(text = the.expression_allname)) # run the expression
    results_allname <- summary(fit_allname)
    parameter_allname <- data.frame(name =allname[i],
                                    beta= results_allname$coefficients[1,2],
                                    lower=results_allname$conf.int[1,3],
                                    upper=results_allname$conf.int[1,4],
                                    se=results_allname$coefficients[1,3],
                                    p=results_allname$coefficients[1,5],
                                    cindex_H=results_allname$concordance[1],
                                    cindexse_H=results_allname$concordance[2],
                                    cindex_U = coef(concordance(fit_allname,timewt="n/G2", ranks=TRUE)),
                                    cindexse_U=sqrt(vcov(concordance(fit_allname,timewt="n/G2", ranks=TRUE)))

    )
    modelres = bind_rows(modelres, parameter_allname)
  }
  modeltime = Sys.time() - start_time
  allindex = bind_rows(allindex, modelres)
  print(paste0("Model ", j, " runs successfully! It takes about ", round(as.numeric(modeltime, units = "mins"), 3)," minutes."))
}

allindex %>%
  mutate(trans = rep(paste0("trans",1:5),each = 5)) %>%
  relocate(trans, .before = name) %>%
  as.data.frame() -> allindex1


