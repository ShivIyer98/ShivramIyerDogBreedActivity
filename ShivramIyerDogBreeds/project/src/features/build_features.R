# libraries ---------------------------------------------------------------

library(data.table)

set.seed(1)

# probability of answers depending on party -------------------------------
# D:democrats, R:Republican

# approve Trump as a president
Ap_Pres <- c(0,1)
D_Q1  <- c(0.93,0.07)
R_Q1  <- c(0.11,0.89)
#https://news.gallup.com/poll/203198/presidential-approval-ratings-donald-trump.aspx

# universial background checks (gun laws)
UBC <- c(0,1)
D_Q2  <- c(0.05,0.95)
R_Q2  <- c(0.15,0.85)
#https://www.washingtonpost.com/politics/americans-of-both-parties-overwhelmingly-support-red-flag-laws-expanded-gun-background-checks-washington-post-abc-news-poll-finds/2019/09/08/97208916-ca75-11e9-a4f3-c081a126de70_story.html

# right to choose (abortion)
R2C <- c(0,1)
D_Q3  <- c(0.25,0.75)
R_Q3  <- c(0.65,0.35)
#https://fivethirtyeight.com/features/the-abortion-debate-isnt-as-partisan-as-politicians-make-it-seem/

# government supported health care
GSHC <- c(0,1)
D_Q4  <- c(0.49,0.51)
R_Q4  <- c(0.88,0.12) 
#https://www.pewresearch.org/fact-tank/2018/10/03/most-continue-to-say-ensuring-health-care-coverage-is-governments-responsibility/

# trust Fox as a news source
Fox <- c(0,1)
D_Q5  <- c(0.58,0.42)
R_Q5  <- c(0.30,0.70)
#https://www.businessinsider.com/most-and-least-trusted-news-outlets-in-america-cnn-fox-news-new-york-times-2019-4

# trust CNN as a news source
CNN <- c(0,1)
D_Q6  <- c(0.18,0.82)
R_Q6  <- c(0.70,0.30)
#https://www.businessinsider.com/most-and-least-trusted-news-outlets-in-america-cnn-fox-news-new-york-times-2019-4

# lowering immigration rate
Lower_Immigration <- c(0,1)
D_Q7  <- c(0.80,0.20) 
R_Q7  <- c(0.40,0.60)
#https://news.gallup.com/opinion/polling-matters/215210/partisan-differences-growing-number-issues.aspx

# consider global warming as a significant problem
Glob_W <- c(0,1)
D_Q8  <- c(0.11,0.89)
R_Q8  <- c(0.60,0.40)
#https://news.gallup.com/opinion/polling-matters/215210/partisan-differences-growing-number-issues.aspx

# increase tax rate for higher income
Tax_high_income <- c(0,1)
D_Q9  <- c(0.18,0.82)
R_Q9  <- c(0.60,0.40)
#https://news.gallup.com/opinion/polling-matters/215210/partisan-differences-growing-number-issues.aspx

# death penalty
Death_Pen <- c(0,1)
D_Q10 <- c(0.59,0.41)
R_Q10 <- c(0.20,0.80)
#https://news.gallup.com/opinion/polling-matters/215210/partisan-differences-growing-number-issues.aspx

# federal government has too much power
Fed_too_much_Power <- c(0,1)
D_Q11 <- c(0.64,0.36)
R_Q11 <- c(0.18,0.82)
#https://news.gallup.com/opinion/polling-matters/215210/partisan-differences-growing-number-issues.aspx

# make gun laws stricter
Stricter_Gun_Laws <- c(0,1)
D_Q12 <- c(0.23,0.77)
R_Q12 <- c(0.66,0.34)
#https://news.gallup.com/opinion/polling-matters/215210/partisan-differences-growing-number-issues.aspx

# make environmental laws stricter
Stricter_Env_laws <- c(0,1)
D_Q13 <- c(0.21,0.79)
R_Q13 <- c(0.65,0.35) 
#https://news.gallup.com/opinion/polling-matters/215210/partisan-differences-growing-number-issues.aspx

# make dataset ------------------------------------------------------------

party <- c("R","D")
Qs <- c("Ap_Pres","UBC","R2C","GSHC","Fox","CNN","Lower_Immigration","Glob_W","Tax_high_income","Death_Pen","Fed_too_much_Power","Stricter_Gun_Laws","Stricter_Env_laws")

data <- NULL

for (j in 1:length(party)){
  sp_tab <- NULL
  for (i in 1:length(Qs)){
    msats <- sample(get(Qs[i]),5000,replace=T,prob=get(paste0(party[j],"_Q",i)))
    sp_tab <- cbind(sp_tab,msats)
    sp_tab <- data.table(sp_tab)
    names(sp_tab)[i] <- Qs[i]
  }
  sp_tab$party <- party[j]
  data <- rbind(data,sp_tab)
}

# add variables [Cats] and [Knitting] -------------------------------------

### [Cats] is not associated with party at all
data$Cats <- sample(c(0,1),nrow(data),replace=T)

### [Knitting] is associated with [Cats]
data$Knitting <- 0
data[Cats==1]$Knitting <- sample(c(0,1),nrow(data[Cats==1]),replace=T,prob=c(0.15,0.85))
data[Cats==0]$Knitting <- sample(c(0,1),nrow(data[Cats==0]),replace=T,prob=c(0.85,0.15))

# order variabless and save data ------------------------------------------

data$id <- paste0('sample_',1:nrow(data))
data <- data[,.(id,party,Ap_Pres,UBC,R2C,GSHC,Fox,CNN,Lower_Immigration,Glob_W,Tax_high_income,Death_Pen,Fed_too_much_Power,Stricter_Gun_Laws,Stricter_Env_laws,Cats,Knitting)]
fwrite(data,"./project/volume/data/raw/data.csv")
