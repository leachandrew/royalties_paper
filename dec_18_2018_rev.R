####### LEACH-BOSKOVIC-MASON Royalty simulations DEC 2018 #######
#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
setwd("/Users/aleach/Google Drive/Papers/Royalties")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/Papers/Royalties")
print(getwd())

print(getwd())
source("../../andrew_base.R")


#specific to this project

library(nleqslv)
library(numDeriv)



#reserve discovery function
ee_cost_exp <- 3
ee_cost_base <- 50
ee_cost_mult <- .00000001
ee_cost_scale<-1
ee_cost_shift<-1




ee_cost_exp <- 3
ee_cost_base <- 50
ee_cost_mult <- .000005

#lines(reserves,.001*2^(reserves/200-2),col="red")
  
#### BASE MODEL ###
reserves<-(c(0:200)/200)^(2)*2000+.01



#extensive margin - extraction cost 

ext_cost_exp <- 4
ext_cost_mult <- 50
ext_cost_base <- 12
ext_cost_res_factor <- 1/180
extract<-c(0:30)






#POLICY PARAMETERS

res_carb_tax<-0
ext_carb_tax<-30
cons_carb_tax<-0 #impact of consumption carbon taxes on oil prices
net_rev_rate<-0.1
gross_rev_rate<-0

oba_rate<-0.8 #percentage of average eibbls returned via OBA
oba_val<-30 #value of OBA credits, in $/tonne

#oba_rate<-0 #percentage of average eibbls returned via OBA
#oba_val<-0 #value of OBA credits, in $/tonne


#fixed GHG parameters
eibbls<-0.55
incidence<-0.5

#abatement costs
tac_1<-250
tac_exp_b<- 2
tac_exp_cc<- 6
tac_exp_cv<-1.15  




#here, we need to make a scenarios table
oil_price<<-45
abate_curve<-"abate_base"

price_vec<-c(80)
carbon_taxes<-c(50)
tax_type<-c("extraction","reserves")
royalties<-c(0,0.3)
roy_type<-c("net","gross")
oba_rates<-c(0,0.8)

abate_curves<-c("abate_base","abate_convex")

#define scenarios

#1) base case
scenario_base<-c(1,price_vec[1],0,"extraction",0,"gross",0,"abate_base")
dim(scenario_base) <- c(1,8)
scenarios<-data.frame(scenario_base,stringsAsFactors = F)
#2) carbon tax on extraction
scen_num<-2
scenario_new<-c(scen_num,price_vec[1],50,"extraction",0,"gross",0,"abate_base")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#3) carbon tax on reserves
scen_num<-3
scenario_new<-c(scen_num,price_vec[1],50,"reserves",0,"gross",0,"abate_base")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#4) carbon tax on extraction w 80% OBA
scen_num<-4
scenario_new<-c(scen_num,price_vec[1],50,"extraction",0,"gross",0.8,"abate_base")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#5) carbon tax on extraction with gross revenue royalties
scen_num<-5
scenario_new<-c(scen_num,price_vec[1],50,"extraction",0.3,"gross",0,"abate_base")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#6) carbon tax on extraction with net revenue royalties
scen_num<-6
scenario_new<-c(scen_num,price_vec[1],50,"extraction",0.3,"net",0,"abate_base")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#7) carbon tax on extraction with convext abatement cost
scen_num<-7
scenario_new<-c(scen_num,price_vec[1],50,"extraction",0,"gross",0,"abate_convex")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#8) carbon tax on extraction with concave abatement cost
scen_num<-8
scenario_new<-c(scen_num,price_vec[1],50,"extraction",0,"gross",0,"abate_concave")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#9) no carbon tax, gross revenue royalties
scen_num<-9
scenario_new<-c(scen_num,price_vec[1],0,"extraction",0.3,"gross",0,"abate_base")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new
#10) no carbon tax, net revenue royalties
scen_num<-10
scenario_new<-c(scen_num,price_vec[1],0,"extraction",0.3,"net",0,"abate_base")
dim(scenario_new) <- c(1,8)
scenarios[scen_num,]<-scenario_new





names(scenarios)<-c("Number","Oil Price","Carbon Tax","Tax Type","Royalty","Base","OBA","MAC_TYPE")
scenarios$`Oil Price` <- as.numeric(as.character(scenarios$`Oil Price`))
scenarios$`Carbon Tax` <- as.numeric(as.character(scenarios$`Carbon Tax`))
scenarios$`Royalty` <- as.numeric(as.character(scenarios$`Royalty`))
scenarios$Number <- as.numeric(as.character(scenarios$Number))
scenarios$OBA <- as.numeric(as.character(scenarios$OBA))





scenario_string2<-function(s,fig_num){ #fignum lets me set different labels for different figures
  subsample<-scenarios[s,]  
  return(cat(paste("Scenario ",s,": Oil Price $",subsample$`Oil Price`,"/bbl, Carbon Price $",subsample$`Carbon Tax`,"/t on ",subsample$`Tax Type`,sep=""),
             paste("Royalties applied on ",subsample$Base," revenues",", output-based allocations at ",(subsample$OBA)*100,"% of average cost",sep=""),
             paste("Abatement cost curve: ",subsample$`MAC_TYPE`,sep=""), 
             sep="\n"))
}


scenario_string<-function(s,fig_num){ #fignum lets me set different labels for different figures
  subsample<-scenarios[s,]  
  return(paste("Sc #",s,": Oil $",subsample$`Oil Price`,"/bbl, CTax $",subsample$`Carbon Tax`,"/t on ",subsample$`Tax Type`,", ",subsample$`Royalty`*100,"% ",subsample$Base," revenue royalties",", OBAs at ",(subsample$OBA)*100,"% of avg cost, MAC ",subsample$`MAC_TYPE`,sep=""))
}


scenario_string_clip<-function(s,fig_num,col_sent){ #fignum lets me set different labels for different figures
  subsample<-scenarios[s,]  
  subsample$Royalty<-subsample$Royalty*100
  subsample$OBA <-subsample$OBA*100
  levels(subsample$MAC_TYPE) <- list(Linear="abate_base", Convex="abate_convex", Concave="abate_concave")
  lead_str_vec<-c("Oil price: $","$", "", "Royalty rate: " ,   "Royalty base: "  ,     "OBA rate: "  , "")
  follow_str_vec<-c("/bbl","/t on ", ".", "%" ,   " revenues"  ,     "%"  , " MAC")
  test<-cbind(lead_str_vec,t(subsample[seq(2,8)]),follow_str_vec)
  test<-test[(col_sent[seq(2,8)]>0),]
  seps<-lead_str_vec[(col_sent[seq(2,8)]>0)]
  #seps[]<-". "
  #seps[length(seps)]<-"."
  #print(subsample$MAC_TYPE)
  #paste("Scenario ",s,": ",paste(c(t(cbind(test,seps))), collapse=''),sep="")
  paste(paste(c(t(cbind(test,seps))), collapse=''),sep="")
}


reserve_cost <- function(bbls) {
  #send barrels in billions, get costs in dollars
  cost <- ee_cost_base+ee_cost_mult * bbls^(ee_cost_exp/ee_cost_scale)+bbls*res_carb_tax*eibbls
  
    return(cost)
}

marg_reserve_cost <- function(bbls) {
  #send barrels in billions, get costs in dollars
  #cost<- ee_cost_base*ee_cost_mult^(bbls/ee_cost_scale-ee_cost_shift)+res_carb_tax*eibbls
  
  cost <- cost <- ee_cost_exp * ee_cost_mult * bbls^(ee_cost_exp-1)+res_carb_tax*eibbls
  return(cost)
}




Fig1<-function(file_sent,width_sent=1200,height_sent=750,file_exp){
  #Figure 1
  par(mfrow=c(1,1))
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,2,2))
  legend_vec<-vector()
  col_vec<-vector()
  old_carb_tax<-res_carb_tax
  res_carb_tax<<-30
  y_range<-range(0,marg_reserve_cost(reserves),marg_reserve_cost(reserves)-res_carb_tax*eibbls)#all extraction columns
  x_range<-range(reserves)
  plot(x_range,y_range,type="n",lwd=2,xlab="Total barrels of proved reserves (millions)",ylab="Marginal cost per barrel of reserves created (2017 USD)",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
  #axis(side=2, at = pretty(range(y_range)))
  lines(reserves,marg_reserve_cost(reserves),type="l",lwd=2)
  legend_vec<-cbind(legend_vec,c("With Carbon Tax"))
  col_vec<-cbind(col_vec,c("black"))
  res_carb_tax<<-0
  lines(reserves,(marg_reserve_cost(reserves)),col="grey60",type="l",lwd=3)
  legend_vec<-cbind(legend_vec,c("Without Carbon Tax"))
  res_carb_tax<<-old_carb_tax
  col_vec<-cbind(col_vec,c("grey60"))
  legend("topleft",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=1)
  if(file_exp==1)
    dev.off()
  }

Fig1("reserve_cost.png",file_exp = 1)


#set output based allocation on the basis of barrels
oba<- function(bbls) {
oba_rate*eibbls  
}



#function for net total carbon costs, including impact of output-based allocations
tot_tax<- function(ghgs,bbls) {
  ext_carb_tax*ghgs-oba(bbls)*oba_val*bbls
}


#function for marginal change in net total carbon costs with change in bbls
dtax_dbbls<- function(ghgs,bbls) {
  -oba(bbls)*oba_val*bbls^0 #use bbls^0 to get the length
}


#function for marginal change in net total carbon costs with change in ghgs
dtax_dghgs<- function(ghgs,bbls) {
  ext_carb_tax*bbls^0 #use bbls^0 to get the length
}



#here type is abate_base unless specified
total_abate_cost <- function(ghgs,bbls,type="abate_base") {
  #set them so they are equal at the minimum of ei=eibbls
  if(type=="abate_base")
    tac_exp<-tac_exp_b
  if(type=="abate_convex")
    tac_exp<-tac_exp_cv
  if(type=="abate_concave")
    tac_exp<-tac_exp_cc
  ei<-ghgs/bbls
  min_ghg<-eibbls*bbls
  c_shift<- (tac_1-tac_1/tac_exp)*eibbls*bbls
  cost <- -tac_1*ghgs+tac_1*eibbls*bbls/tac_exp*(ei/eibbls)^(tac_exp)+c_shift
  return(cost)
}




#calculate marginal change abatement costs for a change in emissions
#here type is abate_base unless specified
dac_dghgs <- function(ghgs,bbls,type="abate_base") {
  if(type=="abate_base")
    tac_exp<-tac_exp_b
  if(type=="abate_convex")
    tac_exp<-tac_exp_cv
  if(type=="abate_concave")
    tac_exp<-tac_exp_cc
  #print(ghgs)
  ei<-ghgs/bbls
  cost <- tac_1-tac_1*(ei/eibbls)^(tac_exp-1)
  return(cost)
}


Fig_TAC<-function(file_sent,width_sent,height_sent,file_exp){
  temp_ext<-c(0:30)
  temp_ghgs<-c(0:200)/200
  par(mfrow=c(1,1))
  y_range<-range(0,1500)#all extraction columns
  x_range<-range(0,1.2*eibbls*15)
  legend_vec<-vector()
  old<-ext_carb_tax
  ext_carb_tax<<-30
  col_vec<-vector()
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,5,2))
  plot(x_range,y_range,type="n",lwd=2,xlab="Emissions",ylab="Abatement Costs ($)",main="Emissions Abatement Costs",cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
  #lines(temp_ghgs,(15*eibbls-temp_ghgs)/(15*eibbls),col="black",type="l",lwd=6)
  ghgs<-temp_ghgs*30*eibbls
  lines(ghgs,total_abate_cost(ghgs,15,type="abate_base"),col="black",type="l",lwd=6)
  lines(ghgs,ghgs*0,col="yellow",type="l",lwd=6)
  #print(total_abate_cost(ghgs,15,type="abate_base"))
  legend_vec<-cbind(legend_vec,c("Base"))
  col_vec<-cbind(col_vec,c("black"))
  lines(ghgs,total_abate_cost(ghgs,15,type="abate_concave"),col="grey90",type="l",lwd=6)
  legend_vec<-cbind(legend_vec,c("Concave"))
  col_vec<-cbind(col_vec,c("grey90"))
  lines(ghgs,total_abate_cost(ghgs,15,type="abate_convex"),col="grey60",type="l",lwd=6)
  legend_vec<-cbind(legend_vec,c("Convex"))
  col_vec<-cbind(col_vec,c("grey60"))
  ext_carb_tax<<-old
  legend("topright",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=2)
  
  if(file_exp==1)
    dev.off()
}

Fig_TAC("TAC.png",1400,750,file_exp=0)




#calculate marginal change in abatement costs for a change in bbls
#here type is abate_base unless specified
dac_dbbls <- function(ghgs,bbls,type="abate_base") {
  #set them so they are equal at the minimum of ei=eibbls
  if(type=="abate_base")
    tac_exp<-tac_exp_b
  if(type=="abate_convex")
    tac_exp<-tac_exp_cv
  if(type=="abate_concave")
    tac_exp<-tac_exp_cc
   cost <- tac_1 * eibbls/tac_exp * (ghgs/bbls/eibbls)^(tac_exp) - tac_1 * eibbls * 
      bbls/tac_exp * ((ghgs/bbls/eibbls)^((tac_exp) - 1) * ((tac_exp) * (ghgs/bbls^2/eibbls))) + 
      (tac_1 - tac_1/tac_exp) * eibbls
  return(cost)
}



Fig_dbbl<-function(file_sent,width_sent,height_sent,file_exp){
  temp_ext<-c(10:30)
  par(mfrow=c(1,1))
  y_range<-range(0,150)#all extraction columns
  x_range<-range(10,30)
  legend_vec<-vector()
  old<-ext_carb_tax
  ext_carb_tax<<-30
  col_vec<-vector()
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,5,2))
  plot(x_range,y_range,type="n",lwd=2,xlab="Extraction",ylab="Change in Abatement Costs ($)",main="Emissions Abatement Costs",cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
  #lines(temp_ghgs,(15*eibbls-temp_ghgs)/(15*eibbls),col="black",type="l",lwd=6)
  lines(temp_ext,dac_dbbls(8.5,temp_ext,type="abate_base"),col="black",type="l",lwd=6)
  #lines(temp_ext,temp_ext*0,col="yellow",type="l",lwd=6)
  #print(total_abate_cost(ghgs,15,type="abate_base"))
  legend_vec<-cbind(legend_vec,c("Base"))
  col_vec<-cbind(col_vec,c("black"))
  lines(temp_ext,dac_dbbls(8.5,temp_ext,type="abate_concave"),col="grey90",type="l",lwd=6)
  legend_vec<-cbind(legend_vec,c("Concave"))
  col_vec<-cbind(col_vec,c("grey90"))
  lines(temp_ext,dac_dbbls(8.5,temp_ext,type="abate_convex"),col="grey60",type="l",lwd=6)
  legend_vec<-cbind(legend_vec,c("Convex"))
  col_vec<-cbind(col_vec,c("grey60"))
  ext_carb_tax<<-old
  legend("topright",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=2)
  
  if(file_exp==1)
    dev.off()
}

Fig_dbbl("dbbl.png",1400,750,file_exp=0)




Fig_TACbbl<-function(file_sent,width_sent,height_sent,file_exp){
  temp_ext<-c(15:30)
  par(mfrow=c(1,1))
  y_range<-range(0,1000)#all extraction columns
  x_range<-range(15,30)
  legend_vec<-vector()
  old<-ext_carb_tax
  ext_carb_tax<<-30
  col_vec<-vector()
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,5,2))
  plot(x_range,y_range,type="n",lwd=2,xlab="Extraction",ylab="Total Abatement Costs ($)",main="Emissions Abatement Costs",cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
  #lines(temp_ghgs,(15*eibbls-temp_ghgs)/(15*eibbls),col="black",type="l",lwd=6)
  lines(temp_ext,total_abate_cost(8.5,temp_ext,type="abate_base"),col="black",type="l",lwd=6)
  #lines(temp_ext,temp_ext*0,col="yellow",type="l",lwd=6)
  #print(total_abate_cost(ghgs,15,type="abate_base"))
  legend_vec<-cbind(legend_vec,c("base"))
  col_vec<-cbind(col_vec,c("black"))
  lines(temp_ext,total_abate_cost(8.5,temp_ext,type="abate_concave"),col="grey90",type="l",lwd=6)
  legend_vec<-cbind(legend_vec,c("Concave"))
  col_vec<-cbind(col_vec,c("grey90"))
  lines(temp_ext,total_abate_cost(8.5,temp_ext,type="abate_convex"),col="grey60",type="l",lwd=6)
  legend_vec<-cbind(legend_vec,c("Convex"))
  col_vec<-cbind(col_vec,c("grey60"))
  ext_carb_tax<<-old
  legend("topleft",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=2)
  
  if(file_exp==1)
    dev.off()
}

Fig_TACbbl("dbbl.png",1400,750,file_exp=0)




Fig_test<-function(file_sent,width_sent,height_sent,file_exp){
  cex_scale<-1.25
  temp_ext<-c(0:30)
  temp_ghgs<-c(0:200)/200
  par(mfrow=c(1,1))
  y_range<-range(0,250)#all extraction columns
  x_range<-range(0,1.2*eibbls*15)
  legend_vec<-vector()
  old<-ext_carb_tax
  ext_carb_tax<<-30
  col_vec<-vector()
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,2,2))
  plot(x_range,y_range,type="n",lwd=2,xlab="Emissions",ylab="Marginal Abatement Costs ($/tonne)",cex.lab=cex_scale, cex.axis=cex_scale, cex.main=cex_scale, cex.sub=cex_scale)
  #lines(temp_ghgs,(15*eibbls-temp_ghgs)/(15*eibbls),col="black",type="l",lwd=6)
  ghgs<-temp_ghgs*30*eibbls
  lines(ghgs,dac_dghgs(ghgs,15,type="abate_base"),col="black",type="l",lwd=3)
  legend_vec<-cbind(legend_vec,c("Base"))
  col_vec<-cbind(col_vec,c("black"))
  lines(ghgs,dac_dghgs(ghgs,15,type="abate_concave"),col="grey60",type="l",lwd=3)
  legend_vec<-cbind(legend_vec,c("Concave"))
  col_vec<-cbind(col_vec,c("grey60"))
  lines(ghgs,dac_dghgs(ghgs,15,type="abate_convex"),col="grey30",type="l",lwd=3)
  legend_vec<-cbind(legend_vec,c("Convex"))
  col_vec<-cbind(col_vec,c("grey30"))
  ext_carb_tax<<-old
  legend("topright",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=1.25)
  
  if(file_exp==1)
    dev.off()
}

Fig_test("test_cost.png",1200,750,file_exp=1)



#extraction costs

tot_ext_cost <- function(ghgs,bbls,res,abate_type="abate_base") {
  #send barrels in billions, get costs in dollars
  cost <- ext_cost_base*(bbls) + ext_cost_mult * (bbls)^(ext_cost_exp)*ext_cost_res_factor/(res+1)+tot_tax(ghgs,bbls)+total_abate_cost(ghgs,bbls,abate_type)
  #cost<-reserves*opt_ext^2
  return(cost)
}

marg_ext_cost <- function(ghgs,bbls,res,abate_type="abate_base") {
  #send barrels in billions, get costs in dollars
  cost <- ext_cost_base+ ext_cost_exp*ext_cost_mult*(bbls)^(ext_cost_exp-1)*ext_cost_res_factor/(res+1)+dac_dbbls(ghgs,bbls,abate_type)+dtax_dbbls(ghgs,bbls)
  #print(ghgs)
  #cost <- ext_cost_base+ ext_cost_exp*ext_cost_mult*(bbls)^(ext_cost_exp-1)*ext_cost_res_factor/(res+1)
  return(cost)
}


#ADD CARBON TAX AND ROYALTIES HERE, PRESUMABLY AS FUNCTIONS tot_roy and marg_roy, functions of
#barrels and price?


Fig2<-function(file_sent,width_sent,height_sent,file_exp){
  temp_res<-reserves+10
  par(mfrow=c(1,1))
  y_range<-range(0,2*tot_ext_cost(10*eibbls,10,temp_res,abate_curve)/10)#all extraction columns
  x_range<-range(temp_res)
  legend_vec<-vector()
  old<-ext_carb_tax
  ext_carb_tax<<-30
  col_vec<-vector()
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,5,2))
  plot(x_range,y_range,type="n",lwd=2,xlab="Reserves (Millions of Barrels)",ylab="Average Extraction Cost ($/bbl at 10 mmbbl/yr)",cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
  lines(temp_res,tot_ext_cost(10*eibbls,10,temp_res,abate_curve)/10,col="black",type="l",lwd=6)
  legend_vec<-cbind(legend_vec,c("With Carbon Tax"))
  col_vec<-cbind(col_vec,c("black"))
  ext_carb_tax<<-0
  lines(temp_res,tot_ext_cost(10*eibbls,10,temp_res,abate_curve)/10,col="grey90",type="l",lwd=3)
  legend_vec<-cbind(legend_vec,c("Without Carbon Tax"))
  col_vec<-cbind(col_vec,c("grey90"))
  #legend("topright",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=2)
  ext_carb_tax<<-30
  lines(temp_res,tot_ext_cost(5*eibbls,10,temp_res,abate_curve)/10,col="grey60",type="l",lwd=3)
  legend_vec<-cbind(legend_vec,c("With Carbon Tax, 50% emissions reduction"))
  col_vec<-cbind(col_vec,c("grey60"))
  legend("topright",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=1)
  ext_carb_tax<<-old
  if(file_exp==1)
    dev.off()
  }

Fig2("extraction_cost.png",1400,750,file_exp=1)



Fig2b<-function(file_sent,width_sent,height_sent,file_exp){
  temp_ext<-c(0:30)+.05
  par(mfrow=c(1,1))
  old<-ext_carb_tax
  ext_carb_tax<<-30
  y_range<-range(0,1.5*tot_ext_cost(temp_ext*eibbls,temp_ext,500,abate_curve)/temp_ext)#all extraction columns
  x_range<-range(temp_ext)
  legend_vec<-vector()
  col_vec<-vector()
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,5,2))
  plot(x_range,y_range,type="n",lwd=2,xlab="Extraction (Millions of Barrels)",ylab="Average Extraction Cost ($/bbl)",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  lines(temp_ext,tot_ext_cost(temp_ext*eibbls,temp_ext,500,abate_curve)/temp_ext,col="black",type="l",lwd=4)
  legend_vec<-cbind(legend_vec,c("With Carbon Tax"))
  col_vec<-cbind(col_vec,c("black"))
  ext_carb_tax<<-0
  lines(temp_ext,tot_ext_cost(temp_ext*eibbls,temp_ext,500,abate_curve)/temp_ext,col="grey50",type="l",lwd=4)
  legend_vec<-cbind(legend_vec,c("Without Carbon Tax"))
  col_vec<-cbind(col_vec,c("grey50"))
#  ext_carb_tax<<-30
#  lines(temp_ext,tot_ext_cost(temp_ext*eibbls*.87,temp_ext,500,abate_curve)/temp_ext,col="grey60",type="l",lwd=8)
#  legend_vec<-cbind(legend_vec,c("Low emissions, with carbon tax"))
#  col_vec<-cbind(col_vec,c("grey60"))
  legend("topleft",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=1)
  ext_carb_tax<<-old
  if(file_exp==1)
    dev.off()
}

abate_curve<-"abate_base"
Fig2b("extraction_cost_b.png",1400,750,file_exp=1)





Fig3<-function(file_sent,width_sent,height_sent,file_exp){
  temp_ext<-c(0:30)+.05
  par(mfrow=c(1,1))
  y_range<-range(0,marg_ext_cost(temp_ext*eibbls*.5,temp_ext,500,abate_curve))#all extraction columns
  x_range<-range(temp_ext)
  legend_vec<-vector()
  old<-ext_carb_tax
  ext_carb_tax<<-30
  col_vec<-vector()
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,5,2))
  plot(x_range,y_range,type="n",lwd=2,xlab="Extraction (Millions of Barrels Per Year)",ylab="Marginal Extraction Cost ($/bbl)",cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
  ext_carb_tax<<-30
  lines(temp_ext,marg_ext_cost(temp_ext*eibbls,temp_ext,500,abate_curve),col="black",type="l",lwd=8)
  legend_vec<-cbind(legend_vec,c("With Carbon Tax"))
  col_vec<-cbind(col_vec,c("black"))
  ext_carb_tax<<-0
  lines(temp_ext,marg_ext_cost(temp_ext*eibbls,temp_ext,500,abate_curve),col="grey90",type="l",lwd=4)
  legend_vec<-cbind(legend_vec,c("Without Carbon Tax"))
  col_vec<-cbind(col_vec,c("grey90"))
  ext_carb_tax<<-30
  lines(temp_ext,marg_ext_cost(temp_ext*eibbls*.5,temp_ext,500,abate_curve),col="grey60",type="l",lwd=8)
  legend_vec<-cbind(legend_vec,c("Low emissions, with carbon tax"))
  col_vec<-cbind(col_vec,c("grey60"))
  
  legend("topleft",legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,cex=2)
  ext_carb_tax<<-old
  if(file_exp==1)
    dev.off()
}

Fig3("extraction_cost_b.png",1400,750,file_exp=0)


net_rev_royalty<- function(p_oil,bbls) {
return(net_rev_rate)
  #return(0.1)
  }

gross_rev_royalty<- function(p_oil,bbls) {
  return(gross_rev_rate)
}


cons_ext_full <- function(x,res_pass){
  rows_x <- length(x)/3 #2 choice variables plus the extraction constraint shadow value
  #print(paste("x has ",rows_x," rows",sep=""))
  x_1 <- x[1:rows_x]
  x_2 <- x[(rows_x+1):(rows_x*2)]
  x_3 <- x[(rows_x*2+1):(rows_x*3)]

  #eq1<- marg_ext_cost(x_1,reserves)+.95*m_value(reserves-x_1)-oil_price+x_2
  #marginal extraction cost equals marginal revenue product
  eq1<- (1-net_rev_royalty(oil_price))*marg_ext_cost(x_3,x_1,res_pass,abate_curve)+.95*sim_der(res_pass-x_1)-(1-net_rev_royalty(oil_price))*(1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*(x_3/x_1)*incidence)+x_2
  #eq1<-eq1*5
  #eq1<- marg_ext_cost(x_1,reserves)-oil_price+x_2
  #kuhn tucker condition for extraction greater than reserves
  eq2 <- (res_pass-x_1)*x_2
  #eq2<-eq2*100
  #marginal abatement cost equals marginal cost of carbon emissions
  #as x_3 gets smaller, percentage reduction gets larger
  
  #dec17 debug - change type be be global abate_curve
  #eq_3<-dac_dghgs(x_3,x_1,type="abate_base")-dtax_dghgs(x_3,x_1)
  eq_3<-dac_dghgs(x_3,x_1,type=abate_curve)-dtax_dghgs(x_3,x_1)
  #eq_3<-x_3-x_1*eibbls
  return(c(eq1,eq2,eq_3))
}


deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}



fit_value<<-lm((reserves*0)~ poly(reserves, 12, raw=TRUE))
slope <- model.matrix(fit_value) %*% matrix(deriv_coef(fit_value), ncol=1)
der_func <- lm(slope ~ poly(reserves, 12, raw=TRUE))

sim_der<-function(res_data){predict(der_func,data.frame(res_vals=res_data))}
ipolate<-function(res_data){predict(fit_value,data.frame(reserves=res_data))}



model_solve <- function(value_sent=reserves*0)
{
  #initial guess
  value_h <- value_sent
  crit <- 10
  iter <- 1
  #print(lambda)
  lambda <- numeric(NROW(reserves))+12
  res_ext <- pmin((reserves*.25),45)
  ghgs <- res_ext*eibbls
  x_test <- c(res_ext,lambda,ghgs)
  x_2 <- numeric(NROW(reserves))+10
  while(crit>.0005){  
    fit_value<<-lm(value_h ~ poly(reserves, 12, raw=TRUE))
    slope <- model.matrix(fit_value) %*% matrix(deriv_coef(fit_value), ncol=1)
    res_vals<- reserves
    der_func <<- lm(slope ~ poly(res_vals, 12, raw=TRUE))
    #solve it
    test_soln<-nleqslv(x_test,cons_ext_full,jacobian=NULL,control=list(maxit=500),res_pass=reserves)
    #qcheck it
    if(test_soln$termcd>1){ #resolve it
      #print(paste("resolving, Iteration",iter,"Flag is",test_soln$convergence,collapse = " "))
      print(paste("resolving, Iteration",iter,"Flag is",test_soln$termcd,collapse = " "))
      lambda <- numeric(NROW(reserves))+12
      res_ext <- pmin((reserves*.25),45)
      ghgs <- res_ext*eibbls
      x_test <- c(res_ext,lambda,ghgs)
      test_soln<-nleqslv(x_test,cons_ext_full,jacobian=NULL,control=list(maxit=500),res_pass=reserves)
      #print(paste("resolved, Iteration",iter,"New flag is",test_soln$convergence,collapse = " "))
      print(paste("resolved, Iteration",iter,"New Flag is",test_soln$termcd,collapse = " "))
    }
    #pars<-test_soln$par
    pars<-test_soln$x
    rows_soln <- length(pars)/3
    opt_ext <- pars[1:rows_soln]
    if(iter==1)
      init_ext<-opt_ext
    #mtext("Optimal Extraction", side=4, line=3)
    x_2 <- pars[(rows_soln+1):(rows_soln*2)]
    ghgs <- pars[(rows_soln*2+1):(rows_soln*3)]
    #lambda <- numeric(NROW(reserves))+oil_price/10
    next_ext<-opt_ext
    next_ext[reserves<100]<-opt_ext[reserves<100]*.9
    next_lambda<-x_2
    next_lambda[reserves<100]<-x_2[reserves<100]*1.1
    next_ghgs<-ghgs
    next_ghgs[reserves<100]<-ghgs[reserves<100]*.9
    x_test <- c(next_ext,next_lambda,next_ghgs) #set starting values for next iteration
    
    #x_test <- c(opt_ext-1,(x_2))
    #netback_h <- (1-net_rev_royalty(oil_price))*((1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext-tot_ext_cost(ghgs,opt_ext,reserves,abate_curve))
    #netback_h <- (1)*((1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext-tot_ext_cost(ghgs,opt_ext,reserves,abate_curve))
    net_rev<-((1-gross_rev_royalty(oil_price))*(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext-tot_ext_cost(ghgs,opt_ext,reserves,abate_curve))
    prod_oil_val<-(oil_price-cons_carb_tax*eibbls*incidence)*opt_ext
    oil_rev<-oil_price*opt_ext
    cons_oil_val<-(oil_price+cons_carb_tax*eibbls*(1-incidence))*opt_ext
    #lines(reserves,net_rev,col="red",type="l")
    value_old <- value_h
    endog_reserves<-reserves-opt_ext
    value_h<- (1-net_rev_royalty(oil_price))*net_rev+.95*ipolate(endog_reserves)
    crit <- mean((value_old-value_h)^2)
    
    if((iter-1)%%5==0)
    {    
      par(mfrow=c(2,1))
      par(mar=c(2.5,4,2,4))
      y_range<-range(0,100)
      x_range<-range(reserves)
      plot(x_range,y_range, type = "n")
      lines(reserves,tot_ext_cost(ghgs,opt_ext,reserves,abate_curve)/opt_ext,col="black")
      lines(reserves,reserves*0+oil_price, col="red")
      lines(reserves,marg_ext_cost(ghgs,opt_ext,reserves,abate_curve), col="blue")
      plot(reserves,opt_ext*eibbls,col="blue")
      lines(reserves,ghgs,col="black",type="l")
      
      par(mfrow=c(2,1))
      par(mar=c(2.5,4,2,4))
      plot(reserves,sim_der(reserves),col="blue")
      #axis(side=2, at = pretty(range(m_value(reserves))))
      par(new = TRUE)
      y_range<-range(opt_ext,init_ext)
      x_range<-range(reserves)
      plot(x_range,y_range, type = "n",xaxt="n",yaxt="n", bty = "n", xlab = "", ylab = "",lwd=4,cex.lab=.8, cex.axis=.8, cex.main=.8, cex.sub=.8)
      lines(reserves,opt_ext, col="red", type = "l", bty = "n", xlab = "", ylab = "",lwd=4)
      lines(reserves,init_ext, col="black", lty=2,type = "l",bty = "n", xlab = "", ylab = "",lwd=1)
      axis(side=4, at = pretty(range(opt_ext)))
      legend_vec<-vector()
      col_vec<-vector()
      y_range<-range(net_rev,cons_oil_val,oil_rev)#all extraction columns
      x_range<-range(reserves)
      legend_vec<-vector()
      plot(x_range,y_range,type="n",lwd=2,xlab="Reserves (Millions of Barrels)",ylab="Revenues and Netbacks (millions of dollars per year)",cex.lab=.8, cex.axis=.8, cex.main=.8, cex.sub=.8)
      lines(reserves,(1-net_rev_royalty(oil_price))*net_rev,col="black",type="l")
      legend_vec<-cbind(legend_vec,c("Net Revenue"))
      col_vec<-cbind(col_vec,c("black"))
      lines(reserves,net_rev,col="red",type="l")
      legend_vec<-cbind(legend_vec,c("Rev after gross rev roy"))
      col_vec<-cbind(col_vec,c("red"))
      lines(reserves,prod_oil_val,col="blue",type="l",lwd=4)
      legend_vec<-cbind(legend_vec,c("Prod oil val"))
      col_vec<-cbind(col_vec,c("blue"))
      lines(reserves,oil_rev,col="green",type="l")
      legend_vec<-cbind(legend_vec,c("Oil price * Quantity"))
      col_vec<-cbind(col_vec,c("green"))
      lines(reserves,cons_oil_val,col="orange",type="l")
      legend_vec<-cbind(legend_vec,c("Consumer oil Value"))
      col_vec<-cbind(col_vec,c("orange"))
      #layout(rbind(1,2), heights=c(7,1))  # put legend on bottom 1/8th of the chart
      legend("bottomright",legend_vec,ncol=3,lty=c(1,1),lwd=c(1,1,1,1,1),col=col_vec,cex=.8)
      
      #y_range<-range(opt_ext[0:20])
      #x_range<-range(reserves[0:20])
      #plot(x_range, y_range, main = "Constrained Extraction",type="n")
      #lines(reserves[0:20],opt_ext[0:20],col="red",lwd=4,lty=1)
      #lines(reserves[0:20],init_ext[0:20],col="blue",lwd=2,lty=1)
      #legend("topleft",c("Optimal Extraction","Initial Extraction"),lty=c(1,1),col=c("red","blue"))
      print(c(iter,crit))
    }
    iter <- iter + 1
    #crit<-0
  }
  
  output_frame<-data.frame(cbind(reserves,opt_ext,ghgs,x_2,value_h,sim_der(reserves)))
  names(output_frame) <- c(paste("reserves",deparse(scn),sep="_"), paste("opt_ext",deparse(scn),sep="_"), paste("ghgs",deparse(scn),sep="_"), paste("lambda",deparse(scn),sep="_"), paste("value",deparse(scn),sep="_"),paste("marg_val",deparse(scn),sep="_"))
  return(output_frame)  
}



#solve for optimal resources for a group of scenarios

opt_resource<-function(svec,file_sent,width_sent,height_sent,file_exp){
  #col_vec <-brewer.pal(NROW(svec),"Set1")
  col_vec <-c("black","black","grey30","grey30","grey60","grey60")
  line_vec <-c("solid", "dotted", "solid","dotted","solid","dotted")
  y_range<-range(0,out_data[,grep("opt_ext", colnames(out_data))])#all extraction columns
  y_range<-range(0,50)#all extraction columns
  x_range<-range(reserves)
  legend_vec<-vector()
  cex_scale<-.25
  if(file_exp==1)
    {
    set_png(file=paste("res_",file_sent,sep=""), width = width_sent, height = height_sent,res=130)
    cex_scale<-1.25
    }
  
  layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4.3, 1))
  par(mar=c(3.5,3.7,1,3.7)+0.2,mgp=c(2.6,1,0))
  #par(mar=c(9,5,2.5,3),mgp=c(2.6,1,0))
  plot(x_range,y_range,type="n",lwd=2,xlab="Reserves (Millions of Barrels)",ylab="Optimal Resource Extraction (millions of bbls per year)",cex.lab=cex_scale, cex.axis=cex_scale, cex.main=cex_scale, cex.sub=cex_scale)
  col_store<-1
  for(s in svec){
    col_name<-paste("opt_ext",deparse(s),sep="_")
    lines(reserves,out_data[,col_name],col = col_vec[col_store],lwd=4,lty = line_vec[col_store])
    legend_vec<- cbind(legend_vec,paste(scenario_string_clip(s,0,uniques)))
    col_store<-col_store+1  
    }
  
  y_range<-range(0,25)#emissions columns
  plot(x_range,y_range,type="n",lwd=2,xlab="Reserves (Millions of Barrels)",ylab="Optimal GHG Emissions (millions of tonnes per year)",cex.lab=cex_scale, cex.axis=cex_scale, cex.main=cex_scale, cex.sub=cex_scale)
  col_store<-1
  for(s in svec){
    col_name<-paste("ghgs",deparse(s),sep="_")
    lines(reserves,out_data[,col_name],col = col_vec[col_store],lwd=4,lty = line_vec[col_store])
    #legend_vec<- cbind(legend_vec,paste(scenario_string_clip(s,0,uniques)))
    col_store<-col_store+1  
  }
  if(force_labels==1)
    legend_vec<-global_labels
  par(mar=c(1,1,0,1))
  plot.new()
  legend("bottom",legend_vec,lty=line_vec,lwd=c(3,3,3,3,3),ncol=1,col=col_vec,cex=cex_scale*.8,seg.len = 5)
  if(file_exp==1)
    dev.off()
  
 #second graph 
  
  #solve for optimal initial reserves
  
  #col_vec <-brewer.pal(NROW(svec),"Set1")
  
  #col_vec <-c("black","black","grey30","grey30")
  #line_vec <-c("dashed", "dotted", "dotdash","longdash")
  
  legend_vec<-vector()
  #is there a reserves tax?
  #if yes
  if(scenarios[s,"Tax Type"]=="reserves"){
    legend_vec<-cbind(legend_vec,"Marginal cost of reserve creation (before tax)")
    legend_vec<-cbind(legend_vec,"Marginal cost of reserve creation (after tax)")
    col_vec <-c("grey30","grey30","grey60","grey60")
    line_vec <-c("solid", "dotted", "solid","dotted")
  }
  #if no
  else {
    legend_vec<-cbind(legend_vec,"Marginal Cost of Reserve Creation")
    col_vec <-c("grey30","grey30","grey60","grey60")
    line_vec <-c("solid","dotted", "solid","dotted")
  }
  #print("Made it to mark 1")
  
  ipl_mc_res <- splinefun(reserves,marg_reserve_cost(reserves),method = "natural")
  y_range<-range(0,out_data[,grep("marg_val", colnames(out_data))],ipl_mc_res(reserves))#all extraction columns
  y_range<-range(0,70)#all extraction columns
  x_range<-range(reserves)
  if(file_exp==1)
    set_png(file=paste("equil_",file_sent,sep=""), width = width_sent, height = height_sent,res=130)
  #par(mar=c(8,5,5,5))
  par(mar=c(9,5,1,3),mgp=c(2.6,1,0))
  plot(x_range,y_range,type="n",xlab="Reserves (Millions of barrels)",ylab="Marginal value or cost of reserves ($/bbl)",xaxs = "i",yaxs = "i",cex.lab=cex_scale, cex.axis=cex_scale, cex.main=cex_scale, cex.sub=cex_scale)  
  #axis(side=2, at = pretty(range(y_range)))
  #lines(reserves,ipl_mc_res(reserves),lwd=4)  
  col_store<-1
  opt_vec<-vector()
  for(s in svec){
    col_name<-paste("marg_val",deparse(s),sep="_")
      ipl_mv_res <- splinefun(reserves,out_data[[col_name]],method = "natural")
      #For a one unit change in extraction, that's 365*1000/100000 change in reserves, or 0.365
      policy_vec<-data.frame(scenarios[s,])
      names(policy_vec)<-colnames(scenarios)
      #print(policy_vec)
      if(policy_vec$`Tax Type`=="reserves")
        {
        res_carb_tax<<-policy_vec$`Carbon Tax`
        ipl_mc_res <- splinefun(reserves,marg_reserve_cost(reserves),method = "natural")
        lines(reserves,ipl_mc_res(reserves),lwd=3,lty="dotted",col="black")  
        }
      else
        {
        res_carb_tax<<-0  
        ipl_mc_res <- splinefun(reserves,marg_reserve_cost(reserves),method = "natural")
        lines(reserves,ipl_mc_res(reserves),lwd=3,lty="solid",col="black")  
        }
      #ipl_mc_res <- splinefun(reserves,marg_reserve_cost(reserves),method = "natural")
      nle_res_dyn <- function(x){
        return((ipl_mc_res(x)-ipl_mv_res(x))[1][1])}
      #lines(reserves,ipl_mc_res(reserves),lwd=8,lty=2)  
      lines(reserves,ipl_mv_res(reserves),col=col_vec[col_store],lwd=4,lty=line_vec[col_store])
      num_test<-180
      opt_res<- nleqslv(num_test,nle_res_dyn,jacobian=NULL)
      #solve fr optimal reserves acquisition
      print(paste("Optimal reserves for scenario",s,"is",opt_res$x,sep = " "))
      points(opt_res$x,ipl_mv_res(opt_res$x), col = "black",pch = 16, cex = cex_scale)
      if(force_labels!=1){
        if(opt_res$x>0)
          legend_vec<- cbind(legend_vec,paste(scenario_string_clip(s,0,uniques),": reserves=",round(opt_res$x,digits=0)," mmbbls, marginal reserve value=$",round(ipl_mv_res(opt_res$x),digits=2),"/bbl",sep = ""))
        else
          legend_vec<- cbind(legend_vec,paste(scenario_string_clip(s,0,uniques),": corner solution, no reserve creation",sep = ""))
        }
      if(force_labels==1){
          if(opt_res$x>0)
            legend_vec<- cbind(legend_vec,paste(global_labels[col_store]," (Reserves=",round(opt_res$x,digits=0)," mmbbls, Marginal Value=$",round(ipl_mv_res(opt_res$x),digits=2),"/bbl)",sep = ""))
          else
            legend_vec<- cbind(legend_vec,paste(global_labels[col_store],": corner solution, no reserve creation",sep = ""))
        }
      
      col_store<-col_store+1
      #print(paste("passing through",s))  
      
      opt_res_out<-data.frame(cbind(s,round(opt_res$x,digits=0),round(ipl_mv_res(opt_res$x),digits=2)))
      opt_vec<-rbind(opt_vec,opt_res_out)
      }
  names(opt_vec) <- c("scenario","reserves","marg_val")
  opt_data<<-data.frame(opt_vec)
  #alt_pts<-alt_equil(opt_data,"opt_resources_appl_test.png",1400,750,file_exp=1)
  #print(alt_pts)
  #points(alt_pts[2,3],alt_pts[2,4], col = "blue",pch = 16, cex = cex_scale*1.5)
  #points(alt_pts[3,3],alt_pts[3,4], col = "red",pch = 16, cex = cex_scale*1.5)
  #arrows(alt_pts[1,3],alt_pts[1,4],x1=alt_pts[2,3]+50,y1=alt_pts[2,4]-1, col = "red",code=2,pch = 16, lty=1,lwd=3)
  #arrows(alt_pts[2,3],alt_pts[2,4],x1=alt_pts[4,3]-50,y1=alt_pts[4,4], col = "blue",code=2,pch = 16, lty=1,lwd=3)
  #text(alt_pts[1,3],alt_pts[1,4],pos=2,labels="Extraction tax    ",cex = cex_scale*1.5)
  #text(alt_pts[4,3],alt_pts[4,4],pos=4,labels="      Reserve Tax",cex = cex_scale*1.5)
  #if(force_labels==1)
  #  legend_vec<-global_labels
  #plot.new()
  #print(line_vec)
  #print(col_vec)
  if(policy_vec$`Tax Type`=="reserves"){
      line_vec<-c("solid","dotted",line_vec) 
      col_vec<- c("black","black","grey30","grey30","grey60","grey60")
  }
  else {
    line_vec<-c("solid",line_vec) 
    col_vec<- c("black","grey30","grey30","grey60","grey60")
  }
  par(xpd=T, mar=c(1,1,0,1))
  #plot.new()
  legend("bottom",legend_vec,lty=line_vec,lwd=c(1,1,1,1,1),inset=c(0,-.45),col=col_vec,cex=cex_scale*.65,seg.len = 5,y.intersp=1.35)
  if(file_exp==1)
    dev.off()
  return(opt_data)
}


alt_equil<-function(opt_mat,file_sent,width_sent,height_sent,file_exp){
svec<- opt_mat[,1]
temp_vec<-vector()
for(s in svec){
   col_name<-paste("marg_val",deparse(s),sep="_")
   ipl_mv_res <- splinefun(reserves,out_data[[col_name]],method = "natural")
   policy_vec<-data.frame(scenarios[s,])
   names(policy_vec)<-colnames(scenarios)
   #print("Policy_vec1 is")
   #print(policy_vec)
    #solve for marginal cost for scenario s2
    alt_vec<-vector() #create new temp vector to store combinations
    for(s2 in svec){
      policy_vec2<-data.frame(scenarios[s2,])
      names(policy_vec2)<-colnames(scenarios)
      #print("Policy_vec2 is")
      #print(policy_vec2)
      if(policy_vec2$`Tax Type`=="reserves")
      {
        res_carb_tax<<-policy_vec$`Carbon Tax`
        ipl_mc_res <- splinefun(reserves,marg_reserve_cost(reserves),method = "natural")
      }
      else
      {
        res_carb_tax<<-0  
        ipl_mc_res <- splinefun(reserves,marg_reserve_cost(reserves),method = "natural")
      }
      nle_res_dyn <- function(x){
        return((ipl_mc_res(x)-ipl_mv_res(x))[1][1])}
      #lines(reserves,ipl_mc_res(reserves),lwd=8,lty=2)  
      num_test<-180
      opt_res<- nleqslv(num_test,nle_res_dyn,jacobian=NULL)
      #print(paste("s1 is ",s,", s2 is ",s2,", equilibrium would be ",opt_res$x,"bbls at price $",ipl_mc_res(opt_res$x),"/bbl",sep=""))
      alt_vec<-rbind(alt_vec,c(s,s2,opt_res$x,ipl_mc_res(opt_res$x)))
      }
    temp_vec<-rbind(temp_vec,alt_vec)  
    }
return(temp_vec)
}


#calculate optimal extraction over time from optimal reserves


simulate<-function(simul_per,svec){
  sim_out<-data.frame(c(1:simul_per))
  names(sim_out)<-c("time")
  sim_store<-data.frame()
  for(s in svec)
  {
    #assign globals based on scenario
    oil_price<-scenarios[s,grep("Oil Price", colnames(scenarios))]
    price_vec=c(oil_price)
    #print(" ")
    #policy_vec<-data.frame(scenarios[s,])
    #names(policy_vec)<-colnames(scenarios)
    assign_globals(scenarios[s,])
    print(paste("Scenario",s,scenario_string(s),sep = " ")  )
    start_res<-opt_data[opt_data$scenario==s,2]
    if(start_res>0)
      {
      sim_res<-start_res[1]
      col_name<-paste("marg_val",deparse(s),sep="_")
      #establish derivative approximation
      mvals<-out_data[[col_name]]
      res_vals<-out_data$reserves
      der_func <<- lm(mvals ~ poly(res_vals, 10, raw=TRUE))
      sim_der<-function(res_data){predict(der_func,data.frame(res_vals=res_data))}
      sim_store<-data.frame()
      test_cont<-c(.5*sim_res,oil_price/8,8)
      for(per_t in 1:simul_per) {
        #print(paste("time period",per_t,sep=" "))
        #print(test_cont)
        test_37 <- nleqslv(test_cont,cons_ext_full,jacobian=NULL,method="Newton",control=list(maxit=1500),res_pass=sim_res)
        sim_ext <- test_37$x[1]
        sim_lambda <- test_37$x[2]
        sim_ghgs<-test_37$x[3]
        if(test_37$termcd>1)
          print(paste("time period",per_t," ext=",round(sim_ext,digits=2)," res=",round(sim_res,digits=2)," lambda=",round(sim_lambda,digits=2)," ghgs=",round(sim_ghgs,digits=2),"flag=",test_37$termcd,sep=" "))
        #print(paste(test_37$fvec[1],test_37$fvec[2],test_37$fvec[3]," iterations ",test_37$iter,sep="  "))
        sim_store<-rbind(sim_store,cbind(sim_res,sim_ext,sim_ghgs))
        sim_res<-max(sim_res-sim_ext,.1)
        test_cont<-c(sim_ext*.5,18,sim_ghgs*.5)
        if(sim_res<sim_ext)
            test_cont<-c(min(sim_res*.05,sim_ext*.5),18,sim_ghgs*.05)
        
      }
    }
    else
        {
          sim_store<-data.frame()
          sim_store<-rbind(sim_store,cbind(numeric(simul_per),numeric(simul_per)))
        }
    names(sim_store) <- c(paste("reserves",deparse(s),sep="_"), paste("opt_ext",deparse(s),sep="_"),paste("GHGs",deparse(s),sep="_"))
    #print(sim_store)
    sim_out<-cbind(sim_out,sim_store)
  }
return(sim_out)
}


Simfig_ext<-function(svec,file_sent,width_sent,height_sent,file_exp){
  legend_vec<-vector()
  #col_vec <-c("red","blue","green","orange")
  #col_vec <-brewer.pal(10,"Paired")
  #col_vec <-c("red",col_vec)
  col_vec <-c("black","black","grey","grey","grey80","grey80")
  line_vec <-c("solid", "dotted", "solid","dotted", "solid","dotted")
  y_range<-range(sim_data[,grep("ext", colnames(sim_data))])#all extraction columns
  x_range<-range(sim_data$time)
  col_store<- 1
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  par(mar=c(5,5,5,2))
  plot(x_range,y_range,type="n",xlab="Time (Years)",ylab="Extraction (millions of barrels)",cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)  
  for(s in svec){
    ext_col<-paste("opt_ext",deparse(s),sep="_")
    lines(sim_data$time,sim_data[,which(colnames(sim_data)==ext_col,arr.ind = FALSE)],type="l",lwd=6,col=col_vec[col_store],lty=line_vec[col_store])
    col_store<- col_store+ 1
    legend_vec<- cbind(legend_vec,scenario_string_clip(s,0,uniques))
  }
  legend("topright",legend_vec,lty=line_vec,lwd=c(4,4,4,4,4),col=col_vec,cex=1.5)
  if(file_exp==1)
    dev.off()
}


Simfig_res<-function(svec,file_sent,width_sent,height_sent,file_exp){
  legend_vec<-vector()
  #col_vec <-c("red","blue","green","orange")
  #col_vec <-brewer.pal(10,"Paired")
  #col_vec <- c("red",col_vec)
  col_vec <-c("black","black","grey","grey","grey80","grey80")
  line_vec <-c("solid", "dotted", "solid","dotted", "solid","dotted")
  
  y_range<-range(sim_data[,grep("reserves", colnames(sim_data))])#all reserves columns
  x_range<-range(sim_data$time)
  col_store<- 1
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  plot(x_range,y_range,type="n",xlab="Time (Years)",ylab="Remaining Reserves (millions of barrels)",)  
  for(s in svec){
    res_col<-paste("reserves",deparse(s),sep="_")
    lines(sim_data$time,sim_data[,which(colnames(sim_data)==res_col,arr.ind = FALSE)],type="l",lwd=4,col=col_vec[col_store],lty=line_vec[col_store])
    col_store<- col_store+ 1
    legend_vec<- cbind(legend_vec,scenario_string_clip(s,0,uniques))
  }
  par(xpd=TRUE)
  #legend(2.8,-1,legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,ncol = 4)
  legend("topright",legend_vec,lty=line_vec,lwd=c(4,4,4,4,4),col=col_vec)
  if(file_exp==1)
    dev.off()
}


Simfig_ghgs<-function(svec,file_sent,width_sent,height_sent,file_exp){
  legend_vec<-vector()
  #col_vec <-c("red","blue","green","orange")
  #col_vec <-brewer.pal(10,"Paired")
  #col_vec <- c("red",col_vec)
  col_vec <-c("black","black","grey","grey","grey80","grey80")
  line_vec <-c("solid", "dotted", "solid","dotted", "solid","dotted")
  y_range<-range(sim_data[,grep("GHGs", colnames(sim_data))])#all reserves columns
  x_range<-range(sim_data$time)
  col_store<- 1
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  plot(x_range,y_range,type="n",xlab="Time (Years)",ylab="Emissions (Mt/yr)",)  
  for(s in svec){
    res_col<-paste("GHGs",deparse(s),sep="_")
    lines(sim_data$time,sim_data[,which(colnames(sim_data)==res_col,arr.ind = FALSE)],type="l",lwd=4,col=col_vec[col_store],lty=line_vec[col_store])
    col_store<- col_store+ 1
    tot_ghgs<-round(sum(sim_data[,which(colnames(sim_data)==res_col,arr.ind = FALSE)]),digits=2)
    legend_vec<- cbind(legend_vec,paste(scenario_string_clip(s,0,uniques)
           ," Total GHGs=",tot_ghgs,".",sep=""))
  }
  par(xpd=TRUE)
  #legend(2.8,-1,legend_vec,lty=c(1,1),lwd=c(4,4,4,4,4),col=col_vec,ncol = 4)
  legend("topright",legend_vec,lty=line_vec,lwd=c(4,4,4,4,4),col=col_vec)
  if(file_exp==1)
    dev.off()
}


Simfig_combo<-function(svec,file_sent,width_sent,height_sent,file_exp){
  cex_scale<-1.25
  legend_vec<-vector()
  #col_vec <-c("red","blue","green","orange")
  col_vec <-c("black","black","grey30","grey30","grey60","grey60")
  line_vec <-c("solid", "dotted","solid", "dotted","solid", "dotted")
  
  y_range<-range(sim_data[,grep("GHGs", colnames(sim_data))])#all reserves columns
  x_range<-range(sim_data$time)
  col_store<- 1
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4.5, 1))
  par(mar=c(3,3,1.5,1.5)+0.1,mgp=c(2,1,0))
  y_range<-range(0,40)#extraction columns
  total_ext<-array()
  plot(x_range,y_range,type="n",xlab="Time (Years)",ylab="Extraction (Millions of barrels per year)")  
  for(s in svec){
    ext_col<-paste("opt_ext",deparse(s),sep="_")
    lines(sim_data$time,sim_data[,which(colnames(sim_data)==ext_col,arr.ind = FALSE)],type="l",lwd=4,col=col_vec[col_store],lty=line_vec[col_store])
    total_ext[col_store]<-round(sum(sim_data[,which(colnames(sim_data)==ext_col,arr.ind = FALSE)]),digits=0)
    col_store<- col_store+ 1
    }
  y_range<-range(0,20)#emissions columns
  col_store<- 1
  plot(x_range,y_range,type="n",xlab="Time (Years)",ylab="Emissions (Mt/yr)")  
  for(s in svec){
    res_col<-paste("GHGs",deparse(s),sep="_")
    lines(sim_data$time,sim_data[,which(colnames(sim_data)==res_col,arr.ind = FALSE)],type="l",lwd=4,col=col_vec[col_store],lty=line_vec[col_store])
    tot_ghgs<-round(sum(sim_data[,which(colnames(sim_data)==res_col,arr.ind = FALSE)]),digits=0)
    if(force_labels!=1)
      legend_vec<- cbind(legend_vec,scenario_string_clip(s,0,uniques))
    if(force_labels==1)
      legend_vec<- cbind(legend_vec,paste(global_labels[col_store]," (Total Extraction=",total_ext[col_store]," mmbbls, Total Emissions= ",tot_ghgs," Mt)",sep = ""))
    col_store<- col_store+ 1
  }
  
  
  #par(mar=c(1,1,1,1))
  par(mar=c(1,1,0,1))
  plot.new()
  legend("bottom",legend_vec,lty=line_vec,lwd=c(3,3),ncol=1,col=col_vec,cex=cex_scale*.8, seg.len=5)
  if(file_exp==1)
    dev.off()
  
 # print(legend_vec)
}


Simfig_combo_add<-function(svec,file_sent,width_sent,height_sent,file_exp){
  cex_scale<-1.25
  legend_vec<-vector()
  #col_vec <-c("red","blue","green","orange")
  col_vec <-c("black","black","grey30","grey30","grey60","grey60")
  line_vec <-c("solid", "dotted","solid", "dotted","solid", "dotted")
  
  y_range<-range(sim_data_test[,grep("GHGs", colnames(sim_data_test))])#all reserves columns
  x_range<-range(sim_data_test$time)
  col_store<- 1
  if(file_exp==1)
    set_png(file=file_sent, width = width_sent, height = height_sent)
  layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights=c(4.5, 1))
  par(mar=c(3,3,1.5,1.5)+0.1,mgp=c(2,1,0))
  y_range<-range(0,40)#extraction columns
  total_ext<-array()
  plot(x_range,y_range,type="n",xlab="Time (Years)",ylab="Extraction (Millions of barrels per year)")  
  for(s in svec){
    ext_col<-paste("opt_ext",deparse(s),sep="_")
    lines(sim_data_test$time,sim_data_test[,which(colnames(sim_data_test)==ext_col,arr.ind = FALSE)],type="l",lwd=4,col=col_vec[col_store],lty=line_vec[col_store])
    total_ext[col_store]<-round(sum(sim_data_test[,which(colnames(sim_data_test)==ext_col,arr.ind = FALSE)]),digits=0)
    col_store<- col_store+ 1
  }
  y_range<-range(0,20)#emissions columns
  col_store<- 1
  plot(x_range,y_range,type="n",xlab="Time (Years)",ylab="Emissions (Mt/yr)")  
  for(s in svec){
    res_col<-paste("GHGs",deparse(s),sep="_")
    lines(sim_data_test$time,sim_data_test[,which(colnames(sim_data_test)==res_col,arr.ind = FALSE)],type="l",lwd=4,col=col_vec[col_store],lty=line_vec[col_store])
    tot_ghgs<-round(sum(sim_data_test[,which(colnames(sim_data_test)==res_col,arr.ind = FALSE)]),digits=0)
    if(force_labels!=1)
      legend_vec<- cbind(legend_vec,scenario_string_clip(s,0,uniques))
    if(force_labels==1)
      legend_vec<- cbind(legend_vec,paste(global_labels[col_store]," (Total Extraction=",total_ext[col_store]," mmbbls, Total Emissions= ",tot_ghgs," Mt)",sep = ""))
    col_store<- col_store+ 1
  }
  
  
  par(mar=c(1,1,1,1))
  plot.new()
  legend("bottom",legend_vec,lty=line_vec,lwd=c(3,3),ncol=1,col=col_vec,cex=cex_scale*.8, seg.len=5)
  if(file_exp==1)
    dev.off()
  
  # print(legend_vec)
}


#The line type. Line types can either be specified as an integer 
#(0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
#or as one of the character strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank" uses ‘invisible lines’ (i.e., does not draw them).


make_uniques<-function(svec)
{
  unique_set<-"Number"
  for(k in seq(2,8))  
    unique_set<-cbind(unique_set,length(unique(scenarios[svec,k]))-1)
  colnames(unique_set)<-names(scenarios)
  return(unique_set)
}




assign_globals<-function(policy_vec)
{
  oil_price<<-45
  oil_price<<-as.numeric(policy_vec$`Oil Price`)
  price_vec<-c(oil_price)  
  res_carb_tax<<-0
  ext_carb_tax<<-0
  oba_val<<-0
  cons_carb_tax<<-0 #impact of consumption carbon taxes on oil prices
    if(policy_vec$`Tax Type`=="extraction"){
      ext_carb_tax<<-policy_vec$`Carbon Tax`
      oba_val<<-policy_vec$`Carbon Tax`
      #print(paste("OBA_val is ",oba_val))
    }
    if(policy_vec$`Tax Type`=="consumption")
      cons_carb_tax<<-policy_vec$`Carbon Tax`
    if(policy_vec$`Tax Type`=="reserves"){
      res_carb_tax<<-policy_vec$`Carbon Tax`
      oba_val<<-policy_vec$`Carbon Tax`
      #print(paste("OBA_val is ",oba_val))
    }
  net_rev_rate<<-0
  gross_rev_rate<<-0
  if(policy_vec$Base =="gross")
    gross_rev_rate<<-policy_vec$`Royalty`
  if(policy_vec$Base =="net")
    net_rev_rate<<-policy_vec$`Royalty`
  oba_rate<<-policy_vec$`OBA`
  abate_curve<<-policy_vec$`MAC_TYPE`
  
    }



#use this if you're starting a new run



#this set lets you used old output to start. 
temp_data<-out_data #set up a spare global to store interim data runs

#select_data<-scenarios[which(scenarios$Number>14),1]
starting_vals<-0
select_data<-scenarios[,1]
start_data<-out_data[,grep("value", colnames(out_data))]#all value columns
start_data<-start_data[,select_data]#then grab the correct scenarios only

remove(out_data)
out_data<-data.frame(reserves)

#select_data<-scenarios
#print(colnames(scenarios))
for(scn in select_data){
  #solve_data<-model_solve(p,carb_vec)
  print(paste("Scenario ",scn))
  oil_price<-scenarios[scn,grep("Oil Price", colnames(scenarios))]
  price_vec=c(oil_price)
  print(" ")
  policy_vec<-data.frame(scenarios[scn,])
  names(policy_vec)<-colnames(scenarios)
  assign_globals(policy_vec)
  print(paste("oil price should be",scenarios[scn,grep("Oil Price", colnames(scenarios))],"and is",oil_price))
  print(paste("carbon price should be",scenarios[scn,grep("Carbon Tax", colnames(scenarios))],"applied at",scenarios[scn,grep("Tax Type", colnames(scenarios))]))
  print(paste("carbon prices are",res_carb_tax,ext_carb_tax,cons_carb_tax))
  print(paste("Royalty rate should be",scenarios[scn,grep("Royalty", colnames(scenarios))],"applied against",scenarios[scn,grep("Base", colnames(scenarios))],"revenues"))
  print(paste("Royalty rates are",gross_rev_rate,net_rev_rate))
  print(paste("Abatement curves should be",scenarios[scn,grep("MAC", colnames(scenarios))]," and is ",abate_curve))
  
  start.time <- Sys.time()
  if(starting_vals==1)
    {
    start_vals<-start_data[,scn]
    solve_data<-model_solve(value_sent=start_vals)
    }
  else
    solve_data<-model_solve()
  out_data<-cbind(out_data,solve_data)
  end.time <- Sys.time()
  time.taken<-round(as.period(end.time-start.time, unit = "minutes"),digits=0)
  print(paste("Time to solve this iteration",time.taken))
 }


time_periods<-60
sim_data<-data.frame(seq(1,time_periods))
select_data<-scenarios$Number #run all scenarios
uniques<-make_uniques(select_data)
force_labels<-0
for(scn in select_data){
  print(scn)
  opt_resource(scn,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=0)
  sim_data<-cbind(sim_data,simulate(time_periods,scn))#simulate based on scenario numbers and time periods.
}


sim_data<-sim_data[0:50,]


filename<-paste("all_",Sys.Date(),".RData",sep = "")

save(list = ls(all = TRUE), file= filename)



#or run from here loading the data
#filename<-paste("all_2018-04-01.RData",sep = "")
#load(file=filename)





#fix everything but oil and carbon
sim_id<-"oil_carbon"

select_data<-scenarios[which(scenarios$`Oil Price` == 80 &scenarios$`Tax Type` =="extraction" & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$OBA ==0 & scenarios$MAC_TYPE =="abate_base"),1]

uniques<-make_uniques(select_data)
force_labels<-1
global_labels<-c("No Carbon Tax","$50/t Carbon Tax on Extraction")
opt_resource(select_data,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ghgs(select_data,paste("sim_ghgs_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_combo(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0


sim_id<-"royalties"
force_labels<-1
global_labels<-c("No Resource Royalties","30% Gross Revenue Royalty","30% Net Revenue Royalty")
select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$`Tax Type` =="extraction" & scenarios$`Carbon Tax` ==0 & scenarios$OBA ==0 & scenarios$MAC_TYPE =="abate_base"),1]
#select_data<-select_data[-1]
uniques<-make_uniques(select_data)
opt_resource(select_data,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ghgs(select_data,paste("sim_ghgs_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_combo(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0


sim_id<-"royalties_2"
force_labels<-1
global_labels<-c("No Carbon Tax or Resource Royalties","$50/t Carbon Tax on Extraction, No Resource Royalties","$50/t Carbon Tax on Extraction, 30% Gross Revenue Royalty","$50/t Carbon Tax on Extraction, 30% Net Revenue Royalty")
select_data<-c(1,2,5,6)
uniques<-make_uniques(select_data)
opt_resource(select_data,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ghgs(select_data,paste("sim_ghgs_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_combo(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0



sim_id<-"OBAs"
force_labels<-1
global_labels<-c("No Carbon Tax","$50/t Carbon Tax on Extraction","$50/t Carbon Tax on Extraction with 80% OBA")
select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$`Tax Type` =="extraction" & scenarios$MAC_TYPE =="abate_base"),1]
uniques<-make_uniques(select_data)
opt_resource(select_data,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ghgs(select_data,paste("sim_ghgs_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_combo(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0

sim_id<-"MACs"

force_labels<-1
global_labels<-c("No Carbon Tax", "$50/t Carbon Tax on Extraction, Linear MAC","$50/t Carbon Tax on Extraction, Convex MAC","$50/t Carbon Tax on Extraction, Concave MAC")
select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$`Tax Type` =="extraction" & scenarios$OBA ==0),1]
uniques<-make_uniques(select_data)
opt_resource(select_data,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_ghgs(select_data,paste("sim_ghgs_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_combo(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0


sim_id<-"MACs_fixed_res"

force_labels<-1
global_labels<-c("No Carbon Tax", "$50/t Carbon Tax on Extraction, Linear MAC","$50/t Carbon Tax on Extraction, Convex MAC")
select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$`Tax Type` =="extraction" & scenarios$OBA ==0),1]
uniques<-make_uniques(select_data)
opt_resource(select_data,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
opt_data$reserves<-opt_data$reserves[2]
time_periods<-50
sim_data_test<-data.frame(seq(1,time_periods))
for(scn in select_data){
  sim_data_test<-cbind(sim_data_test,simulate(time_periods,scn)[1:50,])#simulate based on scenario numbers and time periods.
}

Simfig_combo_add(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0


#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ghgs(select_data,paste("sim_ghgs_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_combo(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0





sim_id<-"res_tax"

force_labels<-1
global_labels<-c("No Carbon Tax","$50/t Carbon Tax on Extraction","$50/t Carbon Tax on Reserves")
select_data<-scenarios[which(scenarios$`Oil Price` == 80 & scenarios$Royalty ==0 & scenarios$`Base`=="gross" & scenarios$OBA ==0 & scenarios$MAC_TYPE =="abate_base"),1]
uniques<-make_uniques(select_data)

opt_resource(select_data,paste("opt_resources_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_ext(select_data,paste("sim_ext_",sim_id,".png",sep=""),1200,750,file_exp=1)
#Simfig_res(select_data,paste("sim_res_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_ghgs(select_data,paste("sim_ghgs_",sim_id,".png",sep=""),1200,750,file_exp=1)
Simfig_combo(select_data,paste("sim_combo_",sim_id,".png",sep=""),1200,750,file_exp=1)
force_labels<-0



#solve for a specific scenario
solve_one<-function(scn)
{
  print(paste("Scenario ",scn))
  oil_price<-scenarios[scn,grep("Oil Price", colnames(scenarios))]
  price_vec=c(oil_price)
  print(" ")
  policy_vec<-data.frame(scenarios[scn,])
  names(policy_vec)<-colnames(scenarios)
  assign_globals(policy_vec)
  print(paste("oil price should be",scenarios[scn,grep("Oil Price", colnames(scenarios))],"and is",oil_price))
  print(paste("carbon price should be",scenarios[scn,grep("Carbon Tax", colnames(scenarios))],"applied at",scenarios[scn,grep("Tax Type", colnames(scenarios))]))
  print(paste("carbon prices are",res_carb_tax,ext_carb_tax,cons_carb_tax))
  print(paste("Royalty rate should be",scenarios[scn,grep("Royalty", colnames(scenarios))],"applied against",scenarios[scn,grep("Base", colnames(scenarios))],"revenues"))
  print(paste("Royalty rates are",gross_rev_rate,net_rev_rate))
  print(paste("Abatement curves should be",scenarios[scn,grep("MAC", colnames(scenarios))]," and is ",abate_curve))
  start.time <- Sys.time()
  solve_data<-model_solve()
  #out_data<<-cbind(out_data,solve_data)
  end.time <- Sys.time()
  time.taken<-round(as.period(end.time-start.time, unit = "minutes"),digits=0)
  print(paste("Time to solve this iteration",time.taken))
  solve_data
}

#run_num<-8
#single_soln<-solve_one(run_num)
#out_min<- 2+(run_num-1)*6
#out_max<-out_min+5
#out_data[,out_min:out_max]<-single_soln


