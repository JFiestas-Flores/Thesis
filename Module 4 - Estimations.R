# rm(list=ls())
# AFTER RUNNING SCRIPTS 1 TO 3 with NPostPeriods = 10
# NPostPeriodsPitLake=20  
setwd ("G:/Shared drives/FES Economics of Tailings Project/A. Treatment cost/8. Optimal design/Final version/New results Npostperiods = 10")
#setwd ("G:/Shared drives/FES Economics of Tailings Project/A. Treatment cost/8. Optimal design/Final version/New results Npostperiods = 10_Med_Wet")
#setwd ("G:/Shared drives/FES Economics of Tailings Project/A. Treatment cost/8. Optimal design/Final version/New results Npostperiods = 10 Cap 25")
#setwd ("G:/Shared drives/FES Economics of Tailings Project/A. Treatment cost/8. Optimal design/Final version/New results Npostperiods = 10 Cap 25 Final")
getwd()

library(GA)

# In the saving files the first number stands for target and the second for end of treatment time

## MAIN VARIABLES FOR THIS SCRIPT is NPostPeriods, ran previously in script 3##

# Number of iterarion per run (maxiter)
MAX = 20 # 20 for NT 100 for treatments

############ Parameter that can be changed  ##############

# Max production from River
Pond_reduction_rate=0.05
MaxProdWaterFromRiver=20
MaxStorageVolume = 25 # Change it to 25
MaxPitLakeVolume = 120 # Should be a
TargetQuantityWaterEndPitLake= 100 # 15 This changes then you can have more water to the pitlake
MaxProdWaterFromRiver = 20 # 

# Intereste rate
i_rate= 0.04

# Wetland parameter
wetland_t = 80 # or 0 for no treatment scenario

# End pitlake constraints
PitLake_Pond_Constraints<-c(MaxPitLakeVolume,TargetQuantityWaterEndPitLake,PitLakeFlowThrough,MaxStorageVolume,PitLake_Vol_Stabi_Time,MaxProdWaterFromRiver,Pond_reduction_rate)

# Mine results parameter
Mine_Results<-CallMineWaterSystemFn(Nperiods=Nperiods,NPostPeriods=NPostPeriods,waterTreatmentVector=WaterTreatVec,ub_WatTreat=ub_WatTreat,PitLake_Pond_Constraints=PitLake_Pond_Constraints,
                                    ObjectFnCoefs=c(3500,-0.03156905,-1,2.5,100,-0.03156905)) 
conic_OSPW_prob<-Mine_Results$conic_OSPW_prob
ConsRowNames<-rownames(Mine_Results$AMatbc$AMat)



#rownames(conic_OSPW_prob$A)

### #ObjectFnCoefs=c(RevenueForBitumen,-costperunitfeshwater,-1 for cost of water recycling conic portion,2.5 for benefit of water recycling,5 Benefit of water treatment,-costperunitfeshwater)

## Pollution limits
# NAs targets (20,10,5,4,3,2,1)
targets <- c(20)

#TDS Targets
TDS_target = 0
target_tds= c(2500)

# Penalty factors
p_factor <- 40 # Adding this allows me to test for different treatment times


###############################################################

targets <- c(20) # Special case since doesn't really matter as there is no treatment

# No treatment (NT) ##############################
tb_up=1 # upper tech bound
tb_lw=0.01

for (j in targets) {
  NAtarget=j 
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 

{
  upbound=c(20,20,50,50,wetland_t,80,NAtarget,tb_up)
  lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
  
  #Chronic and Accute Limits
  
  G_a=c(TDS_target,860,NAtarget,10,50)
  G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
  
  Startpop = 50 # Number for the starting population
  StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
  StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
  StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
  StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
  StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
  StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
  StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
  StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
  
  # Reset the size for it to work To run it faster
  StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
  StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
  StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
  StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
  StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
  StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
  StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
  StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
  set.seed(20211121)
  
  system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                     lower =  lwbound, upper = upbound,  suggestions=StartPopulation, #Take this out for wetland =0 
                                     popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                     optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                     optimArgs = list(poptim = 0.5,
                                                      pressel = 0.99,
                                                      control = list(fnscale = -1, maxit = 1000)),
                                     parallel =TRUE))
  
  DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
  StartPopulation[1,]<-DecisionVariables
  
  system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                     lower =  lwbound, upper = upbound, suggestions=StartPopulation, #Take this out for wetland =0 
                                     popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                     optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                     optimArgs = list(poptim = 0.5,
                                                      pressel = 0.99,
                                                      control = list(fnscale = -1, maxit = 1000)),
                                     parallel =TRUE))
  
  
  DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
  WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
  
  #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
  
 penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                        WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                        WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
  }

assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_NT_Post10"),DecisionVariables)
assign(paste0("WCR_NT",j,i,"_Post10"),WCR)
assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_NT_Post10"),WCR$ObjectiveFunction)
assign(paste0("penalty_ratio_NT",j,i,"_Post10"),penalty_ratio)
    }
}


# Saving results
{
      save (
     OptDecVariables_NATarget20_TDS2500_NT_Post10, #_NOwet #Med_wet
     ObjFunction_NATarget20_TDS2500_NT_Post10, #_NOwet #Med_wet
     WCR_NT202500_Post10, #_NOwet #Med_wet
     penalty_ratio_NT202500_Post10, #_NOwet #Med_wet
     
    # OptDecVariables_NATarget12_TDS2500_NT_Post10, #_NOwet #Med_wet
     #ObjFunction_NATarget12_TDS2500_NT_Post10, #_NOwet #Med_wet
     #WCR_NT122500_Post10, #_NOwet #Med_wet
     #penalty_ratio_NT122500_Post10, #_NOwet #Med_wet

      file="Results NT RTR10_Low 20 Cap 120 lowbound 90 rratio 0.05.rData")
}

# Graphs (Manually) of ET = 10 (remember to change period)
{
# WCR=WCR_NT205000_Post10 #_NOwet #_Med_wet
# {
# png(file=paste0("Water WCR_NT202000_Post10.png")) #_NOwet #Med_wet
# plot(WCR$MineResults$PondWater,typ="l",ylim=c(0,100))
# lines(WCR$MineResults$FreshwaterToProcess, col= "orange") #difference to prices and river dif is the water to utility
# lines(WCR$MineResults$freshWaterRiver, col= "red")
# lines(WCR$Effluent$Total_Eff_Flow, col="blue")
# lines(WCR$MineResults$Watertreated, col="green")
# lines(WCR$MineResults$ProdToPoreOSPW, col="brown",lty=2)
# lines(WCR$MineResults$freshWaterToPond, col="purple")
# lines(WCR$MineResults$PoreWaterToPond, col="gray")
# lines(WCR$MineResults$NetRunoffEvap, col="brown", lty=2)
# lines(WCR$MineResults$PitLakeOutflow,col="violet")
# dev.off()
# 
# png(file=paste0("NAs WCR_NT202000_Post10.png")) #_NOwet #Med_wet
# plot(WCR$Pollution$concPollutants[,3],typ="l",ylim=c(0,80))
# lines(WCR$Effluent$Summer_PostTreatConc[,3],col="green")
# lines(WCR$Effluent$Winter_PostTreatConc[,3],col="red")
# lines(WCR$Effluent$SummerEff_Flow_Limits[,3],col="darkgreen")
# lines(WCR$Effluent$WinterEff_Flow_Limits[,3],col="blue")
# lines(1:70,rep(NAtarget,70),col="violet")
# lines(WCR$PitLake$Eff_Conc_S[,3],col="green")
# lines(51:70,WCR$PitLake$Eff_Flow_Limits[,3],col="darkgreen")
# dev.off()
# 
# png(file=paste0("TDS WCR_NT202000_Post10.png")) #_NOwet #Med_wet
# plot(WCR$Pollution$concPollutants[,1],typ="l",ylim=c(0,5000))
# lines(WCR$Effluent$Summer_PostTreatConc[,1],col="green")
# lines(WCR$Effluent$Winter_PostTreatConc[,1],col="red")
# lines(WCR$Effluent$SummerEff_Flow_Limits[,1],col="darkgreen")
# lines(1:70,rep(5000,70),col="violet")
# lines(WCR$PitLake$EndWet_treat$Postconc[,1],col="green")
# dev.off()
# }
# 
# WCR=WCR_NT202500_Post10 #_Med_wet #_NOwet
# {
# png(file=paste0("Water WCR_NT202500_Post10.png")) #_NOwet #Med_wet
# plot(WCR$MineResults$PondWater,typ="l",ylim=c(0,100))
# lines(WCR$MineResults$FreshwaterToProcess, col= "orange") #difference to prices and river dif is the water to utility
# lines(WCR$MineResults$freshWaterRiver, col= "red")
# lines(WCR$Effluent$Total_Eff_Flow, col="blue")
# lines(WCR$MineResults$Watertreated, col="green")
# lines(WCR$MineResults$ProdToPoreOSPW, col="brown",lty=2)
# lines(WCR$MineResults$freshWaterToPond, col="purple")
# lines(WCR$MineResults$PoreWaterToPond, col="gray")
# lines(WCR$MineResults$NetRunoffEvap, col="brown", lty=2)
# lines(WCR$MineResults$PitLakeOutflow,col="violet")
# dev.off()
# 
# png(file=paste0("NAs WCR_NT202500_Post10.png")) #_NOwet #Med_wet
# plot(WCR$Pollution$concPollutants[,3],typ="l",ylim=c(0,80))
# lines(WCR$Effluent$Summer_PostTreatConc[,3],col="green")
# lines(WCR$Effluent$Winter_PostTreatConc[,3],col="red")
# lines(WCR$Effluent$SummerEff_Flow_Limits[,3],col="darkgreen")
# lines(WCR$Effluent$WinterEff_Flow_Limits[,3],col="blue")
# lines(1:70,rep(NAtarget,70),col="violet")
# lines(WCR$PitLake$Eff_Conc_S[,3],col="green")
# lines(51:70,WCR$PitLake$Eff_Flow_Limits[,3],col="darkgreen")
# dev.off()
# 
# png(file=paste0("TDS WCR_NT202500_Post10.png")) #_NOwet #Med_wet
# plot(WCR$Pollution$concPollutants[,1],typ="l",ylim=c(0,5000))
# lines(WCR$Effluent$Summer_PostTreatConc[,1],col="green")
# lines(WCR$Effluent$Winter_PostTreatConc[,1],col="red")
# lines(WCR$Effluent$SummerEff_Flow_Limits[,1],col="darkgreen")
# lines(1:70,rep(2500,70),col="violet")
# lines(WCR$PitLake$EndWet_treat$Postconc[,1],col="green")
# dev.off()
# }


# New graphs
# 
# #TDS
# png(file=paste0("TDS 2500 NAs 12 20.png")) #_NOwet #Med_wet
# plot(WCR$Pollution$concPollutants[,1],typ="l",ylim=c(0,5000),ylab="TDS consentration (mg/L)")
# lines(1:70,rep(2500,70),col="green")
# WCR=WCR_NT202500_Post10
# lines(WCR$PitLake$EndWet_treat$Postconc[,1],col="green")
# WCR=WCR_NT122500_Post10
# lines(WCR$PitLake$EndWet_treat$Postconc[,1],col="darkgreen")
# dev.off()
# 
# # NAs 
# png(file=paste0("NAs 12 20 TDS 2500 .png")) #_NOwet #Med_wet
# plot(WCR$Pollution$concPollutants[,3],typ="l",ylim=c(0,80),ylab="NAs consentration (mg/L)")
# lines(1:70,rep(20,70),col="green")
# WCR=WCR_NT202500_Post10
# lines(WCR$PitLake$Eff_Conc_S[,3],col="green")
# lines(1:70,rep(12,70),col="darkgreen")
# WCR=WCR_NT122500_Post10
# lines(WCR$PitLake$Eff_Conc_S[,3],col="darkgreen")
# dev.off()


#Do the same for 10 year pitlake - Not be able to wait
}
###############################################################

# Parameters for treatments  ##############################
# Iterations
MAX = 100

# NAs targets
targets <- c(10,5,3,4,2,1) # 10,5,3,4,2,1

# TDS targets
target_tds= c(2500) # 2000, 2500


##############################################################

# Ozonation (OZ)  ##############################
tb_up=2 # upper tech bound
tb_lw=1.01

for (j in targets) {
  NAtarget=j #j #10 #8 #2 
  
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 
    
    {
      upbound=c(20,20,50,50,80,80,NAtarget,tb_up)
      lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
      
      #Chronic and Accute Limits
      
      G_a=c(TDS_target,860,NAtarget,10,50)
      G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
      
      Startpop = 50 # Number for the starting population
      StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
      StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
      StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
      
      # Reset the size for it to work To run it faster
      StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
      StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
      StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
      StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
      set.seed(20211121)
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      StartPopulation[1,]<-DecisionVariables
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
      

      #WCR$ObjectiveFunction
      
      #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
      
      penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                           WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                           WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
    }
    
    assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_OZ_Post10"),DecisionVariables)
    assign(paste0("WCR_OZ",j,i,"_Post10"),WCR)
    assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_OZ_Post10"),WCR$ObjectiveFunction)
    assign(paste0("penalty_ratio_OZ",j,i,"_Post10"),penalty_ratio)
    
  }
}

# Saving Short
{
  save (
    OptDecVariables_NATarget10_TDS2500_OZ_Post10,
    ObjFunction_NATarget10_TDS2500_OZ_Post10,
    WCR_OZ102500_Post10,
    penalty_ratio_OZ102500_Post10,
    
    OptDecVariables_NATarget5_TDS2500_OZ_Post10,
    ObjFunction_NATarget5_TDS2500_OZ_Post10,
    WCR_OZ52500_Post10,
    penalty_ratio_OZ52500_Post10,
    
    OptDecVariables_NATarget4_TDS2500_OZ_Post10,
    ObjFunction_NATarget4_TDS2500_OZ_Post10,
    WCR_OZ42500_Post10,
    penalty_ratio_OZ42500_Post10,
    
    OptDecVariables_NATarget3_TDS2500_OZ_Post10,
    ObjFunction_NATarget3_TDS2500_OZ_Post10,
    WCR_OZ32500_Post10,
    penalty_ratio_OZ32500_Post10,
    
    OptDecVariables_NATarget2_TDS2500_OZ_Post10,
    ObjFunction_NATarget2_TDS2500_OZ_Post10,
    WCR_OZ22500_Post10,
    penalty_ratio_OZ22500_Post10,
    
    OptDecVariables_NATarget1_TDS2500_OZ_Post10,
    ObjFunction_NATarget1_TDS2500_OZ_Post10,
    WCR_OZ12500_Post10,
    penalty_ratio_OZ12500_Post10,
    
    file="Results OZ 10.07.23_Cap25 rratio 0.05.rData")
}

##############################################################

# Reverse Osmeosis (RO)  ##############################
tb_up=4 # upper tech bound
tb_lw=3.01

for (j in targets) {
  NAtarget=j #j #10 #8 #2 
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 
    
    {
      upbound=c(20,20,50,50,80,80,NAtarget,tb_up)
      lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
      
      #Chronic and Accute Limits
      
      G_a=c(TDS_target,860,NAtarget,10,50)
      G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
      
      Startpop = 50 # Number for the starting population
      StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
      StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
      StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
      
      # Reset the size for it to work To run it faster
      StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
      StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
      StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
      StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
      set.seed(20211121)
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      StartPopulation[1,]<-DecisionVariables
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
      
      #WCR$ObjectiveFunction
      
      #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
      
      penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                           WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                           WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
    }
    
    assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_RO_Post10"),DecisionVariables)
    assign(paste0("WCR_RO",j,i,"_Post10"),WCR)
    assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_RO_Post10"),WCR$ObjectiveFunction)
    assign(paste0("penalty_ratio_RO",j,i,"_Post10"),penalty_ratio)
    
  }
}

# Saving shor
{
  save (
    OptDecVariables_NATarget10_TDS2500_RO_Post10,
    ObjFunction_NATarget10_TDS2500_RO_Post10,
    WCR_RO102500_Post10,
    penalty_ratio_RO102500_Post10,
    
    OptDecVariables_NATarget5_TDS2500_RO_Post10,
    ObjFunction_NATarget5_TDS2500_RO_Post10,
    WCR_RO52500_Post10,
    penalty_ratio_RO52500_Post10,
    
    OptDecVariables_NATarget4_TDS2500_RO_Post10,
    ObjFunction_NATarget4_TDS2500_RO_Post10,
    WCR_RO42500_Post10,
    penalty_ratio_RO42500_Post10,
    
        OptDecVariables_NATarget3_TDS2500_RO_Post10,
    ObjFunction_NATarget3_TDS2500_RO_Post10,
    WCR_RO32500_Post10,
    penalty_ratio_RO32500_Post10,
    
        OptDecVariables_NATarget2_TDS2500_RO_Post10,
    ObjFunction_NATarget2_TDS2500_RO_Post10,
    WCR_RO22500_Post10,
    penalty_ratio_RO22500_Post10,
    
        OptDecVariables_NATarget1_TDS2500_RO_Post10,
    ObjFunction_NATarget1_TDS2500_RO_Post10,
    WCR_RO12500_Post10,
    penalty_ratio_RO12500_Post10,
    
    file="Results RO 10.07.23_Cap25 rratio 0.05.rData")
}

##############################################################

# Wetland and Biochar (WL_BC)  ###############################
tb_up=3 # upper tech bound
tb_lw=2.01

for (j in targets) {
  NAtarget=j #j #10 #8 #2 
  
  for (i in target_tds) {
    TDS_target=i
    mod_factor = p_factor # High penalty for all scenarios
    pen_lin=400*mod_factor
    pen_quad=2000 
    
    {
      upbound=c(20,20,50,50,80,80,NAtarget,tb_up)
      lwbound=c(0,0,0,49.9,0,0,0,tb_lw)
      
      #Chronic and Accute Limits
      
      G_a=c(TDS_target,860,NAtarget,10,50)
      G_c=c(500,250,0.95,0.05,15.00) #0.75 changed to 1, 0.95
      
      Startpop = 50 # Number for the starting population
      StartPopulation<-matrix(0,ncol=8,nrow=Startpop) #nrow was 100
      StartPopulation[1,]=c(5,5,35,50,10,10,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[2,]=c(18.9,0,31,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[3,]=c(9,9,35,50,8,0,NAtarget,mean(c(tb_up,tb_lw)))
      StartPopulation[4,]=c(0,0,49,50,50,50,NAtarget,runif(1,tb_lw,tb_up))
      StartPopulation[5,]=c(6,3,20,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[6,]=c(3,2,35,50,0,10,NAtarget*0.75,runif(1,tb_lw,tb_up))
      StartPopulation[7,]=c(3,2,10,50,0,10,NAtarget,runif(1,tb_lw,tb_up))
      
      # Reset the size for it to work To run it faster
      StartPopulation[8:Startpop,1]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,2]<-runif(Startpop-7,0,20)
      StartPopulation[8:Startpop,3]<-runif(Startpop-7,5,50)
      StartPopulation[8:Startpop,4]<-rep(50,Startpop-7)
      StartPopulation[8:Startpop,5]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,6]<-runif(Startpop-7,0,50)
      StartPopulation[8:Startpop,7]<-rep(NAtarget,Startpop-7)
      StartPopulation[8:Startpop,8]<-runif(Startpop-7,tb_lw,tb_up)
      set.seed(20211121)
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      StartPopulation[1,]<-DecisionVariables
      
      system.time(GA2 <- GA_minqa_bobyqa(type = "real-valued", fitness = Neg_WaterCostModel, fitnessMin=WaterCostModel2,
                                         lower =  lwbound, upper = upbound,suggestions=StartPopulation,
                                         popSize = Startpop, maxiter = MAX , run = 100,keepBest = TRUE,
                                         optim = TRUE,pmutation=0.5,elitism=(0.05*100),
                                         optimArgs = list(poptim = 0.5,
                                                          pressel = 0.99,
                                                          control = list(fnscale = -1, maxit = 1000)),
                                         parallel =TRUE))
      
      
      DecisionVariables<-GA2@bestSol[[GA2@iter]][1,]
      WCR<-WaterCostModel2(DecisionVariables,optimiz=FALSE)
      
      #WCR$ObjectiveFunction
      
      #We can use this to substract to any other technologies to obtain the marginal cost of the technology (vs not treating anything)
      
      penalty_ratio= sum(WCR$Effluent$SummerPenalty_EffLim+WCR$Effluent$WinterPenalty_EffLims+
                           WCR$Effluent$Summer_acute_Penalty_EffLims+WCR$Effluent$Winter_acute_Penalty_EffLims+
                           WCR$PitLake$Penalty_EffLims+WCR$PitLake$acute_Penalty_EffLims+WCR$MineResults$Bit_cost)/WCR$ObjectiveFunction
      
      
    }
    
    assign(paste0("OptDecVariables_NATarget",j,"_TDS",i,"_WL_BC_Post10"),DecisionVariables)
    assign(paste0("WCR_WL_BC",j,i,"_Post10"),WCR)
    assign(paste0("ObjFunction_NATarget",j,"_TDS",i,"_WL_BC_Post10"),WCR$ObjectiveFunction)
    assign(paste0("penalty_ratio_WL_BC",j,i,"_Post10"),penalty_ratio)
    
      }
}


# Saving short
{
  save (
    OptDecVariables_NATarget10_TDS2500_WL_BC_Post10,
    ObjFunction_NATarget10_TDS2500_WL_BC_Post10,
    WCR_WL_BC102500_Post10,
    penalty_ratio_WL_BC102500_Post10,
    
        OptDecVariables_NATarget5_TDS2500_WL_BC_Post10,
    ObjFunction_NATarget5_TDS2500_WL_BC_Post10,
    WCR_WL_BC52500_Post10,
    penalty_ratio_WL_BC52500_Post10,
    
    
    OptDecVariables_NATarget4_TDS2500_WL_BC_Post10,
    ObjFunction_NATarget4_TDS2500_WL_BC_Post10,
    WCR_WL_BC42500_Post10,
    penalty_ratio_WL_BC42500_Post10,
    
    
    OptDecVariables_NATarget3_TDS2500_WL_BC_Post10,
    ObjFunction_NATarget3_TDS2500_WL_BC_Post10,
    WCR_WL_BC32500_Post10,
    penalty_ratio_WL_BC32500_Post10,
    
    
    OptDecVariables_NATarget2_TDS2500_WL_BC_Post10,
    ObjFunction_NATarget2_TDS2500_WL_BC_Post10,
    WCR_WL_BC22500_Post10,
    penalty_ratio_WL_BC22500_Post10,
    
    
    OptDecVariables_NATarget1_TDS2500_WL_BC_Post10,
    ObjFunction_NATarget1_TDS2500_WL_BC_Post10,
    WCR_WL_BC12500_Post10,
    penalty_ratio_WL_BC12500_Post10,
    
    file="Results WL_BC 10.07.23_Cap25 rratio 0.05.rData")
}




##############################################################
