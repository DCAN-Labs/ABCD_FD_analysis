library(ggplot2)
library(dplyr)
library(plyr)
library(ggsci)
library(Rfast)
library(ggpubr)
library(viridis)
library(hrbrthemes)
library(lme4)
fig_width=730
fig_height=900
curr_directory = getwd()
project_directory="/mnt/rose/shared/projects/ABCD/motion_data_aggregations/"
setwd(project_directory)
site_labels <- read.csv("raw/site_labels.txt",sep=" ",header=FALSE)
motion_table <- read.csv("raw/aggregate_motion_data.txt",sep=",",header=FALSE)
subnames <- read.csv("raw/subnames.txt",sep="",header=FALSE)
demo_data <- read.csv("raw/abcd_demo_for_R.csv",sep=",",header=TRUE)
filtered_motion_table_nonan <- motion_table[is.na(motion_table[,1])==FALSE,]
site_labels_nonan <- site_labels[is.na(motion_table[,1])==FALSE,]
subnames_nonan <- subnames[is.na(motion_table[,1])==FALSE,]
demo_data_nonan <- demo_data[demo_data$subjectkey %in% subnames_nonan,]
subnames_nonan_df <- data.frame(subjectkey=subnames_nonan)
keys <- join.keys(subnames_nonan_df,demo_data_nonan,"subjectkey")
matches <- match(keys$y,keys$x,nomatch=(keys$n+1))
demo_data_nonan_matched <- demo_data_nonan[order(matches),]
scan_datespan = array(dim = length(demo_data_nonan_matched$scan_date))
for (site in unique(demo_data_nonan_matched$site_id_l)){
  min_date = min(as_date(demo_data_nonan_matched$scan_date[demo_data_nonan_matched$site_id_l == site]),na.rm=TRUE)
  scan_datespan[demo_data_nonan_matched$site_id_l == site] = as.numeric(as.duration(interval(min_date,as_date(demo_data_nonan_matched$scan_date[demo_data_nonan_matched$site_id_l == site]))),"months")
}
motion_data <- data.frame(age = as.factor(demo_data_nonan_matched$interview_age),
                          gender = as.factor(demo_data_nonan_matched$gender),
                          parent_education = cut(demo_data_nonan_matched$HIGHEST_PARENT_ED_DCAN,c(3,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),labels=7:21),
                          race = factor(demo_data_nonan_matched$combined_race_DCAN,labels=c("white","black","AIAN","NHPI","asian","2+ races","other","unknown")),
                          ethnicity=as.factor(demo_data_nonan_matched$demo_race_latinx_DCAN),
                          ID = as.factor(subnames_nonan),
                          meanFD = filtered_motion_table_nonan[,1], 
                          site=as.factor(demo_data_nonan_matched$site_id_l),
                          minutes_at_FD_0p2 = filtered_motion_table_nonan[,4]*0.8/60,
                          minutes_at_FD_0p2_nooutlier = filtered_motion_table_nonan[,3]*0.8/60,
                          pc1 = as.factor(round(demo_data_nonan_matched$pc1_new)),
                          pc2 = as.factor(round(demo_data_nonan_matched$pc2_new)),
                          pc3 = as.factor(round(demo_data_nonan_matched$pc3_new)),
                          Ecbcl = demo_data_nonan_matched$cbcl_scr_syn_external_t,
                          Icbcl = demo_data_nonan_matched$cbcl_scr_syn_internal_t,
                          wisc = demo_data_nonan_matched$pea_wiscv_tss,
                          month = as.factor(demo_data_nonan_matched$scan_month),
                          hour = as.factor(round(demo_data_nonan_matched$scan_time,digits = -4)/10000),
                          span = as.factor(round(scan_datespan)))
motion_data_fit <- data.frame(age = demo_data_nonan_matched$interview_age,
                          gender = demo_data_nonan_matched$gender,
                          parent_education = demo_data_nonan_matched$HIGHEST_PARENT_ED_DCAN,
                          race = as.factor(demo_data_nonan_matched$combined_race_DCAN),
                          ethnicity=demo_data_nonan_matched$demo_race_latinx_DCAN,
                          ID = factor(subnames_nonan,levels=subnames_nonan),
                          meanFD = filtered_motion_table_nonan[,1], 
                          site=as.factor(demo_data_nonan_matched$site_id_l),
                          minutes_at_FD_0p2 = filtered_motion_table_nonan[,4]*0.8/60,
                          minutes_at_FD_0p2_nooutlier = filtered_motion_table_nonan[,3]*0.8/60,
                          pc1 = demo_data_nonan_matched$pc1_new,
                          pc2 = demo_data_nonan_matched$pc2_new,
                          pc3 = demo_data_nonan_matched$pc3_new,
                          Ecbcl = demo_data_nonan_matched$cbcl_scr_syn_external_t,
                          Icbcl = demo_data_nonan_matched$cbcl_scr_syn_internal_t,
                          wisc = demo_data_nonan_matched$pea_wiscv_tss,
                          month = as.factor(demo_data_nonan_matched$scan_month),
                          hour = as.factor(round(demo_data_nonan_matched$scan_time,digits = -4)/10000),
                          span = scan_datespan)
motion_data_fit_nonan <- motion_data_fit[rowsums(is.na(motion_data_fit)) == 0,]
motion_data_fit_no_pe_unknown <- motion_data_fit_nonan[motion_data_fit_nonan$parent_education != 777,]
motion_data_fit_no_pe_race <- motion_data_fit_no_pe_unknown[motion_data_fit_no_pe_unknown$race != 777,]
motion_data_fit_noeth999 <- motion_data_fit_no_pe_race[motion_data_fit_no_pe_race$ethnicity != 777,]
motion_data_fit_clean <- motion_data_fit_noeth999[motion_data_fit_noeth999$ethnicity != 999,]
# Below models show no effect of most factors
temp_model <- lm(meanFD ~ age,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ gender,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ parent_education,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ race,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ ethnicity,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ site,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ pc1,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ pc2,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ pc3,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ Ecbl,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ Icbl,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ wisc,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ month,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ hour,motion_data_fit_clean)
summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ span,motion_data_fit_clean)
summary(temp_model)$r.squared
#omnibus anova shows 10 initial factors may contribute
omnibus_meanFD <- lm(meanFD ~ age+parent_education+race+site+pc1+pc2+pc3+gender+Ecbcl+Icbcl+ethnicity+wisc+month+hour+span,motion_data_fit_clean)
anova(omnibus_meanFD)
age_pe_site_pc2_model <- lm(meanFD ~ age +parent_education +site +pc2,motion_data_fit_clean)
anova(age_pe_site_model,age_pe_site_pc2_model)
age_pe_site_pc2_gender_model <- lm(meanFD ~ age +parent_education +site +pc2 + gender,motion_data_fit_clean)
anova(age_pe_site_pc2_model,age_pe_site_pc2_gender_model)
age_pe_site_pc2_gender_race_model <- lm(meanFD ~ age +parent_education +site +pc2 + gender + race,motion_data_fit_clean)
anova(age_pe_site_pc2_gender_model,age_pe_site_pc2_gender_race_model)
age_pe_site_pc2_gender_race_pc1_model <- lm(meanFD ~ age +parent_education +site +pc2 + gender + race + pc1,motion_data_fit_clean)
anova(age_pe_site_pc2_gender_race_model,age_pe_site_pc2_gender_race_pc1_model)
age_pe_site_pc2_gender_race_pc1_ecbcl_model <- lm(meanFD ~ age +parent_education +site +pc2 + gender + race + pc1 + Ecbcl,motion_data_fit_clean)
anova(age_pe_site_pc2_gender_race_pc1_model,age_pe_site_pc2_gender_race_pc1_ecbcl_model)
age_pe_site_pc2_gender_race_pc1_ecbcl_icbcl_model <- lm(meanFD ~ age +parent_education +site +pc2 + gender + race + pc1 + Ecbcl + Icbcl,motion_data_fit_clean)
anova(age_pe_site_pc2_gender_race_pc1_ecbcl_model,age_pe_site_pc2_gender_race_pc1_ecbcl_icbcl_model)
age_pe_site_pc2_gender_race_pc1_ecbcl_enth_model <- lm(meanFD ~ age +parent_education +site +pc2 + gender + race + pc1 + Ecbcl + ethnicity,motion_data_fit_clean)
anova(age_pe_site_pc2_gender_race_pc1_ecbcl_model,age_pe_site_pc2_gender_race_pc1_ecbcl_enth_model)
age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model <- lm(meanFD ~ age +parent_education +site +pc2 + gender + race + pc1 + Ecbcl + ethnicity+wisc,motion_data_fit_clean)
anova(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_model,age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)
#determine proportion of variance explained by metric -- add univariate as well to this
effect_array = array(data = NA,dim = 20)
effect_name = c("wisc","ethnicity","ecbcl","pc1","race","gender","pc2","site","parent education","age","wisc","ethnicity","ecbcl","pc1","race","gender","pc2","site","parent education","age")
effect_model = rep(c("full","univariate"),1,each=10)
effect_array[1] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_model)$r.squared
age_pe_site_pc2_gender_race_pc1_ecbcl_wisc_model <- lm(meanFD ~ age + parent_education +site +pc2 + gender + race + pc1 + Ecbcl + wisc,motion_data_fit_clean)
effect_array[2] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_site_pc2_gender_race_pc1_ecbcl_wisc_model)$r.squared
age_pe_site_pc2_gender_race_pc1_ethn_wisc_model <- lm(meanFD ~ age + parent_education +site +pc2 + gender + race + pc1 + ethnicity+wisc,motion_data_fit_clean)
effect_array[3] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_site_pc2_gender_race_pc1_ethn_wisc_model)$r.squared
age_pe_site_pc2_gender_race_ecbcl_ethn_wisc_model <- lm(meanFD ~ age + parent_education +site + pc2 + gender + race + Ecbcl + ethnicity + wisc,motion_data_fit_clean)
effect_array[4] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_site_pc2_gender_race_ecbcl_ethn_wisc_model)$r.squared
age_pe_site_pc2_gender_pc1_ecbcl_ethn_wisc_model <- lm(meanFD ~ age + parent_education +site +pc2 + gender + pc1 + Ecbcl + ethnicity + wisc,motion_data_fit_clean)
effect_array[5] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_site_pc2_gender_pc1_ecbcl_ethn_wisc_model)$r.squared
age_pe_site_pc2_race_pc1_ecbcl_ethn_wisc_model <- lm(meanFD ~ age + parent_education + site + pc2 + race + pc1 + Ecbcl + ethnicity + wisc,motion_data_fit_clean)
effect_array[6] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_site_pc2_race_pc1_ecbcl_ethn_wisc_model)$r.squared
age_pe_site_gender_race_pc1_ecbcl_ethn_wisc_model <- lm(meanFD ~ age + parent_education + site + gender + race + pc1 + Ecbcl + ethnicity + wisc,motion_data_fit_clean)
effect_array[7] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_site_gender_race_pc1_ecbcl_ethn_wisc_model)$r.squared
age_pe_pc2_gender_race_pc1_ecbcl_ethn_wisc_model <- lm(meanFD ~ age + parent_education + pc2 + gender + race + pc1 + Ecbcl + ethnicity + wisc,motion_data_fit_clean)
effect_array[8] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_pe_pc2_gender_race_pc1_ecbcl_ethn_wisc_model)$r.squared
age_site_pc2_gender_race_pc1_ecbcl_ethn_wisc_model <- lm(meanFD ~ age + site + pc2 + gender + race + pc1 + Ecbcl + ethnicity + wisc,motion_data_fit_clean)
effect_array[9] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(age_site_pc2_gender_race_pc1_ecbcl_ethn_wisc_model)$r.squared
pe_site_pc2_gender_race_pc1_ecbcl_ethn_wisc_model <- lm(meanFD ~ parent_education + site + pc2 + gender + race + pc1 + Ecbcl + ethnicity + wisc,motion_data_fit_clean)
effect_array[10] <- summary(age_pe_site_pc2_gender_race_pc1_ecbcl_enth_wisc_model)$r.squared - summary(pe_site_pc2_gender_race_pc1_ecbcl_ethn_wisc_model)$r.squared

#redo univariate calculations and store them
univariate_effect_array = array(data = NA,dim = 10)
temp_model <- lm(meanFD ~ wisc,motion_data_fit_clean)
effect_array[11] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ ethnicity,motion_data_fit_clean)
effect_array[12] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ Ecbl,motion_data_fit_clean)
effect_array[13] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ pc1,motion_data_fit_clean)
effect_array[14] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ race,motion_data_fit_clean)
effect_array[15] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ gender,motion_data_fit_clean)
effect_array[16] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ pc2,motion_data_fit_clean)
effect_array[17] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ site,motion_data_fit_clean)
effect_array[18] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ parent_education,motion_data_fit_clean)
effect_array[19] = summary(temp_model)$r.squared
temp_model <- lm(meanFD ~ age,motion_data_fit_clean)
effect_array[20] = summary(temp_model)$r.squared

effsize_table <- data.frame(rsquared = effect_array,measure = effect_name,type = effect_model)
#make bubble plots

bubble_plot_base_text_size = -20
bubble_plot_text_x_size <- bubble_plot_base_text_size + 36
bubble_plot_text_y_size <- bubble_plot_base_text_size + 36
bubble_plot_title_x_size <- bubble_plot_base_text_size + 40
bubble_plot_title_y_size <- bubble_plot_base_text_size + 40
bubble_plot_legend_text_size <- bubble_plot_base_text_size + 32
bubble_plot_legend_title_size <- bubble_plot_base_text_size + 34
sqrd <- scales::trans_new("sqrd",function(x){x^2},sqrt)
effsizepp <- ggplot(data = effsize_table)+aes(x=type,y=measure,size=rsquared,color=effect_name) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey",trans="log10") + 
  scale_size(range = c(1.4,19),name="ratio") +
  theme_classic2() +
  scale_y_discrete(name = "variable") +
  scale_x_discrete(name = "model")+
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/effect_of_factors_on_meanFD.png",width=fig_width,height=fig_height)
effsizepp
dev.off()
#Below shows the model where all combined values show a significant effect (P < 0.05)
age_model <- lm(meanFD ~ age,motion_data_fit_clean)
age_pe_model <- lm(meanFD ~ age + parent_education,motion_data_fit_clean)
age_pe_site_model <- lm(meanFD ~ age + parent_education + site,motion_data_fit_clean)
age_pe_site_gender_model <- lm(meanFD ~ age + parent_education + site + gender,motion_data_fit_clean)
age_pe_site_gender_pc1_model <- lm(meanFD ~ age + parent_education + site + gender + pc1,motion_data_fit_clean)
age_pe_site_gender_pc1_pc2_model <- lm(meanFD ~ age + parent_education + site + gender + pc1 + pc2,motion_data_fit_clean)
age_pe_site_gender_pc1_pc2_siteXpc1_model <- lm(meanFD ~ age + parent_education + site + gender + pc1 + pc2 + pc1*site,motion_data_fit_clean)

#Below shows the significant comparisons of each added parameter
anova(age_model,age_pe_model)
anova(age_pe_model,age_pe_site_model)
anova(age_pe_site_model,age_pe_site_gender_model)
anova(age_pe_site_gender_model,age_pe_site_gender_pc1_model)
anova(age_pe_site_gender_pc1_model,age_pe_site_gender_pc1_pc2_model)
anova(age_pe_site_gender_pc1_pc2_model,age_pe_site_gender_pc1_pc2_siteXpc1_model)

#Below are general models showing the same effect of motion despite different measures
motion_meanFD_lm <- lm(meanFD ~ age + 
                   parent_education + 
                   site + gender + pc1 + pc2 + pc1*site,motion_data_fit_clean)
motion_minutes_at_FD_0p2_lm <- lm(minutes_at_FD_0p2 ~ age + 
                                    parent_education + 
                                    site + gender + pc1 + pc2 + site*pc1,motion_data_fit_clean)
motion_minutes_at_FD_0p2_nooutlier_lm <- lm(minutes_at_FD_0p2_nooutlier ~ age + 
                                              parent_education + 
                                              site + gender + pc1 + pc2 + site*pc1,motion_data_fit_clean)
summary(motion_meanFD_lm)
summary(motion_minutes_at_FD_0p2_lm)
summary(motion_minutes_at_FD_0p2_nooutlier_lm)
meanframes_by_site <- ddply(motion_data,"site",summarise,mean=mean(minutes_at_FD_0p2,na.rm=TRUE))
meanframes_by_race <- ddply(motion_data,"race",summarise,mean=mean(minutes_at_FD_0p2,na.rm=TRUE))
meanframes_by_age <- ddply(motion_data,"age",summarise,mean=mean(minutes_at_FD_0p2,na.rm=TRUE))
meanFD_by_site <- ddply(motion_data,"site",summarise,mean=mean(meanFD,na.rm=TRUE))

#make FD plots

#make cumulative distribution plots
cumu_plot_base_text_size = 12
cumu_plot_text_x_size <- cumu_plot_base_text_size + 36
cumu_plot_text_y_size <- cumu_plot_base_text_size + 36
cumu_plot_title_x_size <- cumu_plot_base_text_size + 40
cumu_plot_title_y_size <- cumu_plot_base_text_size + 40
cumu_plot_legend_text_size <- cumu_plot_base_text_size + 32
cumu_plot_legend_title_size <- cumu_plot_base_text_size + 34
#site cumulative plot
temp.sites <- ddply(motion_data, .(site),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.below <- temp.sites[which(temp.sites$cumulative_distribution<0.2),]
png("analyses/cumu_dist_by_site_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.sites,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=site))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=site),alpha=1,size=3)+
  theme_classic2()+
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size-4),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.75,0.4))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=2))
dev.off()
#race cumulative plot
temp.race <- ddply(motion_data, .(race),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.below <- temp.race[which(temp.sites$cumulative_distribution<0.2),]
png("analyses/cumu_dist_by_race_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.race,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=race))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=race),alpha=1,size=3)+
  theme_classic2()+
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.79,0.26))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=1))
dev.off()
#gender cumulative plot
motion_data_gender <- motion_data[is.na(motion_data$gender)==FALSE,]
temp.gender <- ddply(motion_data_gender, .(gender),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.below <- temp.gender[which(temp.sites$cumulative_distribution<0.2),]
png("analyses/cumu_dist_by_gender_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.gender,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=gender))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=gender),alpha=1,size=3)+
  theme_classic2()+
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.75,0.5))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=1))
dev.off()

#age cumulative plot
motion_data_age <- motion_data[is.na(motion_data$age)==FALSE,]
temp.age <- ddply(motion_data_age, .(age),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.age$age <- as.numeric(as.character(temp.age$age))
temp.below <- temp.age[which(temp.sites$cumulative_distribution<0.2),] #can be used for shading a portion of the plot
png("analyses/cumu_dist_by_age_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.age,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=age))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=age),alpha=0.5,size=3)+
  theme_classic2()+
  scale_color_gsea()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.75,0.5))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=1))
dev.off()

#span cumulative plot
motion_data_span <- motion_data[is.na(motion_data$span)==FALSE,]
temp.span <- ddply(motion_data_span, .(span),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.span$span <- as.numeric(as.character(temp.span$span))
temp.below <- temp.span[which(temp.sites$cumulative_distribution<0.2),] #can be used for shading a portion of the plot
png("analyses/cumu_dist_by_span_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.span,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=span))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=span),alpha=0.5,size=3)+
  theme_classic2()+
  scale_color_gsea()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.75,0.5))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=1))
dev.off()

#pc1 cumulative plot
motion_data_pc1 <- motion_data[is.na(motion_data$pc1)==FALSE,]
temp.pc1 <- ddply(motion_data_pc1, .(pc1),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.pc1$pc1 <- as.numeric(as.character(temp.pc1$pc1))
temp.below <- temp.pc1[which(temp.sites$cumulative_distribution<0.2),] #can be used for shading a portion of the plot
png("analyses/cumu_dist_by_pc1_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.pc1,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=pc1))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=pc1),alpha=0.5,size=3)+
  theme_classic2()+
  scale_color_gsea()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.75,0.5))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=1))
dev.off()

#pc2 cumulative plot
motion_data_pc2 <- motion_data[is.na(motion_data$pc2)==FALSE,]
temp.pc2 <- ddply(motion_data_pc2, .(pc2),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.pc2$pc2 <- as.numeric(as.character(temp.pc2$pc2))
temp.below <- temp.pc2[which(temp.sites$cumulative_distribution<0.2),] #can be used for shading a portion of the plot
png("analyses/cumu_dist_by_pc2_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.pc2,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=pc2))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=pc2),alpha=0.5,size=3)+
  theme_classic2()+
  scale_color_gsea()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.75,0.5))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=1))
dev.off()

#parent education cumulative plot
motion_data_parent_education <- motion_data[is.na(motion_data$parent_education)==FALSE,]
temp.parent_education <- ddply(motion_data_parent_education, .(parent_education),summarize,minutes_at_FD_0p2 = unique(minutes_at_FD_0p2),cumulative_distribution=ecdf(minutes_at_FD_0p2)(unique(minutes_at_FD_0p2)))
temp.parent_education$parent_ed <- as.numeric(as.character(temp.parent_education$parent_education))
temp.below <- temp.parent_education[which(temp.parent_education$cumulative_distribution<0.2),] #can be used for shading a portion of the plot
png("analyses/cumu_dist_by_parent_ed_FD0p2.png",width=fig_width,height=fig_height)
ggplot(temp.parent_education,aes(x=minutes_at_FD_0p2,y=cumulative_distribution,color=parent_ed))+
  geom_line(aes(ymin=0,ymax=cumulative_distribution,colour=parent_ed),alpha=0.5,size=3)+
  theme_classic2()+
  scale_color_gsea()+
  theme(axis.text.x = element_text(size=cumu_plot_text_x_size),
        axis.text.y = element_text(size=cumu_plot_text_y_size),
        axis.title.x = element_text(size=cumu_plot_title_x_size),
        axis.title.y = element_text(size=cumu_plot_title_y_size),
        legend.text = element_text(size=cumu_plot_legend_text_size),
        legend.title = element_text(size=cumu_plot_legend_title_size),
        legend.position = c(0.75,0.38))+
  scale_x_continuous(breaks=c(0,10,20,28))+
  guides(color=guide_legend(ncol=1))
dev.off()

#make bubble plots

bubble_plot_base_text_size = 12
bubble_plot_text_x_size <- bubble_plot_base_text_size + 36
bubble_plot_text_y_size <- bubble_plot_base_text_size + 36
bubble_plot_title_x_size <- bubble_plot_base_text_size + 40
bubble_plot_title_y_size <- bubble_plot_base_text_size + 40
bubble_plot_legend_text_size <- bubble_plot_base_text_size + 32
bubble_plot_legend_title_size <- bubble_plot_base_text_size + 34
sqrd <- scales::trans_new("sqrd",function(x){x^2},sqrt)
#site bubble plot
motion_summary_initial <- tapply(motion_data$site,cut(motion_data$minutes_at_FD_0p2,4),function(x) summary(x))
site_names <- names(motion_summary_initial[[1]])[1:21]
nbins = length(motion_summary_initial)
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
site_totals <- rowsums(motion_array_summary)

sqrd <- scales::trans_new("sqrd",function(x){x^2},sqrt)

bins <- seq(0,1,by=0.1)
motion_summary_initial <- tapply(motion_data$site,cut(motion_data$meanFD,breaks = bins),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:21]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=meanframes_by_site$site[sort(meanframes_by_site$mean,index.return=TRUE)$ix])
bin_name_vector <- rep(c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"),length(site_names))
motion_df <- data.frame(site=site_name_vector,meanFD=bin_name_vector,ratio=motion_data_vector)
meanFDpp <- ggplot(data = motion_df)+aes(x=meanFD,y=site,size=ratio,color=site) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "site") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/meanFD_by_site.png",width=fig_width,height=fig_height)
meanFDpp
dev.off()

motion_summary_initial <- tapply(motion_data$site,cut(motion_data$minutes_at_FD_0p2,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:21]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=meanframes_by_site$site[sort(meanframes_by_site$mean,index.return=TRUE)$ix])
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(site=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=site,size=ratio,color=site) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "site") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_site.png",width=fig_width,height=fig_height)
nframepp
dev.off()

motion_summary_initial <- tapply(motion_data$site,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:21]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=meanframes_by_site$site[sort(meanframes_by_site$mean,index.return=TRUE)$ix])
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(site=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=site,size=ratio,color=site) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "site") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_site_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

motion_summary_initial <- tapply(motion_data$site,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:21]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=meanframes_by_site$site[sort(meanframes_by_site$mean,index.return=TRUE)$ix])
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(site=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=site,size=ratio,color=site) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "site") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_site_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

#make race plots
motion_summary_initial <- tapply(motion_data$race,cut(motion_data$minutes_at_FD_0p2,4),function(x) summary(x))
site_names <- names(motion_summary_initial[[1]])
nbins = length(motion_summary_initial)
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
site_totals <- rowsums(motion_array_summary)

motion_summary_initial <- tapply(motion_data$race,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=meanframes_by_race$race[sort(meanframes_by_race$mean,index.return=TRUE)$ix])
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(race=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=race,size=ratio,color=race) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "race") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_race_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

bins <- seq(0,1,by=0.1)
motion_summary_initial <- tapply(motion_data$race,cut(motion_data$meanFD,breaks = bins),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=meanframes_by_race$race[sort(meanframes_by_race$mean,index.return=TRUE)$ix])
bin_name_vector <- rep(c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"),length(site_names))
motion_df <- data.frame(race=site_name_vector,meanFD=bin_name_vector,ratio=motion_data_vector)
meanFDpp <- ggplot(data = motion_df)+aes(x=meanFD,y=race,size=ratio,color=race) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "race") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/meanFD_by_race.png",width=fig_width,height=fig_height)
meanFDpp
dev.off()

#make age plots
motion_summary_initial <- tapply(motion_data$age,cut(motion_data$minutes_at_FD_0p2,4),function(x) summary(x))
site_names <- names(motion_summary_initial[[1]])
nbins = length(motion_summary_initial)
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
site_totals <- rowsums(motion_array_summary)

motion_summary_initial <- tapply(motion_data$age,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)))
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(age=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=age,size=ratio,color=age) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "age") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_age_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

bins <- seq(0,1,by=0.1)
motion_summary_initial <- tapply(motion_data$age,cut(motion_data$meanFD,breaks = bins),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)))
bin_name_vector <- rep(c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"),length(site_names))
motion_df <- data.frame(age=site_name_vector,meanFD=bin_name_vector,ratio=motion_data_vector)
meanFDpp <- ggplot(data = motion_df)+aes(x=meanFD,y=age,size=ratio,color=age) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "age") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/meanFD_by_age.png",width=fig_width,height=fig_height)
meanFDpp
dev.off()

#make pc1 plots
motion_summary_initial <- tapply(motion_data$pc1,cut(motion_data$minutes_at_FD_0p2,4),function(x) summary(x))
site_names <- names(motion_summary_initial[[1]])[1:7]
nbins = length(motion_summary_initial)
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
site_totals <- rowsums(motion_array_summary)

motion_summary_initial <- tapply(motion_data$pc1,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:7]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=c("-3","-2","-1","0","1","2","3"))
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(pc1=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=pc1,size=ratio,color=pc1) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "pc1") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_pc1_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

bins <- seq(0,1,by=0.1)
motion_summary_initial <- tapply(motion_data$pc1,cut(motion_data$meanFD,breaks = bins),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:7]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=c("-3","-2","-1","0","1","2","3"))
bin_name_vector <- rep(c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"),length(site_names))
motion_df <- data.frame(pc1=site_name_vector,meanFD=bin_name_vector,ratio=motion_data_vector)
meanFDpp <- ggplot(data = motion_df)+aes(x=meanFD,y=pc1,size=ratio,color=pc1) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "pc1") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/meanFD_by_pc1.png",width=fig_width,height=fig_height)
meanFDpp
dev.off()

#make pc2 plots
motion_data$pc2[motion_data$pc2 == 3] <- 2 #set to eliminate a category for a single subject, leading to misleading plots
motion_summary_initial <- tapply(motion_data$pc2,cut(motion_data$minutes_at_FD_0p2,4),function(x) summary(x))
site_names <- names(motion_summary_initial[[1]])[1:7]
nbins = length(motion_summary_initial)
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
site_totals <- rowsums(motion_array_summary)

motion_summary_initial <- tapply(motion_data$pc2,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:6]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=c("-3","-2","-1","0","1","2"))
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(pc2=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=pc2,size=ratio,color=pc2) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "pc2") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_pc2_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

bins <- seq(0,1,by=0.1)
motion_summary_initial <- tapply(motion_data$pc2,cut(motion_data$meanFD,breaks = bins),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:6]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=c("-3","-2","-1","0","1","2"))
bin_name_vector <- rep(c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"),length(site_names))
motion_df <- data.frame(pc2=site_name_vector,meanFD=bin_name_vector,ratio=motion_data_vector)
meanFDpp <- ggplot(data = motion_df)+aes(x=meanFD,y=pc2,size=ratio,color=pc2) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "pc2") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/meanFD_by_pc2.png",width=fig_width,height=fig_height)
meanFDpp
dev.off()

#make education plots
motion_summary_initial <- tapply(motion_data$parent_education,cut(motion_data$minutes_at_FD_0p2,4),function(x) summary(x))
site_names <- names(motion_summary_initial[[1]])[1:18]
nbins = length(motion_summary_initial)
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
site_totals <- rowsums(motion_array_summary)

motion_summary_initial <- tapply(motion_data$parent_education,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:15]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(parent_ed=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=parent_ed,size=ratio,color=parent_ed) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "parent education") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_education_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

bins <- seq(0,1,by=0.1)
motion_summary_initial <- tapply(motion_data$parent_education,cut(motion_data$meanFD,breaks = bins),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:15]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- factor(rep(site_names,each=length(motion_summary_initial)),levels=c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))
bin_name_vector <- rep(c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"),length(site_names))
motion_df <- data.frame(parent_ed=site_name_vector,meanFD=bin_name_vector,ratio=motion_data_vector)
meanFDpp <- ggplot(data = motion_df)+aes(x=meanFD,y=parent_ed,size=ratio,color=parent_ed) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "parent education") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/meanFD_by_education.png",width=fig_width,height=fig_height)
meanFDpp
dev.off()

#do gender plots
motion_summary_initial <- tapply(motion_data$gender,cut(motion_data$minutes_at_FD_0p2,4),function(x) summary(x))
site_names <- names(motion_summary_initial[[1]])[1:2]
nbins = length(motion_summary_initial)
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
site_totals <- rowsums(motion_array_summary)

motion_summary_initial <- tapply(motion_data$gender,cut(motion_data$minutes_at_FD_0p2_nooutlier,breaks = c(0,2,4,6,8,10,12,14,16,18,20)),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:2]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- rep(site_names,each=length(motion_summary_initial))
bin_name_vector <- factor(rep(c("2","4","6","8","10","12","14","16","18","20"),length(site_names)),levels=c("2","4","6","8","10","12","14","16","18","20"))
motion_df <- data.frame(gender=site_name_vector,minute_at_FD_0p2=bin_name_vector,ratio=motion_data_vector)
nframepp <- ggplot(data = motion_df)+aes(x=minute_at_FD_0p2,y=gender,size=ratio,color=gender) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "gender") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/remaining_frames_by_gender_nooutlier.png",width=fig_width,height=fig_height)
nframepp
dev.off()

bins <- seq(0,1,by=0.1)
motion_summary_initial <- tapply(motion_data$gender,cut(motion_data$meanFD,breaks = bins),function(x) summary(x))
nbins = length(motion_summary_initial)
site_names <- names(motion_summary_initial[[1]])[1:2]
motion_array_summary <- array(data = NA,dim = c(length(site_names),nbins))
for (x in 1:length(motion_summary_initial)){motion_array_summary[,x] = unlist(motion_summary_initial[x])[1:length(site_names)] }
for (x in 1:length(site_names)){motion_array_summary[x,] = motion_array_summary[x,]/site_totals[x] }
motion_data_vector <- as.vector(t(motion_array_summary))
site_name_vector <- rep(site_names,each=length(motion_summary_initial))
bin_name_vector <- rep(c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"),length(site_names))
motion_df <- data.frame(gender=site_name_vector,meanFD=bin_name_vector,ratio=motion_data_vector)
meanFDpp <- ggplot(data = motion_df)+aes(x=meanFD,y=gender,size=ratio,color=gender) + 
  geom_point(alpha=0.7) + geom_point(shape=1,colour="grey") + 
  scale_size(range = c(1.4,19),name="ratio",trans=sqrd) +
  theme_classic2() +
  scale_y_discrete(name = "gender") +
  scale_color_ucscgb()+
  theme(axis.text.x = element_text(size=bubble_plot_text_x_size,angle=-90,hjust = 0,vjust=0.5),
        axis.text.y = element_text(size=bubble_plot_text_y_size),
        axis.title.x = element_text(size=bubble_plot_title_x_size),
        axis.title.y = element_text(size=bubble_plot_title_y_size),
        legend.text = element_text(size=bubble_plot_legend_text_size),
        legend.title = element_text(size=bubble_plot_legend_title_size))
png("analyses/meanFD_by_gender.png",width=fig_width,height=fig_height)
meanFDpp
dev.off()
setwd(curr_directory)