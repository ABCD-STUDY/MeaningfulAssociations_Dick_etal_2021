#worked example using Sensemakr for sensitivity analysis of unmeasured confounding
#requires package Sensemakr.
#using the baseline data set with only those that attended with their biological mother
#worked example uses ABCD dataset 2.0.1



#data frame of the measures we're interested in using for both exposure and outcome

ABCD_SensAnalysis <- data.frame(src_subject_id, #the subject id
                         monthsbreastfed, #number of months breastfed
                         nihtbx_cryst_uncorrected, 
                         nihtbx_fluidcomp_uncorrected,
                         nihtbx_totalcomp_uncorrected,
                         interview_age.x,#child's age at baseline
                         devhx_12_weeks_premature_p, #number of weeks born premature
                         crpbi_ss_studycaregiver, #child report of parental behavior
                         race_ethnicity, #5 level race ethnicity 
                         srpf_y_ss_ses,#School Risk and Protective Factors
                         household_income_4level, #reported household income at baseline
                         married_bl, #whether the mother was married
                         gender, #child's gender
                         tobaccop, #whether the mother used tobacco during pregnancy
                         alcoholp, #whether the mother used alcohol during pregnancy
                         mother_educ, #educational attainment of the mother
                         devhx_3_age_at_birth_mother_p, #age of mother when child was born
                         acs_raked_propensity_score, #sampling weights
                         weeksprem)

#recoding of maternal education

ABCD_SensAnalysis <- within(NIHsummary, mother_educ <- relevel(mother_educ, ref = "< HS Diploma"))                         

#for this worked example we are just coding breastfeeding as a binary variable                          
ABCD_SensAnalysis$breastfed <- cut(NIHsummary$monthsbreastfed, breaks= c(0, 1, Inf), labels = c("No", "Yes"), right=FALSE)


#Sensemakr using OLS, which can be used to give a rough estimate of the strength of unmeasured confounding necessary to explain away results. 
#The linear regression model here looks at fluid intelligence. 

LM1 <- lm(nihtbx_fluidcomp_uncorrected ~ gender+breastfed+interview_age.x+race_ethnicity+
            mother_educ+household_income_4level+married_bl+tobaccop+alcoholp+devhx_3_age_at_birth_mother_p+
            weeksprem + crpbi_ss_studycaregiver+srpf_y_ss_ses,
            weights=acs_raked_propensity_score, data=ABCD_SensAnalysis)

summary(LM1)
                                         
mother_educ_vars <- grep("mother_educ", names(coef(LM1)), value = T)

#using maternal education as the benchmarking variable. We also want the reference breastfeeding group  to be the "Yes" group. 
sensitivityLM1 <- sensemakr(LM1, treatment = "breastfedYes", benchmark_covariates = list(mother_educ = mother_educ_vars), kd = 1:2)

summary(sensitivityLM1)
#Bound Label    R2dz.x R2yz.dx Treatment                Adjusted Estimate Adjusted Se Adjusted T
#1x mother_educ 0.0241  0.0524 bfcat4CMorethanTwelve            1.4114      0.2035     6.9359
#2x mother_educ 0.0483  0.1049 bfcat4CMorethanTwelve            0.6980      0.2003     3.4851
#3x mother_educ 0.0724  0.1575 bfcat4CMorethanTwelve           -0.0350      0.1968    -0.1778          -0.0880            0.0262

#different plots can be produced using Sensemakr. Estimate will show the strength to explain away the association strength. For t-value it will show 
#the strength necessary to explain away the results of significance testing. 
plot(sensitivityLM1, sensitivity.of = "estimate", lim=.1, lim.y=.1)

plot(sensitivityLM1, sensitivity.of = "t-value", lim=.1, lim.y=.1)

plot(sensitivityLM1, "extreme")

ovb_minimal_reporting(sensitivityLM1, format="latex") #format can be html or latex

#the rest is just doing the same with different outcome measures. 

LM2 <- lm(nihtbx_fluidcomp_uncorrected ~ breastfed+gender+interview_age.x+race_ethnicity+
            mother_educ+household_income_4level+married_bl+tobaccop+alcoholp+devhx_3_age_at_birth_mother_p+
            weeksprem + crpbi_ss_studycaregiver+srpf_y_ss_ses,
            weights=acs_raked_propensity_score, data=NIHsummary)
summary(LM2)


mother_educ_vars <- grep("mother_educ", names(coef(LM2)), value = T)
sensitivityLM2 <- sensemakr(LM2, treatment = "breastfedYes", benchmark_covariates = list(mother_educ = mother_educ_vars), kd = 1:2)

print(sensitivityLM2)

summary(sensitivityLM2)

plot(sensitivityLM2, sensitivity.of = "estimate", lim=.1, lim.y=.1)

plot(sensitivityLM2, sensitivity.of = "t-value", lim=.1, lim.y=.1)

plot(sensitivityLM2, "extreme")

ovb_minimal_reporting(sensitivityLM2, format="latex") #format can be html or latex

LM3 <- lm(nihtbx_totalcomp_uncorrected ~ gender+breastfed+interview_age.x+race_ethnicity+
     mother_educ+household_income_4level+married_bl+tobaccop+alcoholp+devhx_3_age_at_birth_mother_p+
     devhx_12_weeks_premature_p + crpbi_ss_studycaregiver+srpf_y_ss_ses,
   weights=acs_raked_propensity_score, data=NIHsummary)
summary(LM3)









