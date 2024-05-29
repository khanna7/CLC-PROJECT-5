# Organization of Code


## Table 1

- [Compare characteristics and behaviors of the 173 participants providing data vs those that did not (n=912)](https://github.com/khanna7/CLC-PROJECT-5/blob/master/compare_characteristics_fusn_consent.Rmd)
  * Compute the same characteristics and behaviors as reported in Monnig's [Table 2](https://github.com/khanna7/CLC-PROJECT-5/blob/master/EDA.R)
  * Test whether these characteristics are statistically the same between two groups that provided network information and those that did not. 
  * This is Table 1 in the working draft of the manuscript

## [Table 2](https://github.com/khanna7/CLC-PROJECT-5/blob/mockups/network-contact-characteristics-by-quartile.R): Characteristics of Reported Network Members Stratified by Participant's CDC Guideline Adherence Score Quartiles

- [Characteristics and behaviors of the 851 eligible network contacts](https://github.com/khanna7/CLC-PROJECT-5/blob/master/network-data-analysis.Rmd)
  * Filter by respondents (n=173) who provided network data
  * Filter out network members who are <18 years of age
  * Append mean CDC behavioral score on continuous (rnage 0-3) and discrete (>=2 = satisfactory) scale for participants 
  * Compute summaries of behaviors and demographic characteristics among all 851 network contacts, 
  stratify by CDC scores (satisfactory vs not) 
  * Compute summaries of behaviors and demographic characteristics among all network contacts, 
  but stratified by CDC scores of their nominating participants split in quartiles.  
  * Test whether the summary statistics are significantly different or not (To Be Done)
  * This is Table 2 in the working draft of the manuscript.
  

| Sample Characteristic       | Fourth Quartile (n=?) | Third Quantile (n=?) | Second  Quantile (n=?) | First Quantile |
|-----------------------------|-----------------------|----------------------|------------------------|----------------|
| Age (years), mean (sd)      |                       |                      |                        |                |
| Sex assigned at birth       |                       |                      |                        |                |
| Male: n(%)                  |                       |                      |                        |                |
| Female: n(%)                |                       |                      |                        |                |
| Not Reported: n(%)          |                       |                      |                        |                |
| Gender                      |                       |                      |                        |                |
| Man: n(%)                   |                       |                      |                        |                |
| Woman: n(%)                 |                       |                      |                        |                |
| Nonbinary: n (%)            |                       |                      |                        |                |

## [Table 3](https://github.com/khanna7/CLC-PROJECT-5/blob/paper_data/vaccine-hesitancy-ego-vs-alter-covid-behaviors.Rmd) 
   Adjusted Generalized Estimation Model for Vaccination of Participants associated with Characteristics of Social Network Contacts

## [Table 4](https://github.com/khanna7/CLC-PROJECT-5/blob/paper_data/vaccine-hesitancy-ego-vs-alter-covid-behaviors.Rmd)
    Unadjusted Generalized Estimation Model for Vaccination of Participants associated with Characteristics of Social Network Contacts (pretty/very positive vs not)


## Preliminary Stuff

### Reproducing Results from [Monnig 2021](https://publichealth.jmir.org/2021/11/e29319/)

- Exploratory Analysis: [Table 2](https://github.com/khanna7/CLC-PROJECT-5/blob/master/EDA.R)
- Inferential Analysis: [Tables 3, 4, 5](https://github.com/khanna7/CLC-PROJECT-5/blob/master/reproduce-inferential-analysis.R)
