# Organization of Code

## Script to execute networks pipeline
Run [this](https://github.com/khanna7/CLC-PROJECT-5/tree/test-homophily#:~:text=analyze%2Dnetworks%2Dpiple.sh) with `./analyze-networks-piple.sh` 

## Reproduce results from [Monnig 2021](https://publichealth.jmir.org/2021/11/e29319/) 
- Exploratory Analysis: [Table 2](https://github.com/khanna7/CLC-PROJECT-5/blob/master/EDA.R)
- Inferential Analysis: [Tables 3, 4, 5](https://github.com/khanna7/CLC-PROJECT-5/blob/master/reproduce-inferential-analysis.R)

## Network Analysis

- [Compare characteristics and behaviors of the 173 participants providing data vs those that did not (n=912)](https://github.com/khanna7/CLC-PROJECT-5/blob/master/compare_characteristics_fusn_consent.Rmd)
  * Compute the same characteristics and behaviors as reported in Monnig's [Table 2](https://github.com/khanna7/CLC-PROJECT-5/blob/master/EDA.R)
  * Test whether these characteristics are statistically the same between two groups that provided network information and those that did not. 
  * This is Table 1 in the working draft of the manuscript

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
  
  ### Table 2: Characteristics of Reported Network Members Stratified by Participant's CDC Guideline Adherence Score Quartiles

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


- Test association between vaccine behaviors of the network members and satisfactory COVID scores (discrete) of the participants (i.e., egos).
  * See [here](https://github.com/khanna7/CLC-PROJECT-5/blob/1d9de71440fb9f1015110800edc8b1d7a3203f89/hypothesis-ego-vs-alter-covid-behaviors.Rmd)

- Visualize the networks
  * See [here](https://github.com/khanna7/CLC-PROJECT-5/blob/1d9de71440fb9f1015110800edc8b1d7a3203f89/visualize-networks.R)

## To be Done  
- Test association between substance use behaviors and pro-vaccine behaviors/attitudes for the network members. 
  * Similar to Monnig 2021, but for the network members instead of the participants.
  
- Test association between substance use behaviors of the network members and satisfactory COVID scores (discrete) of the network members.
 

