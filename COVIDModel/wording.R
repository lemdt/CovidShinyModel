
##  ............................................................................
##  Overview of App
##  ............................................................................

about.wording <- "<h4><b>What does this tool do?</b></h4>
The app projects numbers of <i> active cases, hospitalizations, ICU-use and 
deaths</i> for COVID-19 based on local epidemiologic data under serial 
public health interventions.
                    
It can be used for regional planning for predicting hospital needs as the COVID-19 
epidemic progresses. 
                    
<br><br> 
                    
<h4><b>What inputs do I need to make a prediction?</b></h4>
After setting a date for Day 0, you need to input the <i>population of the region</i> that 
you are projecting for, as well as the estimated number of <i>current COVID-19 inpatients</i>. 
                    
<br><br> 
                    
You also will need some measure of the <i>reproductive number Re prior to Day 0</i>. This value
is defaulted to 2.8, which is the approximate reproductive number currently reported in literature. 

<br><br> 
                    
Other parameters include rates, durations, and lags for hospitalizations, ICU, and ventilator
needs. These are defaulted to values reported in literature, but can be changed by clicking on 
'Customize Other Parameters.'
                    
<br><br> 
                   
<h4><b>How does the app predict the number of Day 0 cases and infections?</b></h4>
                    
We use the percent of infections that result in hospitalization to make a prediction
into the actual number of infections. When doing this, we account for the lag time between infection 
and hospitalization, during which more infections will have occurred. 
                    
<br> <br>
                    
By using hospitalization numbers, we try avoid assumptions on detection rates and variable testing 
patterns. 
                    
<br> <br>
                    
<h4><b>What exactly does the app project?</b></h4>
                    
There are three tabs of graphs: 
                                        
<ul>
<li><b>Cases</b>: Shows the projected number of active, recovered, and total cases. </li>
<li><b>Hospitalization</b>: Shows the predicted hospital bed, ICU bed, and ventilator needs 
over time.</li>
<li><b>Hospital Resources</b>: Allows you to enter in current regional capacity for 
hospital resources, and will predict resource availability over time.</li>
</ul>
                    
<br>
                    
<h4><b>How can I take into account for public health interventions which may 
change the dynamics of the disease?</b></h4>
                     
Under the 'Add Interventions' section, you can add and save changes in Re over 
different timepoints, which will change the projections in the graphs.
"

##  ............................................................................
##  Other Header/Instructions
##  ............................................................................

estimate.re.header <- '<h4> Estimate Re based on historical hospitalizations</h4>
Provide data from past dates to estimate the Re value.<br><br>'

##  ............................................................................
##  Wording for Inputs
##  ............................................................................

hosp.input.wording <- "Estimate of current inpatients with COVID-19:"

cases.input.wording <- "Estimate of number of cases of COVID-19:"

prior.double.wording <- "Doubling Time (days) Before %s"

prior.re.wording <- "Re Before %s"

estimate.re.action.wording <- "Estimate Re prior to %s based on data."

int.double.wording <- "New Doubling Time (days) After Interventions"

int.re.wording <- "New Re After Intervention"