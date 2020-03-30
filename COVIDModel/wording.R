##  ............................................................................
## Compilation of all of the strings that are outputted in the app.
##  ............................................................................

##  ............................................................................
##  General
##  ............................................................................

app.title <- "COVID-19 Epidemic Modeling (Model 2 - Markov - Pre-Release)"

about.link.wording <- "Learn more about this tool."

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

end.notes <- '<br><br><b>Notes</b>: This app is a modified version of the <a href="http://penn-chime.phl.io/">Penn Chime app</a>.
This is a beta version - the projections may or may not be accurate.
<br><br> The code for this tool is on <a href="https://github.com/jpspeng/CovidShinyModel">Github</a>.'


##  ............................................................................
##  Warning/Notifications
##  ............................................................................

re.warning.date.repeat <- "This date has already been added."

re.warning.blank.num <- "Please enter the hospitalization number."

re.warning.more.data <- "You need at least two timepoints of data to make a prediction."

best.re.msg <- "<br><br><h4>The best estimate for Re is <b>%s</b>."

params.save.msg <- "Save and Close"

double.int.warning <- "You have already added an intervention on this date."

##  ............................................................................
##  Datatable Messages
##  ............................................................................

re.blank.table <- "No historical data added."

search.msg <- "Find in table:"

##  ............................................................................
## Headers/Instructions/Misc
##  ............................................................................

location.header <- '<h4><b>Location Information</b></h4>'

int.header <- '<h4><b>Add Interventions</b></h4>'

add.int.cb.wording <- 'Add Intervention'

influx.header <- '<h4><b>Add Influx of Infections</b></h4>'

influx.cb.wording <- 'Add Influx of Infected Individuals'

use.double.wording <- 'Use doubling time instead of Re'

settings.wording <- '<h4><b>Settings</b></h4>'

cust.params.wording <- 'Customize Other Parameters'

proj.header <- '<h3><b>Projections</b></h3>'

download.link.wording <-"Download as CSV"

estimate.re.header <- "<h4> Estimate Re based on historical hospitalizations</h4>
Provide data from past dates to estimate the Re value.<br><br>"

##  ............................................................................
##  Wording for Inputs
##  ............................................................................

num.people.wording <- "Number of People in Area"

input.metric.wording <- "Input Metric:"

curr.date.wording <- "On Date:"

proj.days.wording <- 'Number of Days to Project'

hosp.input.wording <- "Estimate of current inpatients with COVID-19:"

cases.input.wording <- "Estimate of number of cases of COVID-19:"

prior.double.wording <- "Doubling Time (days) Before %s"

prior.re.wording <- "Re Before %s"

estimate.re.action.wording <- "Estimate Re prior to %s based on data."

int.double.wording <- "New Doubling Time (days) After Interventions"

int.re.wording <- "New Re After Intervention"

incubation.input.wording <- "Latent period from infection to becoming infectious (days)"

infectious.input.wording <- "Number of days an infected person is infectious"

per.hosp.wording <- "Percent Hospitalized Among Infections"

per.icu.wording <- "Percent ICU Admitted Among Hospitalized"

per.vent.wording <- "Percent Ventilated Among ICU Admissions"

inf.to.hosp.wording <- "Number of days from infection to hospitalization"

hosp.to.icu.wording <- "Number of days from hospitalization to ICU Admission"

icu.to.vent.wording <- "Number of days from ICU admission to ventilation"

hosp.los.wording <- "Hospital Length of Stay for Non-ICU Patients (days)"

icu.los.wording <- "ICU Length of Stay for Non-Ventilated Patients (days)"

vent.los.wording <- "Average time on a ventilator (days)"

int.date.wording <-  "Date Intervention is Implemented"

int.smooth.wording <- "Smoothed over how many days?"

save.int.wording <- "Save Intervention"

avail.hosp.wording <- "Hospital Bed Availability"

avail.icu.wording <- "ICU Space Availability"

avail.vent.wording <- "Ventilator Availability"

influx.date.wording <- "Date of Influx"

influx.num.wording <-  "Number of Infected Entering Region"


##  ............................................................................
##  Wording for Inputs - Model 2 Specific
##  ............................................................................

g.trans.prob.head <- '<br><h4>For an infected person that is in the non-ICU, 
on a given day, what is the chance that - on the next day - they will:</h4>'

icu.trans.prob.head <- '<br><h4>For an infected person in the ICU, on a given day, 
what is the chance that - on the next day - they will:</h4>'

vent.trans.prob.head <- '<br><h4> For an infected person in the ICU who is on a ventilator, 
on a given day, what is the chance that - on the next day - they will:</h4>'

g.to.icu.wording <- 'Go to ICU?'
g.to.disc.wording <- 'Be discharged?'

icu.to.g.wording <- 'Move back to the general hospital (non-ICU)?'
icu.to.vent.wording <- 'Go on a ventilator?'

vent.to.icu.wording <- 'Move off the ventilator (staying in ICU)?'
vent.to.m.wording <- 'Die?'

##  ............................................................................
##  Natural Language Messages
##  ............................................................................

curr.inf.est.wording <- "<h3><b>Estimates for %s</b></h3>
<h4>We estimate there have been <u>%s total cases</u> of COVID-19 in the region, with
<u>%s people actively infected</u>.</h4>"

curr.inf.known.wording <- "<h3><b>Estimates for %s</b></h3>
<h4>There have been <u>%s total cases</u> of COVID-19 in the region, with
<u>%s people actively infected</u>.</h4>"

cases.curr.wording <- "<h4>On %s, there are <b>%s COVID-19 cases</b> in the region, 
with <b>%s actively infected</b>.</h4>"

cases.fut.wording <- "<h4>On %s, there will be <b>%s COVID-19 cases</b> in the region, 
with <b>%s actively infected</b>.</h4>"

hosp.curr.wording <- "<h4>On %s, there are <b>%s hospitalized from COVID-19</b> in the region, 
with <b>%s in ICU care</b> and <b>%s on ventilators</b>.</h4>"

hosp.fut.wording <- "<h4>On %s, there will be <b>%s hospitalized from COVID-19</b> in the region, 
with <b>%s in ICU care</b> and <b>%s on ventilators</b>.</h4>"

res.curr.wording <- "<h4>On %s, there are <b>%s hospital beds available</b> in the region, 
with <b>%s available ICU beds</b> and <b>%s available ventilators</b>.</h4>"

res.fut.wording <- "<h4>On %s, there will be <b>%s hospital beds available</b> in the region, 
with <b>%s available ICU beds</b> and <b>%s available ventilators</b>.</h4>"


##  ............................................................................
##  Initialized Dataframes 
##  ............................................................................

historical.df.blank <- data.frame('Date' = character(0),
                            'Hospitalizations' = numeric(0),
                            'Day' = numeric(0))

int.df.with.re <- data.frame('Day' = numeric(0),
                             'New Re' = numeric(0), 
                             'Days of Smoothing' =  numeric(0))

int.df.with.double <- data.frame('Day' = numeric(0),
                                'New Double Time' = numeric(0), 
                                'Days of Smoothing' =  numeric(0))
