##  ............................................................................
## Compilation of all of the strings that are outputted in the app.
##  ............................................................................

##  ............................................................................
##  General
##  ............................................................................

app.title <- "LEMMA - Local Epidemic Modeling for Management & Action"

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

what.is.lemma <- '<font size="4">LEMMA (Local Epidemic Modeling for Management & Action) is designed to provide local projections of 
the SARS-CoV-2 (COVID-19) epidemic to aid in the planning and management of hospital resources under various scenarios. 
Daily projections are made for hospitalizations, ICU use, ventilator use, as well as total cases, active cases, and 
resolved cases. These projections are based on a modified Susceptible-Exposed-Infectious-Recovered (SEIR) model and are parameterized 
using local epidemic data on the number of hospitalizations. <b><a href="https://lemma.gitbook.io/lemma/-M48KVCMAiSk20b1_LWH/">View the documentation here.</a></b>
<br><br>
The code for this tool is on <a href="https://github.com/lemdt/CovidShinyModel">Github</a>.
<br><br>
LEMMA is a collaborative effort between experts in Medicine, Public Health, and Data Science.<br><br>
<b>Key contributors include:</b>
<ul>
  <li>Maya L. Petersen, MD PhD, Associate Professor of Epidemiology & Biostatistics, UC Berkeley</li>
  <li>Joshua Schwab, MS, Research Programmer, UC Berkeley</li>
  <li>James Peng, MS, Data Scientist, UC San Francisco</li>
  <li>Laura B. Balzer, PhD, Assistant Professor of Biostatistics, UMass Amherst</li>
  <li>Ben Olding, XX, YY, ZZZ</li>
  <li>Randy True, XX, YY, ZZZ</li>
  <li>Karthik Ram, XX, YY, ZZZ</li>
  <li>Vivek Jain, MD, Associate Professor of Medicine, UC San Francisco & San Francisco General Hospital</li>
  <li>Elvin Geng, MD, Professor of Medicine, Washington University in St. Louis</li>
  <li>Diane V. Havlir, MD, Professor of Medicine, UC San Francisco & San Francisco General Hospital</li>
</ul></font>'



##  ............................................................................
##  Warning/Notifications
##  ............................................................................

re.warning.date.repeat <- "This date has already been added."

re.warning.blank.num <- "Please enter the hospitalization number."

re.warning.more.data <-
  "You need at least two timepoints of data to make a prediction."

best.re.msg <- "<br><br><h4>The best estimate for Re is <b>%s</b>."

double.int.warning <-
  "You have already added an intervention on this date."

##  ............................................................................
##  Wording for Model Inputs
##  ............................................................................

incubation.input.wording <-
  "Number of Days from Infection to Becoming Infectious (Latent Period)"

infectious.input.wording <-
  "Duration of infectiousness (days)"

per.hosp.wording <- "Percent of Infected that are Hospitalized"

per.icu.wording <-
  "Percent of Hospitalized COVID-19 Patients That are Currently in the ICU"

per.vent.wording <-
  "Percent of COVID-19 Patients in the ICU who are Currently Ventilated"

inf.to.hosp.wording <-
  "Time from onset of infectiousness to hospitalization (days)"

hosp.to.icu.wording <-
  "Number of days from hospitalization to ICU Admission"

icu.to.vent.wording <-
  "Number of days from ICU admission to ventilation"

hosp.los.wording <-
  "Hospital Length of Stay for Non-ICU Patients (days)"

hosp.los.wording.model0 <- "Average Hospital Length of Stay (Days)"

icu.los.wording <-
  "ICU Length of Stay for Non-Ventilated Patients (days)"

vent.los.wording <- "Average time on a ventilator (days)"

avail.vent.wording <- "Ventilator Availability"

##  ............................................................................
##  Wording for Inputs - Model 2 Specific
##  ............................................................................

g.trans.prob.head <-
  '<br><h4>For General Inpatients (Non-ICU), what is the probability that tomorrow:</h4>'

icu.trans.prob.head <-
  '<br><h4>For ICU Patients not on a Ventilator, what is the probability that tomorrow:</h4>'

vent.trans.prob.head <-
  '<br><h4> For Ventilated Patients, what is the probability that tomorrow:</h4>'

g.to.icu.wording <- 'General Inpatient goes to the ICU?'
g.to.disc.wording <- 'General Inpatient is Discharged?'

icu.to.g.wording <-
  'ICU Patient Returns To Being a General Inpatient?'
icu.to.vent.wording <- 'ICU Patient Goes on a Ventilator?'

vent.to.icu.wording <- 'Move off the Ventilator?'
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

cases.curr.wording <-
  "<h4>On %s, there are <b>%s COVID-19 cases</b> in the region,
with <b>%s actively infected</b>.</h4>"

cases.fut.wording <-
  "<h4>On %s, there will be <b>%s COVID-19 cases</b> in the region,
with <b>%s actively infected</b>.</h4>"

hosp.curr.wording <-
  "<h4>On %s, there are <b>%s hospitalized from COVID-19</b> in the region,
with <b>%s in ICU care</b> and <b>%s on ventilators</b>.</h4>"

hosp.fut.wording <-
  "<h4>On %s, there will be <b>%s hospitalized from COVID-19</b> in the region,
with <b>%s in ICU care</b> and <b>%s on ventilators</b>.</h4>"

res.curr.wording <-
  "<h4>On %s, there are <b>%s hospital beds available</b> in the region,
with <b>%s available ICU beds</b> and <b>%s available ventilators</b>.</h4>"

res.fut.wording <-
  "<h4>On %s, there will be <b>%s hospital beds available</b> in the region,
with <b>%s available ICU beds</b> and <b>%s available ventilators</b>.</h4>"
