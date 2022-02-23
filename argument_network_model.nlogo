globals
[
  nb-protest
  total-initial-protestor ;; the total number of people who are selected as initial protestors
    ;; the dose of grievance to pass
  infection_from_nw
  infection_to_non_pb

]
  breed[basic_agents basic_agent]
  breed[spreaders spreader]

extensions [nw]

links-own[
 trust
]

basic_agents-own
[
  threshold         ;; the person's threshold of grievance to protest

  protest?            ;; If true, the person protest
  protest-time        ;; current continous protest time

  memorized-grievance  ;; vector to record grievance accumated in the previous memory-T steps
  total-grievance     ;; sum of all memorized-grievance
  last-grievance      ;; grievance from the last time step
  prior_belief

]

spreaders-own
[
  threshold         ;; the person's threshold of grievance to protest
  protest?            ;; If true, the person protest

]


;;;
;;; SETUP PROCEDURES
;;;

to setup
  clear-all
  set infection_from_nw 0
  set infection_to_non_pb 0
  setup-network
  ;; set up the individual
  setup-basic_agents
  setup-spreaders

  ;ask n-of 300 patches [set pcolor brown]
  reset-ticks
end


to setup-basic_agents
  ask basic_agents
  [
    ;; set up the position of citizens
    setxy random-xcor random-ycor

    ;; initialize everyone to non-protest state
    set protest? false

    set shape "person"
    set color white

    ;; indicate the identity group and issue type of each citizen.
    ;; each citizen can only belong to one identity group and are sensitive to only one issue

    ;; set the threshold of the citizen to be a random number selected from zero to max-threshold
    set threshold ( random-float max-threshold )

    ;; select initial group of people who protest
    ;;set protest? ( select-as-origin? identity-group issue-type )

    ;; initialize the grievance history
    set memorized-grievance n-values memory-T [0];; a vector
    set total-grievance ( sum memorized-grievance )
    ifelse random-float 1 < existing_belief_rate [set prior_belief 1] [set prior_belief 0]
    clear-protest-time
  ]

  set nb-protest (count basic_agents with [protest?])



  ;; determine the total number of orgin
  determine-origin-number-select-origin
  set total-initial-protestor (count basic_agents with [ protest? ])
 ask basic_agents [ assign-color ]
end


to setup-spreaders
    create-spreaders initial-people * proportion_of_spreaders
 [
  setxy random-xcor random-ycor

    ;; initialize everyone to non-protest state
    set protest? true

    set shape "person"
    set color yellow


    ;; select initial group of people who protest
    ;;set protest? ( select-as-origin? identity-group issue-type )

    ;; initialize the grievance history

  ]
end

to setup-network

  nw:generate-watts-strogatz basic_agents links initial-people - initial-people * proportion_of_spreaders average-link 0.1 [ fd 10 ]
  ask links [set trust random-float 2 * trust_rate]

end





to determine-origin-number-select-origin
;; Determine if a citizen should be the initial protestor.
  let number-of-origin-condition 0

    set number-of-origin-condition initial-people
    set total-initial-protestor  ( round ( number-of-origin-condition * initial-fraction ) )
    ask n-of total-initial-protestor  basic_agents [ set protest? True]
    ask spreaders [set protest? True]

end


to clear-protest-time
  set protest-time 0
end


to assign-color  ;; basic_agent procedure
  ifelse protest?
    [ set color red ];; protest is red + (issue-type * 2 + identity-group * 2)
    [ set color green ];; not protest is green + (issue-type * 2 + identity-group * 2)
end


;;;
;;; GO PROCEDURES
;;;


to go
  ;; stop the simulation if all people are protesting or not protesting

  ;if ticks >= 100 [stop]

  ;; iterative process of infect people ( let others to protest )
  ask basic_agents
    [
      move
      clear-last-grievance
    ]
   ask spreaders
    [
      move
    ]

  ask basic_agents
    [
      dialogue
      interact
    ]
    ask spreaders
    [
      dialogue2
      interact2
    ]

  ask basic_agents
    [
      update-grievance
      decide-state-update-protest-time
      assign-color
    ]

  tick
end


;; People move about at random.
to move  ;; basic_agent procedure
  rt random-float 90
  lt random-float 90
  if [pcolor] of patch-ahead 1 = black [fd 1]
end

to clear-last-grievance
  set last-grievance 0
end

;; Interact, how each citizen pass (or not pass) dose to its neighbors
;; The dose will be added to the last-grievance of each citizen.
to dialogue  ;; basic_agent procedure

  let myprotest? protest?

  let this_agent who

  let neighbors-not-protest link-neighbors with [ not protest? ]

  if neighbors-not-protest != nobody
  [
    if myprotest?
    [
      ask neighbors-not-protest
      [
        if random-float 1 < infection_rate * (prior_belief + [trust] of (link-with basic_agent this_agent))
        [
        ;; a dose is passed.
           set last-grievance (last-grievance + dose-unit)
           set infection_from_nw (infection_from_nw + 1)
          if prior_belief = 0 [set infection_to_non_pb (infection_to_non_pb + 1)]
        ]
      ]
      ]
    ]
end

to interact  ;; basic_agent procedure

  let myprotest? protest?
  let nearby-inactive (basic_agents-on neighbors) ;;Reports an agentset containing all the turtles that are on the given patch or patches, or standing on the same patch as the given turtle or turtles.

  if nearby-inactive != nobody
  [
    if myprotest?
    [
      ask nearby-inactive
      [
        if random-float 1 < infection_rate * prior_belief
        [
        ;; a dose is passed.
           set last-grievance (last-grievance + dose-unit)
          if prior_belief = 0 [set infection_to_non_pb (infection_to_non_pb + 1)]
        ]
      ]
      ]
    ]
end


to dialogue2 ;; for spreaders


  let neighbors-not-protest link-neighbors with [ not protest? ]

  let this_agent who

  if  neighbors-not-protest != nobody
  [
  ask  neighbors-not-protest
    [
      if random-float 1 < infection_rate * (prior_belief + [trust] of (link-with spreader this_agent)) [

      set last-grievance (last-grievance + dose-unit)
        if prior_belief = 0 [set infection_to_non_pb (infection_to_non_pb + 1)]
      ;set color blue  ;burda anladık ki, spreaderlar basic agentların last grievanc'ını artırıyo, bu da çalışıyo yani
    ]
    ]
  ]
  ;set color blue - spreader'lar buraya giriyor
end


to interact2 ;; for spreaders

  let nearby-inactive (basic_agents-on neighbors)

  if nearby-inactive != nobody
  [
  ask nearby-inactive
    [
       if random-float 1 < infection_rate * prior_belief
        [
        ;; a dose is passed.
           set last-grievance (last-grievance + dose-unit)
          if prior_belief = 0 [set infection_to_non_pb (infection_to_non_pb + 1)]
        ]
      ;set color blue  ;burda anladık ki, spreaderlar basic agentların last grievanc'ını artırıyo, bu da çalışıyo yani
    ]
  ]
  ;set color blue - spreader'lar buraya giriyor
end



;; update the grievance vector--remove the earliest grievance, push the last grievance to the end of the grievance vector
to update-grievance
  ;; put the last-grievance to the end of the list, remove the head of the list
  set memorized-grievance (lput last-grievance memorized-grievance)
  set memorized-grievance (remove-item 0 memorized-grievance)

  ;; calculate total-grievance
  set total-grievance sum memorized-grievance
end

;; Determine the protest status according to current grievance vector.
;; Update the protest-time for all protesting people.
to decide-state-update-protest-time
  ifelse total-grievance > threshold
  [
    ifelse not protest?
    [ ;; for non-protestor: set the non-protest to protest, initiate protest-time as 1
      set protest? True
      set protest-time 1
    ]
    [;; for protestor: increase the protest-time
      set protest-time (protest-time + 1)
    ]
  ]
  [;; For the protestor whose grievance falls below the threshold, the protester may recover to non-protestor or may not (due to inertia).
    if protest?
    [ recover ]
  ]

  ;; If the protest-time reach maximum protest-time, force the citizen not to protest due to limited energy assumption.
  if protest-time > max-protest-time
  [ set protest? False
    clear-protest-time
  ]
end

;; If a citizen protests at time t-1 and his/her grievance falls below threshold at time t,
;; the citizen returns back to non-protest.
to recover
  set protest? false
  clear-protest-time
end
@#$#@#$#@
GRAPHICS-WINDOW
795
10
1396
612
-1
-1
23.72
1
10
1
1
1
0
1
1
1
-12
12
-12
12
1
1
1
hours
30.0

BUTTON
538
362
630
436
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
177
42
321
75
initial-people
initial-people
50
800
300.0
5
1
NIL
HORIZONTAL

PLOT
520
34
760
231
Populations
hours
# of people
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Protest" 1.0 0 -2674135 true "" "plot count basic_agents with [ protest? ]"
"Inactive" 1.0 0 -10899396 true "" "plot count basic_agents with [ not protest? ]"
"Spreader" 1.0 0 -14070903 true "" "plot count spreaders"

TEXTBOX
29
168
245
186
Parameters for citizens:
15
0.0
1

SLIDER
31
203
175
236
max-threshold
max-threshold
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
183
203
329
236
max-protest-time
max-protest-time
0
20
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
27
10
241
42
Parameters on global scale:
15
0.0
1

SLIDER
335
203
478
236
memory-T
memory-T
0
10
1.0
1
1
NIL
HORIZONTAL

BUTTON
648
364
736
436
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
31
43
157
76
initial-fraction
initial-fraction
0
0.1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
338
41
488
74
infection_rate
infection_rate
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
28
288
307
321
proportion_of_spreaders
proportion_of_spreaders
0
1
0.01
0.01
1
NIL
HORIZONTAL

TEXTBOX
26
257
294
295
Parameters for spreaders:
15
0.0
1

MONITOR
519
244
744
289
Number of Red + Yellow
count basic_agents with [protest?] + proportion_of_spreaders * initial-people
17
1
11

CHOOSER
29
343
167
388
dose-unit
dose-unit
1 2 3 4
0

SLIDER
30
403
202
436
existing_belief_rate
existing_belief_rate
0
1
0.5
0.1
1
NIL
HORIZONTAL

CHOOSER
32
98
170
143
average-link
average-link
1 2 3 4 5
0

SLIDER
188
105
360
138
trust_rate
trust_rate
0
1
1.0
0.1
1
NIL
HORIZONTAL

MONITOR
35
457
221
502
avg infections from network
infection_from_nw / (ticks + 1)
17
1
11

MONITOR
36
565
251
610
NIL
count links / count basic_agents
17
1
11

MONITOR
36
511
272
556
avg infections of non prior believers
infection_to_non_pb / (ticks + 1)
17
1
11

@#$#@#$#@
## WHAT IS IT?

This model simulates the spread of an infectious disease in a closed population. It is an introductory model in the curricular unit called epiDEM (Epidemiology: Understanding Disease Dynamics and Emergence through Modeling). This particular model is formulated based on a mathematical model that describes the systemic dynamics of a phenomenon that emerges when one infected person is introduced in a wholly susceptible population. This basic model, in mathematical epidemiology, is known as the Kermack-McKendrick model.

The Kermack-McKendrick model assumes a closed population, meaning there are no births, deaths, or travel into or out of the population. It also assumes that there is homogeneous mixing, in that each person in the world has the same chance of interacting with any other person within the world. In terms of the virus, the model assumes that there are no latent or dormant periods, nor a chance of viral mutation.

Because this model is so simplistic in nature, it facilitates mathematical analyses and also the calculation of the threshold at which an epidemic is expected to occur. We call this the reproduction number, and denote it as R_0. Simply, R_0 stands for the number of secondary infections that arise as a result of introducing one infected person in a wholly susceptible population, over the course of the infected person's contagious period (i.e. while the person is infective, which, in this model, is from the beginning of infection until recovery).

This model incorporates all of the above assumptions, but each individual has a 5% chance of being initialized as infected. This model shows the disease spread as a phenomenon with an element of stochasticity. Small perturbations in the parameters included here can in fact lead to different final outcomes.

Overall, this model helps users
1) engage in a new way of viewing/modeling epidemics that is more personable and relatable
2) understand how the reproduction number, R_0, represents the threshold for an epidemic
3) think about different ways to calculate R_0, and the strengths and weaknesses in each approach
4) understand the relationship between derivatives and integrals, represented simply as rates and cumulative number of cases, and
5) provide opportunities to extend or change the model to include some properties of a disease that interest users the most.

## HOW IT WORKS

Individuals wander around the world in random motion. Upon coming into contact with an infected person, by being in any of the eight surrounding neighbors of the infected person or in the same location, an uninfected individual has a chance of contracting the illness. The user sets the number of people in the world, as well as the probability of contracting the disease.

An infected person has a probability of recovering after reaching their recovery time period, which is also set by the user. The recovery time of each individual is determined by pulling from an approximately normal distribution with a mean of the average recovery time set by the user.

The colors of the individuals indicate the state of their health. Three colors are used: white individuals are uninfected, red individuals are infected, green individuals are recovered. Once recovered, the individual is permanently immune to the virus.

The graph INFECTION AND RECOVERY RATES shows the rate of change of the cumulative infected and recovered in the population. It tracks the average number of secondary infections and recoveries per tick. The reproduction number is calculated under different assumptions than those of the Kermack McKendrick model, as we allow for more than one infected individual in the population, and introduce aforementioned variables.

At the end of the simulation, the R_0 reflects the estimate of the reproduction number, the final size relation that indicates whether there will be (or there was, in the model sense) an epidemic. This again closely follows the mathematical derivation that R_0 = beta*S(0)/ gamma = N*ln(S(0) / S(t)) / (N - S(t)), where N is the total population, S(0) is the initial number of susceptibles, and S(t) is the total number of susceptibles at time t. In this model, the R_0 estimate is the number of secondary infections that arise for an average infected individual over the course of the person's infected period.

## HOW TO USE IT

The SETUP button creates individuals according to the parameter values chosen by the user. Each individual has a 5% chance of being initialized as infected. Once the model has been setup, push the GO button to run the model. GO starts the model and runs it continuously until GO is pushed again.

Note that in this model each time-step can be considered to be in hours, although any suitable time unit will do.

What follows is a summary of the sliders in the model.

INITIAL-PEOPLE (initialized to vary between 50 - 400): The total number of individuals in the simulation, determined by the user.
INFECTION-CHANCE (10 - 100): Probability of disease transmission from one individual to another.
RECOVERY-CHANCE (10 - 100): Probability of an infected individual to recover once the infection has lasted longer than the person's recovery time.
AVERAGE-RECOVERY-TIME (50 - 300): The time it takes for an individual to recover on average. The actual individual's recovery time is pulled from a normal distribution centered around the AVERAGE-RECOVERY-TIME at its mean, with a standard deviation of a quarter of the AVERAGE-RECOVERY-TIME. Each time-step can be considered to be in hours, although any suitable time unit will do.

A number of graphs are also plotted in this model.

CUMULATIVE INFECTED AND RECOVERED: This plots the total percentage of infected and recovered individuals over the course of the disease spread.
POPULATIONS: This plots the total number of people with or without the flu over time.
INFECTION AND RECOVERY RATES: This plots the estimated rates at which the disease is spreading. BetaN is the rate at which the cumulative infected changes, and Gamma rate at which the cumulative recovered changes.
R_0: This is an estimate of the reproduction number, only comparable to the Kermack McKendrick's definition if the initial number of infected were 1.

## THINGS TO NOTICE

As with many epidemiological models, the number of people becoming infected over time, in the event of an epidemic, traces out an "S-curve." It is called an S-curve because it is shaped like a sideways S. By changing the values of the parameters using the slider, try to see what kinds of changes make the S curve stretch or shrink.

Whenever there's a spread of the disease that reaches most of the population, we say that there was an epidemic. As mentioned before, the reproduction number indicates the number of secondary infections that arise as a result of introducing one infected person in a totally susceptible population, over the course of the infected person's contagious period (i.e. while the person is infected). If it is greater than 1, an epidemic occurs. If it is less than 1, then it is likely that the disease spread will stop short, and we call this an endemic.

## THINGS TO TRY

Try running the model by varying one slider at a time. For example:
How does increasing the number of initial people affect the disease spread?
How does increasing the recovery chance the shape of the graphs? What about changes to average recovery time? Or the infection rate?

What happens to the shape of the graphs as you increase the recovery chance and decrease the recovery time? Vice versa?

Notice the graph Cumulative Infected and Recovered, and Infection and Recovery Rates. What are the relationships between the two? Why is the latter graph jagged?

## EXTENDING THE MODEL

Try to change the behavior of the people once they are infected. For example, once infected, the individual might move slower, have fewer contacts, isolate him or herself etc. Try to think about how you would introduce such a variable.

In this model, we also assume that the population is closed. Can you think of ways to include demographic variables such as births, deaths, and travel to mirror more of the complexities that surround the nature of epidemic research?

## NETLOGO FEATURES

Notice that each agent pulls from a truncated normal distribution, centered around the AVERAGE-RECOVERY-TIME set by the user. This is to account for the variation in genetic differences and the immune system functions of individuals.

Notice that R_0 calculated in this model is a numerical estimate to the analytic R_0. In the special case of one infective introduced to a wholly susceptible population (i.e., the Kermack-McKendrick assumptions), the numerical estimations of R0 are very close to the analytic values.

## RELATED MODELS

AIDS, Virus and Virus on a Network are related models.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Yang, C. and Wilensky, U. (2011).  NetLogo epiDEM Basic model.  http://ccl.northwestern.edu/netlogo/models/epiDEMBasic.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2011 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2011 Cite: Yang, C. -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person lefty
false
0
Circle -7500403 true true 170 5 80
Polygon -7500403 true true 165 90 180 195 150 285 165 300 195 300 210 225 225 300 255 300 270 285 240 195 255 90
Rectangle -7500403 true true 187 79 232 94
Polygon -7500403 true true 255 90 300 150 285 180 225 105
Polygon -7500403 true true 165 90 120 150 135 180 195 105

person righty
false
0
Circle -7500403 true true 50 5 80
Polygon -7500403 true true 45 90 60 195 30 285 45 300 75 300 90 225 105 300 135 300 150 285 120 195 135 90
Rectangle -7500403 true true 67 79 112 94
Polygon -7500403 true true 135 90 180 150 165 180 105 105
Polygon -7500403 true true 45 90 0 150 15 180 75 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count basic_agents with [protest?]</metric>
    <enumeratedValueSet variable="max-protest-time">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-people">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-T">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-frequency">
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-threshold">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion_of_spreaders">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dose-unit">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior_belief_rate">
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-link">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust_rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="max-protest-time">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-people">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-T">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-frequency">
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-threshold">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion_of_spreaders">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dose-unit">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prior_belief_rate">
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-link">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust_rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
