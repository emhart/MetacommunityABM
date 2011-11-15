globals [m_p popsize current n_color age_reproduct o-patch t-patch uo-patch]
breed [n1s n1]
breed [n2s n2]
breed [ps p]
n1s-own [energy age cap_eff hand_time tastiness]
n2s-own [energy age cap_eff hand_time tastiness]
ps-own [energy age delay rep_min rep_max]
patches-own [penergy group node n1_size n2_size tp_size p_size occupied ln1_size ln2_size puphill]


to setup
clear-all
set-plot-y-range 0 (max list size_n1 size_n2)
set-default-shape n1s "ant"
set-default-shape n2s "ant"
set-default-shape ps "hawk"
create-n1s (size_n1)
create-n2s (size_n2)
create-ps (size_p)
create-resource
ask n1s [place_ants red n1_capt_eff n1_handle_time n1_conversion]
ask n2s [place_ants blue  n2_capt_eff n2_handle_time n2_conversion]
;ask patches [patch_count]
 ask ps [place_pred pink]
;set age_reproduct (list 5 50 100 75 50 5)
set current 1
end



to place_ants [my_color c_eff han_t taste]
  move-to one-of patches with [pcolor = green]
 face one-of neighbors
  set color my_color
   set energy 20
   set age 0
   set cap_eff c_eff
   set hand_time han_t
   set tastiness taste
   
   end
to place_pred [my_color]
move-to one-of patches with [pcolor = green]
face one-of neighbors
set color my_color

set energy 20
set delay 0
set age 0
end

   
   
 to go
ifelse((random 2) = 1)
 [ask n1s [feed n1_frate N1_disp]]
 [ask n2s [feed n2_frate N2_disp]]
  ask ps [hunt_p]

ifelse((random 2) = 1)
 [ask n1s [reproduce N1_reprod_thresh n1_capt_eff n1_handle_time n1_conversion]]
 [ask n2s [reproduce N2_reprod_thresh n2_capt_eff n2_handle_time n2_conversion]]
  ask ps [reproduce_p P_reprod_thresh]
  
ifelse((random 2) = 1)
 [ask n1s [alive]]
 [ask n2s [alive]]
ask ps [alive]

 ifelse((random 2) = 1)
 [extinct_patches]
 [grow_new_patches]
; file-open "my_testrun12.csv"
;ask patches with [node = 1] 
;  [ file-write group file-type "," file-write n1_size file-type "," file-write n2_size file-type "," file-write p_size file-type "," file-write ticks file-print " " ]
 

 grow_patches
 patch_count

tick


 end
 

 
to reproduce_p [threshold]

;let r_prob item (round (age / 100)) age_reproduct 
if (energy >= P_reprod_thresh ) [
  set energy (energy * .5)
 
  hatch 1 [set age 0
  set energy 20
  set delay 0
 
   face one-of neighbors ]]
  end

to reproduce [threshold c_eff han_t taste]
;let r_prob item (round (age / 100)) age_reproduct 
if (energy > threshold) [
  set energy (energy * .33)
  hatch 1 [set age 0
  set energy 5
  set cap_eff c_eff
   set hand_time han_t
   set tastiness taste
 
  face one-of neighbors  ]
  ]

end 
 
 to move [disp cost]
   
  let move_en (random disp)
  fd move_en
  set energy (energy - cost)
  face one-of neighbors 
  end

to create-resource
ask patches[
    if (sum([pcolor] of patches in-radius patch-space) = 0 )  [
        if ((random-float 100) < resource-grow-rate) [
         ask patches in-radius patch-size [
        set pcolor 55
        set penergy 30
        set group current
        set node 2
        ]
        
        set pcolor 55
        set penergy 30
        set group current 
        set node 1 
       
        ]

    ]
    
    set current (current + 1)
    ]
    
    
 
  end
  
 to hunt_p
 if (delay < ticks)[
  ifelse (any? n1s-here) or (any? n2s-here)[
  ask ps [feed_p]]
  [
     ifelse (sum([puphill] of neighbors) > 0)       
            [
             rt random 180
            lt random 180
            ifelse ([penergy] of patch-ahead 1 > 0) [fd 1]
            [fd -1
            rt random 180
            lt random 180]
      
           set energy energy - 1
            ]
            [move P_disp 2]
             
  ]
    ]
 end 
 ;These functions control how our organisms feed.
 
to feed_p 
ifelse (random 2 = 1)[
  let preyn2 one-of n2s-here
  if (preyn2 != nobody) and (random-float 1 < [cap_eff] of preyn2)[
         
          set energy (energy + [tastiness] of preyn2)
         set delay (ticks + [hand_time] of preyn2)
           ask preyn2 [die]
      ]]
[
  let preyn1 one-of n1s-here
  if (preyn1 != nobody) and (random-float 1 < [cap_eff] of preyn1)[
      
          set energy (energy + [tastiness] of preyn1)
          set delay (ticks + [hand_time] of preyn1)
          ask preyn1 [die]
      ]]
 
end                        
           
  
  
  
to feed [frate disp]
     let n2s_count count n2s-on neighbors
     
      if(penergy > 0)[ 
      ifelse (ln1_size + ln2_size < 5)  [ 
            if (breed = n2s)[set frate frate]
       if (breed = n1s) and (count n2s-here > 0) [set frate (frate - n2_n1_interfere)]
      
       set energy (energy + frate)
       set penergy (penergy - frate)
       set pcolor 45]
       [
         rt random 180
            lt random 180
           set energy energy - .5
            ifelse ([penergy] of patch-ahead 1 > 0) [fd 1]
            [fd -1
            rt random 180
            lt random 180]
  ]]    
      
        
      if (breed = n1s) and (n2s_count >= disp-thresh) and (disp-thresh-on = true) [move disp 1]
      
          if (sum([penergy] of neighbors) > 0) and (penergy < 0)[ 
          face one-of neighbors with [penergy > 0]
          fd 1 
          
          set energy energy - .5
          ]
      
      
      
        if (pcolor = 0) [move disp 1]
     
      
      if (breed = n2s) and (sum([penergy] of neighbors) < n2_energy-threshold )
     [move disp 1]
      
     if (breed = n1s) and (sum([penergy] of neighbors)  < n1_energy-threshold)
     [move disp 1]
      
      
       
       ;if (penergy <= 0)  and (sum([penergy] of neighbors) > mean (list n2_energy-threshold n1_energy-threshold)) [uphill penergy]
end     
  
to alive
if (energy <= 0) [die]
if (age > 500) [die]
set age (age + 1)
end
 
to grow_patches 
ask patches [ 
if (penergy >= 30) [set penergy 30 
set pcolor 55]
if (penergy < 30) and (group >= 1)  [set pcolor 45
ifelse (any? n1s) and (any? n2s) [ set penergy (penergy + (resource-regrowth / 2))] [set penergy (penergy + resource-regrowth)]]
]
end


to grow_new_patches
ask patches[
     if (random-normal 0 1 > 4)[
    if (sum([pcolor] of patches in-radius patch-space) = 0 )  [
        if ((random-float 100) < resource-grow-rate) [
         ask patches in-radius patch-size [
        set pcolor 55
        set penergy 30
        set group current
        set node 2
        
        ]
        set pcolor 55
        set penergy 30
        set group current 
        set node 1 
       
        ]

    ]
    
    set current (current + 1)
    ]
    ]
end

to extinct_patches
  ask patches with [node = 1] [
if (random-normal 0 1 > 4)[

       set pcolor 0
        set penergy 0
        set group 0 
        set node 0
        set occupied 0
        set n1_size 0
        set n2_size 0
        ask patches in-radius patch-size [
        set pcolor 0
        set penergy 0
       set group 0
        set node 0 
     ]

]
]

end


to do-plot-counts
set t-patch count patches with [node = 1]
set o-patch count patches with [occupied = 1]

set-current-plot "Counts"
set-plot-y-range 0 1
;set-current-plot-pen "Occupied"
;plot o-patch / t-patch
set-current-plot-pen "N2_only"
plot (count patches with [n2_size > 0 and n1_size = 0]) / t-patch
set-current-plot-pen "N1_Only"
plot (count patches with [n1_size > 0 and n2_size = 0]) / t-patch
set-current-plot-pen "N1_N2"
plot (count patches with [n1_size > 0 and n2_size > 0]) / t-patch

end






to do-plot-pop-size
 



set-current-plot "Populations"
set-current-plot-pen "N1"
plot count n1s
set-current-plot-pen "N2"
plot count n2s
set-current-plot-pen "P"
plot count ps

end

to plot-patch-pop
set-current-plot "Patch_Pop"
set-plot-y-range 0 30
set-current-plot-pen "N1"
if (count patches with [n1_size > 0] > 0)[
plot mean ([n1_size] of patches with [n1_size > 0])]
set-current-plot-pen "N2"
if (count patches with [n2_size > 0] > 0)[
plot mean ([n2_size] of patches with [n2_size > 0])]
set-current-plot-pen "P"
if (count patches with [n2_size > 0] > 0)[
plot mean ([n2_size] of patches with [n2_size > 0])]

end

to plot-state-space
set-current-plot "State_space"
plotxy (count n1s) (count ps)


end

to patch_count
ask patches with [node = 1]
        [
            ifelse (count turtles-here > 0) or (any? turtles in-radius patch-size)[set occupied 1] [set occupied 0]
            set n1_size sum (list count n1s-here count n1s in-radius patch-size) 
            set n2_size sum (list count n2s-here count n2s in-radius patch-size) 
            set p_size sum (list count ps-here count ps in-radius patch-size) 
           set tp_size n1_size + n2_size
]
ask patches with [node > 0][
    set ln1_size count n1s-here
    set ln2_size count n2s-here
    set puphill (1.5 * ln2_size) + ln1_size
   
]
end

to make-movie

  ;; prompt user for movie location
  user-message "First, save your new movie file (choose a name ending with .mov)"
  let path user-new-file
  if not is-string? path [ stop ]  ;; stop if user canceled

  ;; run the model
  setup
  movie-start path
  movie-grab-view
  while [  ticks < 1000 ]
    [ go
      movie-grab-view ]

  ;; export the movie
  movie-close
  user-message (word "Exported movie to " path)
end



@#$#@#$#@
GRAPHICS-WINDOW
232
25
1252
1066
50
50
10.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
0
0
1
ticks

CC-WINDOW
5
1080
1617
1175
Command Center
0

SLIDER
17
28
189
61
size_n1
size_n1
0
100
100
1
1
NIL
HORIZONTAL

SLIDER
15
72
187
105
size_n2
size_n2
0
100
100
1
1
NIL
HORIZONTAL

BUTTON
1463
10
1532
43
Set-up
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
1545
10
1608
43
Go!
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
15
265
187
298
resource-grow-rate
resource-grow-rate
0
10
10
.01
1
NIL
HORIZONTAL

SLIDER
15
499
187
532
resource-regrowth
resource-regrowth
0
10
3
.5
1
NIL
HORIZONTAL

PLOT
1077
427
1453
618
Populations
tick
popsize
0.0
500.0
0.0
10.0
true
true
PENS
"N1" 1.0 0 -2674135 true
"N2" 1.0 0 -13345367 true
"P" 1.0 0 -2064490 true

SLIDER
21
548
193
581
n1_frate
n1_frate
0
10
3.5
.5
1
NIL
HORIZONTAL

SLIDER
17
590
189
623
n2_frate
n2_frate
0
10
3.5
.5
1
NIL
HORIZONTAL

SLIDER
23
642
195
675
n2_n1_interfere
n2_n1_interfere
0
10
2
.5
1
NIL
HORIZONTAL

SLIDER
25
322
197
355
patch-space
patch-space
0
20
10
1
1
NIL
HORIZONTAL

SLIDER
30
370
202
403
patch-size
patch-size
0
20
4
1
1
NIL
HORIZONTAL

SLIDER
14
687
186
720
disp-thresh
disp-thresh
1
10
4
1
1
NIL
HORIZONTAL

PLOT
1074
212
1453
420
Counts
tick
Patch occupancy
0.0
100.0
0.0
1.0
true
true
PENS
"Occupied" 1.0 0 -14835848 true
"N1_only" 1.0 0 -2674135 true
"N2_only" 1.0 0 -13345367 true
"N1_N2" 1.0 0 -8630108 true

PLOT
1072
10
1454
202
Patch_Pop
tick
Mean Patch Pop Size
0.0
500.0
0.0
10.0
true
true
PENS
"N1" 1.0 0 -2674135 true
"N2" 1.0 0 -13345367 true

PLOT
1076
633
1453
824
State_space
N1
N2
0.0
10.0
0.0
10.0
true
true
PENS
"default" 1.0 0 -16777216 true

BUTTON
1458
578
1535
611
Plot Me!
do-plot-pop-size
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
1462
168
1539
201
Plot Me!
plot-patch-pop
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
1459
787
1536
820
Plot Me!
plot-state-space
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
1465
392
1542
425
Plot Me!
do-plot-counts
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
21
739
157
772
disp-thresh-on
disp-thresh-on
0
1
-1000

SLIDER
900
92
1072
125
N1_reprod_thresh
N1_reprod_thresh
0
50
20
1
1
NIL
HORIZONTAL

SLIDER
899
132
1071
165
N2_reprod_thresh
N2_reprod_thresh
0
50
20
1
1
NIL
HORIZONTAL

SLIDER
14
120
186
153
size_p
size_p
0
50
50
1
1
NIL
HORIZONTAL

CHOOSER
883
252
976
297
n1_handle_time
n1_handle_time
0 1 2 3 4 5 6 7 8 9 10
10

CHOOSER
981
252
1073
297
n2_handle_time
n2_handle_time
1 2 3 4 5 6 7 8 9 10
4

SLIDER
893
314
1065
347
n1_capt_eff
n1_capt_eff
0
1
0.1
.01
1
NIL
HORIZONTAL

SLIDER
893
356
1065
389
n2_capt_eff
n2_capt_eff
0
1
0.5
.01
1
NIL
HORIZONTAL

SLIDER
897
410
1069
443
n1_conversion
n1_conversion
0
20
2
1
1
NIL
HORIZONTAL

SLIDER
901
464
1073
497
n2_conversion
n2_conversion
0
20
8
1
1
NIL
HORIZONTAL

SLIDER
897
57
1069
90
P_reprod_thresh
P_reprod_thresh
0
50
40
1
1
NIL
HORIZONTAL

CHOOSER
901
522
993
567
P_disp
P_disp
1 2 3 4 5 6 7 8 9 10 11 12 13 14
4

MONITOR
1469
528
1536
573
Predators
count ps
17
1
11

MONITOR
1454
483
1546
528
Species 2 Blue
count n2s
17
1
11

MONITOR
1455
432
1545
477
Species 1 Red
count n1s
17
1
11

SLIDER
32
414
204
447
N1_disp
N1_disp
0
30
20
1
1
NIL
HORIZONTAL

SLIDER
41
460
213
493
N2_disp
N2_disp
0
30
9
1
1
NIL
HORIZONTAL

SLIDER
897
179
1069
212
n2_energy-threshold
n2_energy-threshold
0
50
14
1
1
NIL
HORIZONTAL

SLIDER
893
216
1065
249
n1_energy-threshold
n1_energy-threshold
0
100
8
1
1
NIL
HORIZONTAL

BUTTON
423
742
520
775
NIL
make-movie\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

@#$#@#$#@
WHAT IS IT?
-----------
This section could give a general understanding of what the model is trying to show or explain.


HOW IT WORKS
------------
This section could explain what rules the agents use to create the overall behavior of the model.


HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.


THINGS TO NOTICE
----------------
This section could give some ideas of things for the user to notice while running the model.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


CREDITS AND REFERENCES
----------------------
This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

ant
true
0
Polygon -7500403 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7500403 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7500403 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7500403 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7500403 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7500403 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7500403 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7500403 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7500403 true true 249 107 211 147 168 147 168 150 213 150

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

hawk
true
0
Polygon -7500403 true true 151 170 136 170 123 229 143 244 156 244 179 229 166 170
Polygon -16777216 true false 152 154 137 154 125 213 140 229 159 229 179 214 167 154
Polygon -7500403 true true 151 140 136 140 126 202 139 214 159 214 176 200 166 140
Polygon -16777216 true false 151 125 134 124 128 188 140 198 161 197 174 188 166 125
Polygon -7500403 true true 152 86 227 72 286 97 272 101 294 117 276 118 287 131 270 131 278 141 264 138 267 145 228 150 153 147
Polygon -7500403 true true 160 74 159 61 149 54 130 53 139 62 133 81 127 113 129 149 134 177 150 206 168 179 172 147 169 111
Circle -16777216 true false 144 55 7
Polygon -16777216 true false 129 53 135 58 139 54
Polygon -7500403 true true 148 86 73 72 14 97 28 101 6 117 24 118 13 131 30 131 22 141 36 138 33 145 72 150 147 147

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
NetLogo 4.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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