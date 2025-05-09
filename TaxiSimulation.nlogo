extensions [table]

globals [ride-requests wait-times congested-streets nearest-results smart-results]

turtles-own [has-passenger? dispatched? destination speed pickup-time]

patches-own [is-street? traffic-level]

;; Setup function
to setup
  clear-all
  set ride-requests []
  set wait-times []
  set congested-streets []

  ;; Identify street patches
  ask patches [
    ifelse (pxcor mod 5 = 0) or (pycor mod 5 = 0) [
      set is-street? true
    ] [
      set is-street? false
    ]
  ]

  ;; Set default traffic level and color for streets
  ask patches with [is-street?] [
    set traffic-level 1
    set pcolor gray
  ]

  ;; Create traffic jams (clustered congestion), avoiding corners
  repeat 5 [
    let traffic-center one-of patches with [
      is-street? and
      pxcor > ((0 - max-pxcor) + 5) and pxcor < (max-pxcor - 5) and
      pycor > ((0 - max-pycor) + 5) and pycor < (max-pycor - 5)
    ]
    if traffic-center != nobody [
      let x [pxcor] of traffic-center
      let y [pycor] of traffic-center

      ask patches with [
        is-street? and
        ((pxcor = x and abs(pycor - y) < 5) or (pycor = y and abs(pxcor - x) < 5)) and
        not is-corner? self
      ] [
        set traffic-level one-of [2 3 4]
      ]
    ]
  ]

  ;; Apply traffic colors
  ask patches with [is-street?] [
    if traffic-level = 1 [ set pcolor white ]
    if traffic-level = 2 [ set pcolor yellow ]
    if traffic-level = 3 [ set pcolor brown ]
    if traffic-level = 4 [ set pcolor gray ]
  ]

  ;; Save all congested streets (traffic level > 1)
  set congested-streets patches with [is-street? and traffic-level > 1]

  ;; Create taxis
  create-turtles num-taxis [
    move-to one-of patches with [is-street?]
    set has-passenger? false
    set dispatched? false
    set destination []
    set speed 1
    set shape "car"
    set color black
    set heading one-of [0 90 180 270]
  ]

  reset-ticks
end

to go
  if ticks >= 2000 [ stop ]
  generate-rides
  dispatch-taxis dispatch-strategy
  move-taxis
  tick
end

to generate-rides
  if (random-float 1 < 0.3) and (length ride-requests < 20) [
    let pickup-spot one-of patches with [is-street?]
    let dropoff-spot one-of patches with [is-street? and self != pickup-spot]

    if (pickup-spot != nobody and dropoff-spot != nobody) [
      let new-request (list (list [pxcor] of pickup-spot [pycor] of pickup-spot)
                            (list [pxcor] of dropoff-spot [pycor] of dropoff-spot)
                            ticks)
      set ride-requests lput new-request ride-requests
      ask pickup-spot [set pcolor green]
      ask dropoff-spot [set pcolor red]
    ]
  ]
end

to-report best-of-5-path-cost [start-x start-y end-x end-y]
  let directions [
    ["hv"]  ;; horizontal first, then vertical
    ["vh"]  ;; vertical first, then horizontal
    ["zigzag1"]
    ["zigzag2"]
    ["straight-diagonal"]
  ]
  let best-cost 1e10

  foreach directions [ pattern ->
    let cost estimate-path-cost start-x start-y end-x end-y pattern
    if cost < best-cost [
      set best-cost cost
    ]
  ]

  report best-cost
end

to-report estimate-path-cost [x1 y1 x2 y2 style]
  let cost 0
  let x x1
  let y y1
  let step-count 0
  let max-steps 200

  show (word "🧠 Estimating path [" style "] from (" x1 "," y1 ") to (" x2 "," y2 ")")

  while [x != x2 or y != y2 and step-count < max-steps] [
    set step-count step-count + 1
    show (word "🚶 Step " step-count ": (" x "," y ")")

    ;; Movement logic (guarantees progress)
    if style = "hv" [
      if x != x2 [ set x x + sign(x2 - x) ]
      if x = x2 and y != y2 [ set y y + sign(y2 - y) ]
    ]

    if style = "vh" [
      if y != y2 [ set y y + sign(y2 - y) ]
      if y = y2 and x != x2 [ set x x + sign(x2 - x) ]
    ]

    if style = "zigzag1" [
      if step-count mod 2 = 0 and x != x2 [
        set x x + sign(x2 - x)
      ]
      if step-count mod 2 != 0 and y != y2 [
        set y y + sign(y2 - y)
      ]
      ;; fallback
      if x = x1 and y = y1 [
        if x != x2 [ set x x + sign(x2 - x) ]
        if y != y2 [ set y y + sign(y2 - y) ]
      ]
    ]

    if style = "zigzag2" [
      if step-count mod 2 = 0 and y != y2 [
        set y y + sign(y2 - y)
      ]
      if step-count mod 2 != 0 and x != x2 [
        set x x + sign(x2 - x)
      ]
      ;; fallback
      if x = x1 and y = y1 [
        if x != x2 [ set x x + sign(x2 - x) ]
        if y != y2 [ set y y + sign(y2 - y) ]
      ]
    ]

    if style = "bounce" [
      if step-count mod 4 < 2 and x != x2 [
        set x x + sign(x2 - x)
      ]
      if step-count mod 4 >= 2 and y != y2 [
        set y y + sign(y2 - y)
      ]
    ]

    ;; Traffic cost
    if x >= min-pxcor and x <= max-pxcor and y >= min-pycor and y <= max-pycor [
      let p patch x y
      if p != nobody [
        if [is-street?] of p [
          set cost cost + [traffic-level] of p
        ]
        if not [is-street?] of p [
          set cost cost + 5
        ]
      ]
    ]

    if x < min-pxcor or x > max-pxcor or y < min-pycor or y > max-pycor [
      show (word "⚠️ Out of bounds at (" x "," y "), adding penalty.")
      set cost cost + 10
    ]
  ]

  if step-count >= max-steps [
    show (word "💀 Max steps reached at (" x "," y ") — giving up.")
    report 1e10
  ]

  show (word "✅ Final cost: " cost)
  report cost
end





to dispatch-taxis [strategy]
  let unassigned-requests []
  let assigned-taxis []

  foreach ride-requests [ request ->
    let already-assigned? any? turtles with [dispatched? and destination = request]
    if not already-assigned? [
      set unassigned-requests lput request unassigned-requests
    ]
  ]

  if strategy = "random" [
    let available-taxis turtles with [
      not has-passenger? and not dispatched?
    ]

    let shuffled-requests shuffle unassigned-requests

    foreach (list available-taxis) [ taxi ->
      if length shuffled-requests > 0 [
        let request first shuffled-requests  ;; pick one in order
        set shuffled-requests but-first shuffled-requests  ;; remove it so no one else gets it

        ask taxi [
          set destination request
          set dispatched? true
          set has-passenger? false
          set color green
        ]
        set assigned-taxis lput taxi assigned-taxis
        set unassigned-requests remove request unassigned-requests
      ]
    ]
  ]




  if strategy = "nearest" [
    foreach unassigned-requests [ request ->
      let pickup-location first request
      let pickup-x item 0 pickup-location
      let pickup-y item 1 pickup-location

      let chosen-taxi min-one-of turtles with [
        not has-passenger? and not dispatched? and not member? self assigned-taxis
      ] [
        distancexy pickup-x pickup-y
      ]

      if chosen-taxi != nobody [
        ask chosen-taxi [
          set destination request
          set dispatched? true
          set has-passenger? false
          set color green
        ]
        set assigned-taxis lput chosen-taxi assigned-taxis
      ]
    ]
  ]
  if strategy = "smart" [
    let available-taxis turtles with [
      not has-passenger? and not dispatched? and not member? self assigned-taxis
    ]

    ask available-taxis [
      let sorted-requests sort-by
      [[a b] ->
        distancexy (item 0 (first a)) (item 1 (first a))
        < distancexy (item 0 (first b)) (item 1 (first b))
      ] unassigned-requests


      let top-3 sublist sorted-requests 0 (min (list 3 length sorted-requests))


      let best-request nobody
      let best-traffic 1e10

      foreach top-3 [ request ->
        let pickup-x item 0 (first request)
        let pickup-y item 1 (first request)
        let pickup-traffic [traffic-level] of patch pickup-x pickup-y

        if pickup-traffic < best-traffic [
          set best-traffic pickup-traffic
          set best-request request
        ]
      ]

      if best-request != nobody [
        set destination best-request
        set dispatched? true
        set has-passenger? false
        set color green
        set assigned-taxis lput self assigned-taxis
        set unassigned-requests remove best-request unassigned-requests
      ]
    ]
  ]
  if strategy = "super-smart" [
    let k 3  ;; number of ride requests to evaluate per taxi

    let available-taxis turtles with [
      not has-passenger? and not dispatched? and not member? self assigned-taxis
    ]

    ask available-taxis [
      let taxi-x xcor
      let taxi-y ycor
      show (word "🚕 Taxi " who " starting super-smart dispatch from (" taxi-x "," taxi-y ")")

      let sorted-requests sort-by
      [[a b] ->
        distancexy (item 0 (first a)) (item 1 (first a)) <
        distancexy (item 0 (first b)) (item 1 (first b))
      ] unassigned-requests

      let top-k sublist sorted-requests 0 (min (list k length sorted-requests))

      let best-request nobody
      let best-cost 1e10

      foreach top-k [ request ->
        let pickup-x item 0 (first request)
        let pickup-y item 1 (first request)
        show (word "🔍 Taxi " who " checking request: " request)

        let cost best-of-5-path-cost taxi-x taxi-y pickup-x pickup-y
        show (word "📏 Estimated pickup path cost: " cost)

        if cost < best-cost [
          set best-cost cost
          set best-request request
          show (word "✅ New best request for Taxi " who ": " request " (Cost: " cost ")")
        ]
      ]
     ifelse best-request != nobody [
        set destination best-request
        set dispatched? true
        set has-passenger? false
        set color green
        set assigned-taxis lput self assigned-taxis
        set unassigned-requests remove best-request unassigned-requests
        show (word "🎯 Taxi " who " assigned to: " best-request)
      ] [
        show (word "⚠️ Taxi " who " found NO valid request.")
      ]

    ]
  ]







end

to-report sign [n]
  if n > 0 [ report 1 ]
  if n < 0 [ report -1 ]
  report 0
end


to-report straight-path-traffic-cost [start-x start-y end-x end-y]
  let cost 0
  let delta-x sign (end-x - start-x)
  let delta-y sign (end-y - start-y)
  let x start-x
  let y start-y

  while [x != end-x or y != end-y] [
    if x != end-x [ set x x + delta-x ]
    if y != end-y [ set y y + delta-y ]
    let p patch x y
    ifelse [is-street?] of p
    [
      set cost cost + [traffic-level] of p
    ]
    [
      set cost cost + 5  ;; optional penalty for off-street
    ]

  ]
  report cost
end


to recolor-street [p]
  ask p [
    if traffic-level = 1 [ set pcolor white ]
    if traffic-level = 2 [ set pcolor yellow ]
    if traffic-level = 3 [ set pcolor brown ]
    if traffic-level = 4 [ set pcolor gray ]
  ]
end

to move-taxis
  ask turtles [
    if dispatched? [
      let pickup-location first destination
      let pickup-patch patch (item 0 pickup-location) (item 1 pickup-location)

      move-algo pickup-patch

      if patch-here = pickup-patch [
        set color red
        set has-passenger? true
        set dispatched? false
        recolor-street patch-here

        let request-time item 2 destination
        let wait-time (ticks - request-time)
        set wait-times lput wait-time wait-times
        set ride-requests remove destination ride-requests
      ]
    ]

    if has-passenger? [
      let dropoff-location item 1 destination
      let dropoff-patch patch (item 0 dropoff-location) (item 1 dropoff-location)

      move-algo dropoff-patch

      if patch-here = dropoff-patch [
        set has-passenger? false
        set color black
        recolor-street patch-here
      ]
    ]

    if dispatched? and not member? destination ride-requests [
      ;; The ride was picked up already — cancel this taxi's trip
      set dispatched? false
      set destination []
      set color black
    ]

  ]
end

to move-algo [tpatch]
  if tpatch = nobody [ stop ]
  let next-move min-one-of neighbors4 with [is-street?] [distance tpatch]
  if next-move != nobody [
    let traffic-factor ([traffic-level] of next-move)
    face next-move
    if (ticks mod traffic-factor) = 0 [
      fd (speed / traffic-factor)
    ]
  ]
end

to-report is-corner? [p]
  let vertical? any? patches with [
    is-street? and
    pxcor = [pxcor] of p and
    abs(pycor - [pycor] of p) = 1
  ]
  let horizontal? any? patches with [
    is-street? and
    pycor = [pycor] of p and
    abs(pxcor - [pxcor] of p) = 1
  ]
  report vertical? and horizontal?
end

to-report average_wait_time
  if length wait-times > 0 [
    report mean wait-times
  ]
  report 0
end
@#$#@#$#@
GRAPHICS-WINDOW
381
10
799
429
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-20
20
-20
20
0
0
1
ticks
30.0

BUTTON
0
10
66
43
Setup
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

BUTTON
75
10
138
43
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
0
243
221
276
1. Generate Rides (Test Button)
generate-rides
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
0
49
172
82
num-taxis
num-taxis
1
50
25.0
1
1
NIL
HORIZONTAL

CHOOSER
0
89
138
134
dispatch-strategy
dispatch-strategy
"random" "nearest" "smart"
2

BUTTON
0
336
192
369
3. Move Test (Test Button)
move-taxis
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
0
291
231
324
2. Dispatch (Test Button)
dispatch-taxis \"nearest\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
4
145
127
190
average_wait_time
average_wait_time
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
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
0
@#$#@#$#@
