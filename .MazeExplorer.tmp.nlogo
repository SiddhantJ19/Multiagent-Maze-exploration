globals [
  patch-data dimX dimY row i j
  ; global counter
  num-message-exchanges
]
breed [circles circle]

breed [robots robot]

breed [towers tower]

robots-own [
  battery-drainage ; for overall analysis

  visited ; patch-set -> all patches visited by the robot
  message-buffer ; patch-set -> every 10th tick robots recieves set of patches communicated by other robots
  communicated ; patch-set -> all patches which may be visited by the robot but communicated by the peer robots;

  finished ; bool -> if finished, robot do not move

  isleader ; bool -> true if the robot is leader for current message passing

  num-steps ; int -> num of steps travelled by the robot
;  last-updated ; ticks

  ; when pioneer broadcasts
  exit-found ; bool;
  pioneer   ; robot

; new implementation of decentralized comm
  my-leader
]

patches-own [
  parent-patch ; patch -> the patch from which robot moved to this patch
  isvisited ; bool -> if the patch is visited by any of the robots

  visited-by ; turtle-set -> set of robots visited this patch
]


towers-own [

  pioneer
  exit-found

  memory ; patch-set
  message-buffer
]

;
to load-own-patch-data
  let file user-file
  if ( file != false )
  [
    set patch-data []
    file-open file
    set dimX file-read
    set dimY file-read
    set i dimX - 1
    while [i >= 0 ]
    [
      set row file-read
      set j 0
      while [j < dimY ]
      [
        ; user-message "i j"
        set patch-data sentence patch-data (list (list i j get-color item j row ))
         set j j + 1
      ]
      set i i - 1
    ]
    user-message "File loading complete!"
    file-close
  ]
end

to-report get-color [char]
  if char = "w" [report white]
  if char = "c" [report yellow]
  if char = "B" [report red]
  if char = "E" [report blue]
  if char = "T" [report pink]
end

to show-patch-data
  clear-patches
  clear-robots
  ifelse ( is-list? patch-data )
    [ foreach patch-data [ ?1 -> ask patch first ?1 item 1 ?1 [ set pcolor last ?1 ] ] ]
    [ user-message "You need to load in patch data first!" ]
  display
end


to clear-towers
  ask towers [die]
end
to clear-robots
  ask robots [die]
end


to setup
  ;clear-all
  set num-message-exchanges 0
  reset-ticks
  clear-robots
  clear-towers
  clear-drawing

  init-patches
  init-robots-at-source
  init-towers
end

to init-towers
  ask patches with [pcolor = pink]
  [
    sprout-towers 1
    sprout-circles 1 [set shape "circle 3" set xcor pxcor set ycor pycor set size comm-range * 2 stamp die]
  ]
  ask towers [
      set shape "flag"
      set message-buffer (patch-set)
      set memory (patch-set)
      set exit-found false
      set pioneer nobody
  ]
end

to init-patches
  ask patches [
    set isvisited false
    set visited-by (turtle-set); initializing empty robot set
  ]
end

to clear-all-robots
  ask robots [die]
end

to init-robots-at-source
  create-robots num-robots
  ask robots [
    set isleader false
    set finished false
    move-to one-of patches with [pcolor = blue]
    set pen-size 2
    set visited (patch-set) pd ; initializing empty visited
    set message-buffer (patch-set) ; empty message buffer since no communication happened
    set communicated (patch-set) ; empty communicated since no communication happened
    set exit-found false
    set num-steps 0
    ;set last-updated 0

    set visited (patch-set visited patch-here) ; updating visited by adding current patch
    set pioneer nobody
    set battery-drainage 0
  ask patch-here [
    set parent-patch nobody
    set isvisited true
    set visited-by (turtle-set visited-by myself) ; myself = robot which called ask patch-here;
  ]
 ]
end


to go
  ifelse collaboration = true
  [path-finder-collaboration]
  [path-finder-basic]
  if all? robots [pcolor = red] [stop]
  tick
end

; collaborative path finding
to path-finder-collaboration
  ; communication
  ifelse (ticks mod 11) = 0 ; every ten ticks, communicate
  [
    ifelse communication-type = "Decentralized"
    [
      ;leader-election-communication-normal
      leader-election-communication-rangeBased
    ]
    [
      tower-communication
    ]

  ]
  ; Motion planning
  [ ; when not communicating, move
    if any? links [ask links [die]]
    path-finder-collab-pioneer;
  ]
end

to tower-communication
  ask robots [
    if (any? towers in-radius comm-range) ;and (ticks - last-updated > 5)
    [

      set num-message-exchanges num-message-exchanges + 1
      set battery-drainage battery-drainage + comm-range * communication-range-battery-drainage-factor

      ;set last-updated ticks
      let curr-tower one-of towers in-radius comm-range
      create-link-to curr-tower
      ; send current knowledge base to tower
      ; tower then sends the combined message-buffer
      ask curr-tower [
        set message-buffer [visited] of myself
        set memory (patch-set memory message-buffer)
        set message-buffer(patch-set)
      ]

      set exit-found [exit-found] of curr-tower
      set pioneer [pioneer] of curr-tower
      set message-buffer [memory] of curr-tower

      ;everyone process-messages -> move messages to visited
      process-messages
    ]
  ]
end


to leader-election-communication-normal
  ; elect leader -> choose one randomly
  let leader one-of robots

  ; leader has empty message-buffer
  ask leader [
    set isleader true
  ]

  ; other robots send their visited nodes info as messages to leader
  ask robots with [isleader = false] [
    set battery-drainage battery-drainage + comm-range * communication-range-battery-drainage-factor
    create-link-to leader
    send-message-to-leader self leader
    set num-message-exchanges num-message-exchanges + 1
  ]

  ; leader then sends the combined message-buffer
  ask leader [
    let num-connected-neighbours count link-neighbors
    set battery-drainage battery-drainage + comm-range * communication-range-battery-drainage-factor * num-connected-neighbours
    send-knowledge-base leader
  ]

  ;everyone process-messages -> move messages to visited
  ask robots [process-messages]

  ; remove leader
  ask leader [
    set isleader false
  ]
end

to leader-election-communication-rangeBased
  ;ask turtles [ show turtles in-radius 1 ]
  let queue []
  let comm-visited []
  let leader-who 0
  ask robots [
    if not member? who comm-visited [
      ; leader has no leader
      set my-leader nobody
      set leader-who who
      let candidates robots in-radius comm-range

      let num-connected-neighbours (count candidates) - 1
      set num-message-exchanges num-message-exchanges + num-connected-neighboursneighbours
      set battery-drainage battery-drainage + comm-range * communication-range-battery-drainage-factor * num-connected-neighbours
      set queue sort [who] of candidates
      foreach queue [x ->
        if not member? x comm-visited[
          if x != who [
            ask robot x [
              set num-message-exchanges num-message-exchanges + 1
              set battery-drainage battery-drainage + comm-range * communication-range-battery-drainage-factor
              set my-leader leader-who
              ; send message to leader
              send-message-to-leader self (robot leader-who)
              create-link-to robot leader-who
            ]
            set comm-visited insert-item 0 comm-visited x
          ]
        ]
      ]

      ; send back messages to peer
      send-knowledge-base robot leader-who
    ]
  ]

  ;everyone process-messages -> move messages to visited
  ask robots [process-messages]

end

to process-messages
  ;show who
  ; copy patches to visited
  ; show "visited"
  ; show visited
  set communicated (patch-set communicated message-buffer)
  ; show visited
  ; clear message-buffer
  ; show "MB"
  ; show message-buffer
  set message-buffer (patch-set)
  ; show message-buffer
end

to send-knowledge-base [leader]
  ask leader [
    ; show "leader"
    ; show leader
    ask link-neighbors [
      ; show who
      ; show message-buffer
      set num-message-exchanges num-message-exchanges + 1
      set message-buffer (patch-set [message-buffer] of leader)
      ; show message-buffer
    ]
  ]
end

to send-message-to-leader [curr leader]
  let visited-curr [visited] of curr
  ask leader [
    set message-buffer (patch-set message-buffer visited-curr)
  ]
end



to path-finder-collab-pioneer
  ask robots with [finished = false]
  ; pioneer notifiacation
  [
    ifelse exit-found = true ; if any of the robots found the exit; then follow the pioneer's path
    [
      ; 2nd phase, Following Pioneer's path
      let current-patch-visited-by [visited-by] of patch-here ; set of agents who visited current patch
      ifelse member? pioneer current-patch-visited-by
      [
        ; if current patch is visited by the pioneer robot, trace the path of the pioneer robot which is not visited by current robot
        let pioneer-visited [visited] of pioneer
        let candidates neighbors4 with [
          not is-wall self and
          not member? self [visited] of myself and
          member? self pioneer-visited
        ]
        ifelse any? candidates
        [
          moveToOneOf candidates
        ]
        [
          backTrackFromHere
        ]
      ]
      ; if current patch is not visited by the pioneer robot, backtrack
      [
        backtrackFromHere
      ]
    ]
    ; 1st phase
    [
      path-finder-collab
    ]

    if pcolor = red [
      set finished true
      set exit-found true
     ask robots in-radius comm-range [
            ifelse [pioneer] of myself = nobody
      [
        set pioneer myself
      ]
      [
        set pioneer [pioneer] of myself
      ]
      set exit-found true
    ]
  ]
  ]
end

to path-finder-collab
    let candidates neighbors4 with [not member? self [visited] of myself and not is-wall self]
    ifelse any? candidates
    [
      let not-communicated-candidates candidates with [not member? self [communicated] of myself]
      ifelse any? not-communicated-candidates
      ; if any candidate neighbor is present which is neither visited nor communicated
      [
        moveToOneOf not-communicated-candidates
      ]
      ; if all the neighbor candidates are not visited but all are communicated, then no choice, so move to one of the communicated
      [
        moveToOneOf candidates
      ]
    ]
    ; if no neighbors, backtrack
    [
      backtrackFromHere
    ]

    set battery-drainage battery-drainage + 1

    if pcolor = red and exit-found = false [
      set finished true
      set exit-found true


    ifelse communication-type = "Decentralized"
    [
      ask robots in-radius comm-range[
        set pioneer myself
        set exit-found true
      ]
    ]
    [
      ask towers in-radius comm-range [
        set exit-found true
        set pioneer myself
      ]
    ]
    ]
    if pcolor = red [
    set finished true
    set exit-found true
    ask robots in-radius comm-range [
      ifelse [pioneer] of myself = nobody
      [
        set pioneer myself
      ]
      [
        set pioneer [pioneer] of myself
      ]
      set exit-found true
    ]
  ]

end







to backtrackFromHere
  let parent-of-curr [parent-patch] of self
  ifelse parent-of-curr = NoBody [die]
  [
    move-to parent-of-curr
    set num-steps num-steps + 1
  ]
end

to moveToOneOf [candidates]
    let parent patch-here
    move-to one-of candidates
    set num-steps num-steps + 1
    ask patch-here [
      set isvisited true
      set visited-by (turtle-set visited-by myself)
    ]

    ask patch-here [set parent-patch parent]
    set visited (patch-set visited patch-here)
end

to-report is-wall [curr]
  ifelse (pcolor = white or pcolor = pink) [ report true ] [report false ]
end


to path-finder-basic
  ask robots with [finished = false] [
    ; check if self (candidate which is a patch) is not visited
    let candidates neighbors4 with [not member? self [visited] of myself and not is-wall self ]
    ifelse any? candidates
      [
        moveToOneOf candidates
      ]
      ; if stuck ie no neighbors then backtrack
      [
        backtrackFromHere
      ]

    set battery-drainage battery-drainage + 1
    if pcolor = red [set finished true]
  ]
end








@#$#@#$#@
GRAPHICS-WINDOW
316
34
892
611
-1
-1
18.933333333333334
1
10
1
1
1
0
0
0
1
0
29
0
29
1
1
1
ticks
30.0

BUTTON
20
85
211
118
NIL
show-patch-data
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
8
126
226
159
NIL
load-own-patch-data
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
27
179
100
212
NIL
setup\n
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
119
177
182
210
NIL
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

SWITCH
27
320
182
353
collaboration
collaboration
0
1
-1000

SLIDER
19
264
192
297
num-robots
num-robots
1
200
12.0
1
1
NIL
HORIZONTAL

MONITOR
1067
194
1193
239
% maze explored
100 * count patches with [ isvisited = true] / count patches with [pcolor = yellow]
3
1
11

MONITOR
1080
333
1191
378
Avg num steps
sum [num-steps] of robots / count robots
17
1
11

MONITOR
1089
283
1177
328
Total Steps
sum [num-steps] of robots
17
1
11

SLIDER
21
447
193
480
comm-range
comm-range
1
50
7.0
1
1
NIL
HORIZONTAL

CHOOSER
25
384
195
429
communication-type
communication-type
"central" "Decentralized"
0

MONITOR
1052
423
1211
468
num-message-exchanges
num-message-exchanges
17
1
11

MONITOR
203
169
260
214
leaders
count robots with [my-leader = nobody]
17
1
11

INPUTBOX
1312
242
1568
302
communication-range-battery-drainage-factor
2.0
1
0
Number

TEXTBOX
1311
310
1567
385
This can be based on wireless technology, protocols, number of communication handshakes
12
0.0
1

MONITOR
1308
401
1487
446
average-battery-drainage
sum [battery-drainage] of robots / num-robots
17
1
11

TEXTBOX
44
362
194
380
Search Strategies\n
12
0.0
1

@#$#@#$#@
## WHAT IS IT?

Multi Agent Maze Exploration.

In recent years, Mazes have been used to examine the artificial intelligence of robots by observing their ability to traverse mazes using algorithm for maze exploration. 
The modelling is being done to find good stratagies to improve reliability, restrict resource usage etc.

This model implements search and rescue robots as agents and the goal for the swarm is not only to find the exit but all the agents must reach the exit.

This implementation can help in deciding optimal number of robot agents needed to solve a maze of certain size. Optimality of Swarm depends on

1 Time taken to solve the maze
2 Resources use
2.1 Battery Usage
2.2 Number of Message Exchanges
2.3 Number of steps
3 Percentage of Maze explored

## Agents
### Robots
#### Behavior
1. Traverse Maze
2. Commuincates with other robots
3. Communicates with Towers
4. Decides on direction to visit based on Knowledge Base

#### Attributes
1. Battery - Modelled as Battery Drainage. Depends on steps covered, communication distance, communication technology, communication stratagy (leader vs no leader)
2. Communication range - MAx distance, robots can communicate with each other; same for all robots
3. Communication technology - same for all robots. This factor inculcates any communication technology and effects battery.
4. Memory - Infinite memory

### Towers

#### Behavior
1. Communicate with robots

#### Attributes
2. Communication range
3. Memory - Infinite Memory

## Implementation

To solve the maze, we have implemented 3 stratagies. 1 Non-Collaborative and 2 collaborative stratagies.

### Non-collaborative (Independent) Exploration .
The robots search for exit on their own. There are noways of collaboration.
The robots maintain the record of the cells they have visited and recursively finds the exit.
### Collaborative Exploration - Decentralized

Every 10 ticks, the robots communicate. The communication is done as follows

1. Leader Election - Set of leader robots are elected such that **distance between any 2 leaders > communication range**
2. Data-Collection - The robots send their visited record to their corresponding leader
3. Data-Distribution - The leader then distributes the collected knowledge base to peers
4. Data Processing - The robots recieve the message from the leader and store it    in-memory but seperate from their own visited records

The communication is done every 10 ticks. Rest of the time, the turtles move based on 2 seperate knowledge-bases, one that they themselves have visited and the communicated KB.

At every junction a turtle has the follwing order of preference

1. Move to neighbor not visited and not communicated
2. If all neighbors are either visited or communicated, go to the communicated neighbor
3. If no neighbor or all neighbors visited, bactrack the path

Finally, if one of the robots reaches the exit, the peers (within the range) are communicated that the exit has been found and which robot (pioneer) found the exit. 
The robots then try to find the respective nearest node common with the pioneer. Once the common node is found they trace the pioneer's path which is guaranteed to lead to the exit.

### Collaborative Exploration - Centralized
In situations where individual robots are incapable of behaving as leaders, a more centralized approach is used where central towers help robots navigate through the unknown territory. This also depends on economic and structural feasibility of such towers.

The Communication flow is as follows

1. Every 10th tick, the towers communicate with nearby robots and exchange KB
2. The towers and robots update their KB from the robots

**The Motion of robots is same as described in the decentralized approach**

## Analysis
Followed in the excel sheets


## Assumptions
1. Infinite memory - Modern Robotics carry enough memory to mask a lidar point cloud of a whole factory
2. Every Message exchange takes one tick, this can however be varied based on the communication distance
3. We assumed the ticks are in sync for all the robots, though this is not true in distributed settings but we don't forsee any emergence coming from implementing distributed clocks

## References
http://car.ensm-douai.fr/CAR2010/Presentations/10-doniec-car2010-slides.pdf
https://www.researchgate.net/publication/257655885_Multi-Agent_Maze_Exploration
http://www.swarmrobot.org/Communication.html
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

circle 3
true
0
Circle -955883 false false -2 -2 302

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
NetLogo 6.1.1
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? robots [pcolor = red]</exitCondition>
    <metric>ticks</metric>
    <metric>sum [num-steps] of robots / count robots</metric>
    <metric>100 * count patches with [ isvisited = true] / count patches with [pcolor = yellow]</metric>
    <metric>num-message-exchanges / num-robots</metric>
    <metric>sum [battery-drainage] of robots / num-robots</metric>
    <enumeratedValueSet variable="comm-range">
      <value value="5"/>
      <value value="8"/>
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-robots">
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="3"/>
      <value value="4"/>
      <value value="10"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="30"/>
      <value value="50"/>
      <value value="80"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="communication-type">
      <value value="&quot;central&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="communication-range-battery-drainage-factor">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collaboration">
      <value value="true"/>
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
