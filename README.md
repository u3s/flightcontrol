# flightcontrol
Erlang concurrency example for educational purposes.

## simulation
1. planes moving around X,Y surface
2. X, Y are non-negative values, together they form plane coordinates that is {X, Y} = {Plane distance from airport, Lane number}
3. {X, Y} = {0, 0} - point representing the airport, if reached by a plane landing is considered succesful
4. plane crashes may occur in the air (collision between planes) or while landing (when Distance is 0 but Lane is not 0)
5. Planes parameters are:
  - Speed (higher value - slower plane),
  - Distance - how far from there airport
  - Lane - 0 is the Lane enabling a proper landing
6. succesfull landing happens if plane reaches {0, 0} position
     
```
      2  1  0 (Distance from airport)
    2 P-----
    1 ------
    0 ------AIRPORT
(Lane
number)    
```


## runnning code
1. get Erlang installed (erl command present in PATH)
2. start Erlang shell with "erl" command
3. compile and load Erlang module with "c(flightcontrol), l(flightcontrol)."
4. start selected simulation function for example with "flightcontrol:simulate()."

## references
1. https://digitaljoel.nerd-herders.com/2011/03/31/erlang-concurrency-demo-application/
