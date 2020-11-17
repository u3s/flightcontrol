# flightcontrol
Erlang concurrency example for educational purposes.

## simulation
1. planes moving around X,Y surface
2. X, Y are non-negative values, together they form plane coordinates that is {X, Y} = {Plane distance from airport, Lane number}
3. {X, Y} = {0, 0} - point representing the airport, if reached by a plane landing is considered succesful
4. plane crashes may occur in the air (collision between planes) or while landing (when Distance is 0 but Lane is not 0)

## references
1. https://digitaljoel.nerd-herders.com/2011/03/31/erlang-concurrency-demo-application/
