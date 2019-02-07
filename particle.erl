-module(particle).
-export([init/0]).

%% State of the particle.
-record(particle, {
	  position = [0,0,0,0,0],
	  velocity = [0,0,0,0,0],
	  cost = 0,
	  best_pos = [0,0,0,0,0],
	  best_cost = 0
	 }).

%% Initialize the particle and start the loop.
init() ->

    VarMin = -5,
    VarMax = 5,
    NVars = 5,

    VarInfo = {VarMin, VarMax, NVars},

    Position = random_pos(VarMin, VarMax, NVars),
    Cost = cost_functions:rastrigin_function(Position),

    State = #particle{
	       position = Position,
	       cost = Cost,	    
	       velocity = [0 || _ <- lists:seq(1, NVars)],
	       best_pos = Position,
	       best_cost = Cost},

    %print_state(State),
    loop(State, VarInfo).

%% The "main loop" for all particles.
loop(State, VarInfo) ->
    
    #particle{position = Position, velocity = Velocity, cost = _,
	      best_pos = BestPos, best_cost = BestCost} = State,

    {VarMin, VarMax, NVars} = VarInfo,
  
    receive
	{global_best, {_, GlobalBestPos}} ->
 
	    NewVelocityTemp = update_velocity(Velocity, NVars, Position, BestPos, GlobalBestPos),
	 
	    NewVelocity = limit_velocity(NewVelocityTemp, VarMin, VarMax),

	    
	    NewPositionTemp = particle_update:add_lists(Position, NewVelocity),
	    
	    NewPosition = limit_position(NewPositionTemp, VarMin, VarMax),
	    

	    NewCost = cost_functions:rastrigin_function(NewPosition),
	    
	    {NewBestCost, NewBestPos} = update_localbest(NewCost, NewPosition, BestCost, BestPos),
	    
	    NewState = #particle{
		       position = NewPosition,
		       cost = NewCost,
		       velocity = NewVelocity,
		       best_pos = NewBestPos,
		       best_cost = NewBestCost},
	    
	  
	    pso ! {local_best, {NewBestCost, NewBestPos}},
	    
	    loop(NewState, VarInfo);

	get_local_best ->
	    pso ! {local_best, {BestCost, BestPos}},
	    
	    loop(State, VarInfo);
	die ->
	    die;
	Any ->
	    io:format("Received undefined message ~p~n", [Any]),
		loop(State, VarInfo)
    end.

%% Generate N random variables between Min and Max.
random_pos(VarMin, VarMax, NVars) ->
    Differance = VarMax - VarMin,
    [VarMin + (Differance* rand:uniform()) || _ <- lists:seq(1,NVars)].
    
%% Print out state of the particle.
print_state(State) ->
    #particle{position = Position, velocity = Velocity, cost = Cost,
	      best_pos = BestPos, best_cost = BestCost} = State,
    
    io:format("~nParticle ~p:~nPosition - ~p~nVelocity - ~p~nCost - ~p~nBestPos - ~p~nBestCost - ~p~n~n",
	      [self(), Position, Velocity, Cost, BestPos, BestCost]).

	    

update_velocity(Velocity, NVars, Position, BestPos, GlobalBestPos) ->

    %% 2 Random velocities.
    RandomVelo1 = [particle_update:get_phi1() * rand:uniform() ||
		      _ <- lists:seq(1,NVars)],
    RandomVelo2 = [particle_update:get_phi2() * rand:uniform() ||
		      _ <- lists:seq(1,NVars)],

    %% Local and global best in relation to current position.
    LocalBestTemp = particle_update:subtract_lists(BestPos, Position),
    GlobalBestTemp = particle_update:subtract_lists(GlobalBestPos, Position),
	    
    %% Multily the random velocities with local and global best in relation to current position.
    LocalBestTemp2 = particle_update:mult_lists(RandomVelo1, LocalBestTemp),	    
    GlobalBestTemp2 = particle_update:mult_lists(RandomVelo2, GlobalBestTemp),
	    
    %% Add LocalBestTemp2 and GlobalBest2 together.
    NewVelocityTemp = particle_update:add_lists(GlobalBestTemp2, LocalBestTemp2),
    
    %% Add NewVelocityTemp to the current velocity and return.
    particle_update:add_lists(Velocity, NewVelocityTemp).

limit_velocity(Velocity, VarMin, VarMax) ->
    MaxVelocity = 0.2 * (VarMax - VarMin),
    MinVelocity = -MaxVelocity,
    VelocityTemp1 = [max(Vel, MinVelocity) || Vel <- Velocity] ,
    [min(Vel, MaxVelocity) || Vel <- VelocityTemp1].

limit_position(Position, VarMin, VarMax) ->
    PositionTemp1 = [max(Pos, VarMin) || Pos <- Position],
    [min(Pos, VarMax) || Pos <- PositionTemp1].
    
update_localbest(NewCost, NewPos, BestCost, _) when NewCost < BestCost ->
    {NewCost, NewPos};
update_localbest(_, _, BestCost, BestPos) ->
    {BestCost, BestPos}.
