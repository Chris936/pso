-module(pso).
-export([start/0]).

%% Init all. Set up algorithm.
start() ->
    GlobalBestCost = 10000000, % Set very high for minimization problems.
    NumberOfIterations = 1000,
    register(pso, self()),
    Particles = spawn_n(1000),

    broadcast(Particles, get_local_best),
    sleep(100),

    %% NewGlobalBest is a 2-tuple - {cost, pos}.
    NewGlobalBest = search_mailbox({GlobalBestCost, []}),
    
    loop(Particles, NewGlobalBest, NumberOfIterations).

%% Main loop of algorithm.
loop(P, GlobalBest, 0) ->
    broadcast(P, die),
    finished(GlobalBest);

loop(Particles, GlobalBest,NumberOfIterations) ->

    broadcast(Particles, {global_best, GlobalBest}),
    sleep(1),
    NewGlobalBest = search_mailbox(GlobalBest),

    loop(Particles, NewGlobalBest, NumberOfIterations-1).

%% Finish the algorithm.
finished(GlobalBest) ->
    {GlobalBestCost, GlobalBestPos} = GlobalBest,
    io:format("Finished. Global best was ~p~n", [GlobalBestCost]),
    io:format("Position - ~p~n", [GlobalBestPos]).

%% Spawn N number of particles.     
spawn_n(N) ->
    [spawn(particle, init, []) || _ <- lists:seq(1,N)].

%% Broadcast Message to a list of particles.
broadcast(Particles, Message) ->
    [P ! Message || P <- Particles].

%% Sleep helper function.
sleep(X) ->
    receive
    after X ->
	    true
    end.

%% Search through the mailbox. Try to find a better GlobalBest.
search_mailbox({GlobalBestCost, GlobalBestPos}) ->
    receive
	{local_best, {LocalBestCost, LocalBestPos}}
	  when LocalBestCost < GlobalBestCost ->
	    search_mailbox({LocalBestCost, LocalBestPos});
	{local_best, {_, _}} -> 
	    search_mailbox({GlobalBestCost, GlobalBestPos});
	Any ->
	    io:format("Received undefined message ~p~n", [Any])
		
    after 0 ->
	    {GlobalBestCost, GlobalBestPos}
    end.
