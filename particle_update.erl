-module(particle_update).
-export([
	get_kappa/0,
	get_phi1/0,
	get_phi2/0,
	get_phi/0,
	get_chi/0,
	mult_lists/2,
	add_lists/2,
	subtract_lists/2]).

get_kappa() ->
    1.

get_phi1() ->
    2.05.

get_phi2() ->
    2.05.

get_phi() ->
    get_phi1() + get_phi2().
    
get_chi() ->
    Phi = get_phi(),
    2 * get_kappa() / abs(2 - Phi - math:sqrt(math:pow(Phi, 2) - 4 * Phi)).
    


mult_lists(List1, List2) ->
[List1Element * List2Element || {List1Element, List2Element
				} <- lists:zip(List1,List2)].
add_lists(List1, List2) ->
[List1Element +  List2Element || {List1Element, List2Element
				} <- lists:zip(List1,List2)].
subtract_lists(List1, List2) ->
[List1Element -  List2Element || {List1Element, List2Element
				} <- lists:zip(List1,List2)].
