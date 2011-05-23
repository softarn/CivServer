-module(city_bombard).
-export([bombard/4]).

bombard(AttackerStr, AtkMP, AtkTerrStr, DefList) ->
	random:seed(now()),
	Attacker = list_to_atom(string:to_lower(AttackerStr)),
	AtkAttrs = unit_attr:get_attr(Attacker),
	AtkTerr = string:to_lower(AtkTerrStr),
	AP = element(2, AtkAttrs),
	NewAP = round(AP * (1 + element(1, terr_attr:get_attr(AtkTerr)))),
	AUDP = round((NewAP * AtkMP) / 100),
	DAU = (random:uniform(AUDP + 1) - 1) + (random:uniform(AUDP + 1) - 1),
	DPU = round(DAU / length(DefList)),	% Damage per unit.
	[X-DPU || X <- DefList].