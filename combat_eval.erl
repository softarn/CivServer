%%  Combat evaluation module.
%% 	Author: 	Arian Jafari
%%	aria-jaf@dsv.su.se
%% ========================================
%% Combat is evaluated by calling the combat/4 method,
%% sending two atoms (the unit names) and their current
%% manpower. The function then returns a tuple containing
%% the units' updated manpower, for instance {87, 34}.
%% 
%% This function pulls unit data from the "unit_attr"
%% and its get_attr/1 function.
%%
%% Terrain bonuses (e g attack/defense combat modifiers)
%% are not yet implemented, but it will be implemented in
%% the next version.

-module(combat_eval).
-export([combat/4]).

combat(Attacker, AtkMP, Defender, DefMp) ->
	case element(6, unit_attr:get_attr(list_to_atom(Attacker))) of
		assault ->
			assault_combat(list_to_atom(Attacker), AtkMP, list_to_atom(Defender), DefMp);
		range ->
			ranged_combat(list_to_atom(Attacker), AtkMP, DefMp);
		bombardment ->
			bombard_combat(list_to_atom(Attacker), AtkMP, DefMp)
	end.
	
assault_combat(Atk, AtkMP, Def, DefMP) ->
	Charges = random:uniform(12) + random:uniform(12),
	AtkAttrs = unit_attr:get_attr(Atk),
	DefAttrs = unit_attr:get_attr(Def),
	AP = element(2, AtkAttrs),
	% Add attack bonus from terrain
	% NewAP = AP * TerrAtk,
	case Def of
		pikeman ->
			case element(7, unit_attr:get_attr(Atk)) of
				mounted ->
					DP = (element(2, element(3, DefAttrs)));
				_ ->
					DP = (element(1, element(3, DefAttrs)))
			end;
		_ ->
			DP = (element(3, DefAttrs))
	end,
	%DP = element(3, DefAttrs),
	% Add defense bonus
	% NewDP = DP * TerrDef,
	assault_charge(AP, AtkMP, DP, DefMP, Charges).

assault_charge(_, AtkMP, _, DefMP, 0) ->
	{AtkMP, DefMP};
assault_charge(_, AtkMP, _, DefMP, _) when (AtkMP < 1) or (DefMP < 1) ->
	{AtkMP, DefMP};
assault_charge(AP, AtkMP, DP, DefMP, Charges) ->
	AUDP = round((AP * AtkMP) / 100),
	DUDP = round((DP * DefMP) / 100),
	DAU = (random:uniform(AUDP + 1) - 1) + (random:uniform(AUDP + 1) - 1),
	DDU = (random:uniform(DUDP + 1) - 1) + (random:uniform(DUDP + 1) - 1),
	NewAtkMP = AtkMP - DDU,
	NewDefMP = DefMP - DAU,
	assault_charge(AP, NewAtkMP, DP, NewDefMP, Charges-1).

ranged_combat(Attacker, AtkMP, DefMP) ->
	Charges = random:uniform(3) + random:uniform(3),
	AtkAttrs = unit_attr:get_attr(Attacker),
	AP = element(2, AtkAttrs),
	% Add attack bonus from terrain
	% NewAP = AP * TerrAtk,
	ranged_charge(AP, AtkMP, DefMP, Charges).
	
ranged_charge(_, AtkMP, DefMP, 0) ->
	{AtkMP, DefMP};
ranged_charge(_, AtkMP, DefMP, _) when (DefMP < 1) ->
	{AtkMP, DefMP};
ranged_charge(AP, AtkMP, DefMP, Charges) ->
	AUDP = round((AP * AtkMP) / 100),
	DAU = (random:uniform(AUDP + 1) - 1) + (random:uniform(AUDP + 1) - 1),
	NewDefMP = DefMP - DAU,
	ranged_charge(AP, AtkMP, NewDefMP, Charges-1).

bombard_combat(Attacker, AtkMP, DefMP) ->
	AtkAttrs = unit_attr:get_attr(Attacker),
	AP = element(2, AtkAttrs),
	% Add attack bonus from terrain
	% NewAP = AP * TerrAtk,
	AUDP = round((AP * AtkMP) / 100),
	DAU = (random:uniform(AUDP + 1) - 1) + (random:uniform(AUDP + 1) - 1),
	NewDefMP = DefMP - DAU,
	{AtkMP, NewDefMP}.
