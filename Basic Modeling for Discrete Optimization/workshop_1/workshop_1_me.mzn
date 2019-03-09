% Use this editor as a MiniZinc scratch book
%Write a model named temperature.mzn to determine the minimal costs to keep the buildingtemperature within the required range of 25 to 30 degrees.

%create array of decision variables

array[int] of int: readings;
int: start;
int: n = length(readings);
set of int: HOUR = 1..n;
set of int: HOUR0 = 0..n;

array[HOUR0] of var 25..30: temp;
array[HOUR] of var ACTION: choice;
array[HOUR] of var 25..30: tp;

constraint forall(h in HOUR)(tp[h] = temp[h]);

enum ACTION = {heat, strongly_heat, cool, strongly_cool, do_nothing};

array[ACTION] of int: temp_change = [+1,+4,-2,-5,0];
array[ACTION] of int: cost = [1,5,3,9,0];



constraint temp[0] = start;



%create the decision making process for temp
constraint forall(h in HOUR)(temp[h] = (readings[h] + temp[h-1]) div 2 + temp_change[choice[h]]);

var int: total_cost = sum(h in HOUR)(cost[choice[h]]);

solve minimize total_cost;

%The expected output is the temperature readings at each time, the choices made at each hour, and the total cost.
%temp = [25, 30, 30, 25, 26, 27];
%choice = [do_nothing, cool, do_nothing, strongly_heat, strongly_heat];
%cost = 13;

output ["temp = \(tp);\n"];
output ["action = \(choice);\n"];
output ["cost = \(total_cost);\n"];