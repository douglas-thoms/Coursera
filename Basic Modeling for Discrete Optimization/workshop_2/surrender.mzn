% workshop 2
% select a set of trade negotiation party
enum NEGOTIATOR;
NEGOTIATOR: dummy; 

int: l; % minimum party size
int: u; % maximum party size
int: m; % minimum joint ability

var int: party_size;
array[NEGOTIATOR] var NEGOTIATOR: selected_party;

array[NEGOTIATOR] of int: honor;
%joint honour
array[NEGOTIATOR,NEGOTIATOR] of int: joint;

constraint card(party_size) >= l;
constraint card(party_size) <= u;
constraint forall(i in 1..party_size-1)(honor[negotiator_party[i]] >= 
                                          (honor[negotiator_party[i]] != 0) + honor[negotiator_party[i+1]]);

%1 set an array using l..u - bounded cardinality
%create selected team array between size of l..u
%allow dummy variable

%constraint honor of negotiation party (joint ability) >= m
% this is done by using all negotiators in selected team array and selecting the subscripts i and j in array joint
var int: total_honor = sum(i,j in joint)(joint[i,j]);
constraint total_honor >= m;


%total negotiation strength of party = sum(negotiation strength of all pairs



%minimize sum of negotiators in party
var int: total_individual_honour = sum(i in party_size)(honour[i]);

minimize total_individual_honour;
