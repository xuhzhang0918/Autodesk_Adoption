Assosiation rules are divided by geography, collection or both.
An example of association rule is:
	rule 	  support confidence  lift  count
{A,B} => {C}    0.2      0.7       2.5   100

rule: devices which own product A and B are very likely to also own product C.
support: 20% of all devices own product A and B.
confidence: For devices which own product A and B, 70% percent of them also own product C.
lift: For devices which own product A and B, they are 2.5 times more likely to own product C relative to the typical rate that C is owned.
count(count of observations): number of devices that own product A and B.