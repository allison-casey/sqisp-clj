(hint (42));
(hint (42.24));
(hint "hello");
(hint ":world");
(hint null);
(hint true);
(hint [[":hash"],["map"]] call hash_map);
(hint [["values","of",":set",":a"]] call hash_set);
(hint ["a",":vector",[":nested"]]);
(hint ((allUnits) select (2)));
something = "hello";
hello_world = ":42";
if ((count (allUnits))) then {
(hint "we have some units");
} else {
(hint "we don't have any units");
};
(hint (if ((allUnits)) then {
"hello"} else {
"world"}));
hello = (call {
(hint "calling global")
(42);
})
;
private _hello = "world";
private _something = ":else";
(hint _hello);

_something;
(((2) + (3) + (4) + ((2.2) * (3)) + (((1) / (2)) - (1))) % (2) % (2));
((1) isEqualTo (1) isEqualTo (1));
!((1) isEqualTo (2));
true;
false;
((2) > (1));
((0) < (1) and (1) < (2));
((0) <= (0));
((1) >= (2));
true;
true;
