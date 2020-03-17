private _hello = "world";
private _something = ":else";
(hint _hello);

_something;
(hint (call {private _something = "hello world";
_something;
}));
(hint ((allUnits) select (2)));
something = "hello";
hello_world = ":42";
(hint (42));
(hint (42.24));
(hint "hello");
(hint ":world");
(hint null);
(hint true);
(hint [[":hash"],["map"]] call hash_map);
(hint [["values","of",":set",":a"]] call hash_set);
(hint ["a",":vector",[":nested"]]);
if ((count (allUnits))) then {
(hint "we have some units");
} else {
(hint "we don't have any units");
};
(hint (if ((allUnits)) then {
"hello"} else {
"world"}));
