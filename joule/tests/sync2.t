$vdata = <p: {x: int, y: int | $z}, (not p): [int, int | $z]>;
(: v: $vdata, w: $wdata | $rest :) <= nil;
{x: int, y: int, z: int} <= $vdata;
nil <= $wdata $rest;
