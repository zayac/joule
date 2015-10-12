$_vdata = <p: {x: int, y: int | $_z}, (not p): [int, int | $_z]>;
(: v: $_vdata, w: $_wdata | $^rest :) <= nil;
{x: int, y: int, z: int} <= $_vdata;
nil <= $_wdata $^rest;
