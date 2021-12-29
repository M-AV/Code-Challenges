﻿module Calculations

// Useful for calculating a distance through a grid
// https://en.wikipedia.org/wiki/Taxicab_geometry
let manhattanDistance2D (x1, y1) (x2, y2) =
    abs(x1 - x2) + abs(y1 - y2)
let manhattanDistance3D (x1, y1, z1) (x2, y2, z2) =
    abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)