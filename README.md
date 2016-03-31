# MaSiNes
Lightweight Signal Machine simulator

MaSiNes is a lightweight *Signal Machine* simulator written in modular OCaml.

*Signal Machines*, also referred to as *Abstract Geometrical Computation* model in the scientific literature, is a simple symbolic and geometric computation model working on the two-dimensional Euclidean plane as its space-time playground.

MaSiNes was originally written for the sake of fun. It is however simple and fast and has been used by a former colleague in his research work, to generate figures for scientific articles and a PhD thesis.

![Primes sieve](/img/primes.svg)

## Requirements

An OCaml distribution (v4.x or whatever), the external OCaml library xml-light (`libxml-light-ocaml-dev` on Ubuntu/Debian) and the lib Cairo bindings (`libcairo-ocaml-dev` on Ubuntu/Debian).

## Contents

- `v1.1` contains the current version: simple and efficient; XML input format; several convenient output format (PDF via Cairo, SVG, PGF/Tikz).
- `v1.0` contains the obsolete first version: not optimized; bad *ad-hoc* input format and Python scripts to convert XML to this format; stays here for **archive** purpose only as it features an experimental 2D Signal Machine simulator (working on the three-dimensional Euclidean plane) that has not yet ported to the current version.
- `MaSiVi3d` is a poor 3D visualization Python script (using `pygame`) to visualize the 3D SVG-like output files (`.svg3d`) generated by the experimental 2D Signal Machine simulator of MaSiNes v1.0.

## References

A selection of scientific references on the model:
- https://hal.inria.fr/tel-00548817
- https://hal.inria.fr/hal-00504876v1
- https://tel.archives-ouvertes.fr/tel-00870600
