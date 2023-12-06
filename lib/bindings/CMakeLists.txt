cmake_minimum_required(VERSION 3.25)
project(ewe C)

set(CMAKE_C_STANDARD 11)
include_directories(../../../../.opam/5.0.0/lib/ocaml)

add_library(ewe library.c)
