#lang brag

n-file : n-config n-entry*

n-config : [n-parts] [n-order]

n-parts : /"Parts" /"=" IDENT (/"," IDENT)*

n-order : /"Order" /"=" IDENT (/"," IDENT)*

n-entry : /">" IDENT /":" IDENT n-definition+

@n-definition : /NUMBER STRING