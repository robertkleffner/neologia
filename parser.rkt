#lang brag

n-file : n-config n-entry*

n-config : (n-parts | n-order | n-output)*
n-parts : /"Parts" /"=" IDENT (/"," IDENT)*
n-order : /"Order" /"=" IDENT (/"," IDENT)*
n-output : /"Output" /"=" IDENT

n-entry : /">" IDENT /":" IDENT n-definition+
@n-definition : /NUMBER STRING