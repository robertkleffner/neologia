#lang brag

n-top : n-config n-entry*

n-config : (n-parts | n-order | n-file | n-path)*
n-parts : /"Parts" /"=" IDENT (/"," IDENT)*
n-order : /"Order" /"=" IDENT (/"," IDENT)*
n-path : /"Path" /"=" STRING
n-file : /"File" /"=" STRING

n-entry : /">" IDENT /":" IDENT n-definition+
@n-definition : /NUMBER STRING