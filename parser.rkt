#lang brag

n-top : n-config n-section-defs n-entry*

n-config : (n-parts | n-order | n-file | n-path)*
n-parts : /"Parts" /"=" IDENT (/"," IDENT)*
n-order : /"Order" /"=" IDENT (/"," IDENT)*
n-path : /"Path" /"=" STRING
n-file : /"File" /"=" STRING

n-section-defs : n-section-def*
n-section-def : SECTION /"=" STRING

n-entry : /">" IDENT /":" IDENT n-definition+ n-sections
@n-definition : /NUMBER STRING
n-sections : n-section*
n-section : SECTION /"=" IDENT (/"," IDENT)*