divert(`-1')
# forloop(var, from, to, stmt) - simple version
define(`forloop', `pushdef(`$1', `$2')_forloop($@)popdef(`$1')')
define(`_forloop',
       `$4`'ifelse($1, `$3', `', `define(`$1', incr($1))$0($@)')')
# repeat(howmany, what)
define(`rep',`forloop(dummy,1,$1,`$2')')
# Helpers for common repetitions
define(`r2',`rep(2,`$1')')
define(`r3',`rep(3,`$1')')
define(`r4',`rep(4,`$1')')
define(`r8',`rep(8,`$1')')
# Major triads
define(`C_maj',  `[C,E,G,]$1')
define(`Db_maj', `[_D,F,_A,]$1')
define(`D_maj',  `[D,^F,A,]$1')
define(`Eb_maj', `[_E,G,_B,]$1')
define(`E_maj',  `[E,^G,B,]$1')
define(`F_maj',  `[F,A,C]$1')
define(`Gb_maj', `[_G,_B,_D]$1')
define(`G_maj',  `[G,B,D]$1')
define(`Ab_maj', `[_A,C_E]$1')
define(`A_maj',  `[A,^CE]$1')
define(`Bb_maj', `[_B,DF]$1')
define(`B_maj',  `[B,^D^F]$1')
# Minor triads
define(`C_min',  `[C,_E,G,]$1')
define(`Db_min', `[_D,_F,_A,]$1')
define(`D_min',  `[D,F,A,]$1')
define(`Eb_min', `[_E,_G,_B,]$1')
define(`E_min',  `[E,G,B,]$1')
define(`F_min',  `[F,_A,C]$1')
define(`Gb_min', `[_G,__B,_D]$1')
define(`G_min',  `[G,_B,D]$1')
define(`Ab_min', `[_A,_C_E]$1')
define(`A_min',  `[A,CE]$1')
define(`Bb_min', `[_B,_DF]$1')
define(`B_min',  `[B,D^F]$1')
# Major sevenths
define(`C_7',    `[C,E,G,B,]$1')
define(`Db_7',   `[_D,F,_A,C]$1')
define(`D_7',    `[D,^F,A,^C]$1')
define(`Eb_7',   `[_E,G,_B,D]$1')
define(`E_7',    `[E,^G,B,^D]$1')
define(`F_7',    `[F,A,CE]$1')
define(`Gb_7',   `[_G,_B,_DF]$1')
define(`G_7',    `[G,B,D^F]$1')
define(`Ab_7',   `[_A,C_EG]$1')
define(`A_7',    `[A,^CE^G]$1')
define(`Bb_7',   `[_B,DFA]$1')
define(`B_7',    `[B,^D^F^A]$1')
divert`'dnl
