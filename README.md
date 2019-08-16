# Eroica

Lisp which can transpile to lua and has normal&reader macros; parser combinators are embedded in the evaluator allowing for quick parsing of DSELs at read time by reader macros.

This project is highly unstable having been the product of about two months worth of development.

I'd like to add "soundness" checking during compilation soon, which would check for nonsensical function calls or undefined symbols being used as variables. Currently this is not done since this is designed to be embedded in programs that inject many globals into the environment. There is resolution in some sort of "assume defined" directive though, which is what ought to be done.

Dedicated to the memory of a great man, Ludwig van Beethoven! A personal hero in his epitomizing of the romantic ideal and his relentless dedication to his craft.
