" Vim syntax file
" Maintainer:       samtenka
" Latest Revision:  2022-06

if exists("b:current_syntax")
  finish
endif

"----------------------------------------------------------------------------/
"   Comments                 
"----------------------------------------------------------------------------/

"   SAM: haskell style comments
syntax match mooComment "--.*"
hi mooComment ctermfg=Brown

"----------------------------------------------------------------------------/
"   Operators                
"----------------------------------------------------------------------------/

"   SAM: outer and quote operators (noise; de-emphasize)
syntax match mooOr "="
syntax match mooOr "|"
syntax match mooOr "\""
syntax match mooOr "\"\""
hi mooOr ctermfg=DarkGray

"   SAM: unary operators (potent; emphasize)
syntax match mooFilled "\*"
syntax match mooFilled "\!"
syntax match mooFilled "?"
hi mooFilled ctermfg=Cyan

"   SAM: parens
syntax match mooParen "("
syntax match mooParen ")"
hi mooParen ctermfg=DarkCyan

"   SAM: parens
syntax match mooOutput "\["
syntax match mooOutput "]"
syntax match mooOutput "\[]"
hi mooOutput ctermfg=DarkGray  

"   SAM: special non-terminal nodes
syntax match mooSpecial "QUOTE"
syntax match mooSpecial "BCKSL"
syntax match mooSpecial "NEWLN"
syntax match mooSpecial "SPACE"
hi mooSpecial ctermfg=Magenta

"   SAM: non-terminal nodes
syntax match mooGrammarSymbol "[A-Z][_A-Z0-9]\+\([_A-Z0-9]\)\@!"
syntax match mooGrammarSymbol "[A-Z][_A-Z0-9]\+$"
hi mooGrammarSymbol ctermfg=Magenta

"----------------------------------------------------------------------------/
"   Strings                  
"----------------------------------------------------------------------------/

"   SAM: match literal with no whitespace or escapes in them
syntax match cmooMatch "\(\[\)\@<=[^" ]*\(]\)\@="
hi cmooMatch ctermfg=DarkGreen


"   SAM: string literals with no whitespace or escapes in them
syntax match cmooStr "\(\"\)\@<=[^" ]*\(\"\)\@="
hi cmooStr ctermfg=LightGray

