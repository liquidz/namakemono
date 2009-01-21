if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword nmkDefine def let1 ref
syn match nmkDefine "_"
syn keyword nmkFunction arr each pr di cons car cdr reverse load ! null? fun? fold and recv str-ref list->string substr or match for let map apply
syn keyword nmkTodo   contained TODO FIXME XXX
syn match nmkComment ";.*$" contains=nmkTodo
syn keyword nmkStatement if
syn keyword nmkType this nl rescue when
syn match nmkStatement "\."
syn match nmkFunction "call\/cc"
syn match nmkStatement "\.\<[0-9]\+\>\."
syn match nmkNumber oneline "[-#+0-9][-#+/0-9.]*"
syn region nmkString  start=+'+ end=+'+ skip=+\\\\\|\\'+
syn region nmkString  start=+"+ end=+"+ skip=+\\\\\|\\"+
syn region nmkString start=+\%(\\\)\@<!#/+ skip=+\\[\\/]+ end=+/+
syn region nmkLambda matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALL
"syn region nmkLambda matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALL
syn region nmkLambda matchgroup=Statement start="\[" matchgroup=Statement end="\]" contains=ALL
syn keyword nmkBoolean true false
syn region nmkComment  start="#|" end="|#"

if version >= 508 || !exists("did_namakemono_syntax_inits")
  if version < 508
    let did_namakemono_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink nmkDefine            Define
  HiLink nmkNumber            Number
  HiLink nmkString            String
  HiLink nmkStatement         Statement
  HiLink nmkFunction          Function
  HiLink nmkComment           Comment
  HiLink nmkPipe              Identifier
  HiLink nmkBoolean           Boolean
  HiLink nmkType              Type
  delcommand HiLink
endif


let b:current_syntax = "namakemono"
