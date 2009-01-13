if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword nmkDefine def let1
syn match nmkDefine "_"
"syn match nmkType ","
syn keyword nmkFunction arr each pr di cons car cdr reverse load ! null? fun?
syn keyword nmkTodo   contained TODO FIXME XXX
syn match nmkComment ";.*$" contains=nmkTodo
syn region nmkComment  start="#|" end="|#"
syn keyword nmkStatement if fn end
syn keyword nmkType this nl
syn match nmkStatement "\."
syn match nmkStatement "\.\<[0-9]\+\>\."
syn match nmkNumber "\<[0-9]\+\>"
syn region nmkString  start=+'+ end=+'+ skip=+\\\\\|\\'+
syn region nmkString  start=+"+ end=+"+ skip=+\\\\\|\\"+
syn region nmkString start=+\%(\\\)\@<!#/+ skip=+\\[\\/]+ end=+/+
syn region nmkLambda matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=ALL
"syn region nmkLambda matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=ALL
syn region nmkLambda matchgroup=Statement start="\[" matchgroup=Statement end="\]" contains=ALL
syn keyword nmkBoolean true false

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
