module MIX.Instructions where

data Instruction = NOP
                 | ADD
                 | FADD
                 | SUB
                 | FSUB
                 | MUL
                 | ADD
                 | FADD
                 | SUB
                 | FSUB
                 | MUL
                 | FMUL
                 | DIV
                 | FDIV
                 | NUM
                 | CHAR
                 | HLT
                 | SLA
                 | SLAX
                 | SLC
                 | SRA
                 | SRAX
                 | SRC
                 | MOVE
                 | LDA
                 | LD1
                 | LD2
                 | LD3
                 | LD4
                 | LD5
                 | LD6
                 | LDX
                 | LDAN
                 | LD1N
                 | LD2N
                 | LD3N
                 | LD4N
                 | LD5N
                 | LD6N
                 | LDXN
                 | STA
                 | ST1
                 | ST2
                 | ST3
                 | ST4
                 | ST5
                 | ST6
                 | STX
                 | STJ
                 | STZ
                 | JBUS
                 | IOC
                 | IN
                 | OUT
                 | JRED
                 | JMP
                 | JOV
                 | JSJ
                 | JNOV
                 | J1
                 | J2
                 | J3
                 | J4
                 | J5
                 | J6
                 | JX
                 | INCA
                 | ENTA
                 | DECA
                 | ENNA
                 | INC1
                 | ENT1
                 | DEC1
                 | ENN1
                 | INC2
                 | ENT2
                 | DEC2
                 | ENN2
                 | INC3
                 | ENT3
                 | DEC3
                 | ENN3
                 | INC4
                 | ENT4
                 | DEC4
                 | ENN4
                 | INC5
                 | ENT5
                 | DEC5
                 | ENN5
                 | INC6
                 | ENT6
                 | DEC6
                 | ENN6
                 | INCX
                 | ENTX
                 | DECX
                 | ENNX
                 | CMPA
                 | FCMP
                 | CMP1
                 | CMP1
                 | CMP1
                 | CMP1
                 | CMP1
                 | CMP1
                 | CMP1
