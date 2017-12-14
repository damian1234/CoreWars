module Code(Instruction,Opcode,Field,bldProgram,mkMem)where

type Instruction = (Opcode,Field,Field)
    
--operation to be performs
data Opcode = MOV
            | DAT
            | ADD
            | SUB
            | JMP
            | JMZ
            | JMN
            | DJN
            | CMP
            | SPL
              deriving(Show)



operation:: String -> Opcode
operation "MOV" = MOV
operation "DAT" = DAT
operation "ADD" = ADD
operation "SUB" = SUB
operation "JMP" = JMP
operation "JMZ" = JMZ
operation "JMN" = JMN
operation "DJN" = DJN
operation "CMP" = CMP
operation "SPL" = SPL

splitInstr :: String -> [String]
splitInstr s = words s

data Control = Identity
            |Direct
            |Indirect
            |Immediate
            |AutoDecrement
            |Data
            deriving(Show)

--control field
cf:: [Char] -> Field
cf ('#':x:_) = (Immediate, (read [x]::Integer))
cf ('@':x:_) = (Indirect,(read ([x])::Integer))
cf ('<':x:_) = (AutoDecrement, (read [x]::Integer))
cf (x:_) = (Direct, (read [x]::Integer))
cf [] = (Data,0)

type Field = (Control, Integer)



bldInstruct:: [String] -> Instruction
bldInstruct (op:fs) = ( (operation op), (cf (fs!!0)), (cf (fs!!1)) )

--get rid of comma in A field
parseA :: String -> String
parseA s = init s


bldProgram :: [String] ->[Instruction]
bldProgram xs = (map bldInstruct (map splitInstr (xs)))

mkMem:: ([Instruction], Integer) -> [Instruction]
mkMem (xs, 0) = xs
mkMem (xs, len) = mkMem (newXs, newLen)
          where (newXs, newLen) = (addInstruction xs len)


addInstruction :: [Instruction] -> Integer -> ([Instruction], Integer)
addInstruction xs len = ((mappend xs [placeHolder]), len-1)

placeHolder::Instruction
placeHolder = (DAT,(Direct,0), (Direct,0))

