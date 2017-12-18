module Code(Instruction,Opcode,Field,bldProgram,mkMem, instruct)where

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

update :: Instruction -> Int -> [Instruction] -> [Instruction]
update inst adr mem = do 
                    let (myH, myT) = splitAt adr mem
                    let myTail = tail myT
                    let newTail = mappend [inst] myTail
                    (mappend myH newTail)

instruct :: Instruction-> [Instruction] -> Int -> Bool -> ([Instruction], Int, Bool)
instruct (DAT, f1, f2) mem pc alive = (mem, (pc+1), False)
instruct (MOV, f1, f2) mem pc alive = ((update newInst dest mem), (mod (pc+1) (length mem)), alive)
        where newInst = getInstruct f1 mem pc
              dest =pc + (getAval f2 mem)
              
instruct (ADD,f1,f2) mem pc alive = do 
                            let x = (pc +(getAval f2 mem))
                            let inst1 = getInstruct f1 mem pc
                            let (op,fld1, fld2) = getInstruct f2 mem pc
                            let (ctrl, no) = fld2
                            let newfld = (ctrl, (mod ((chkAdd inst1) + no) (length mem)))
                            ((update (op,fld1,newfld) x mem),(mod (pc+1) (length mem)), alive)

instruct (SUB,f1,f2) mem pc alive= do
                            let x = (pc +(getAval f2 mem))
                            let inst1 = getInstruct f1 mem pc
                            let (op,fld1, fld2) = getInstruct f2 mem pc
                            let (ctrl, no) = fld2
                            let newfld = (ctrl, (mod (no - (chkAdd inst1)) (length mem)))
                            ((update (op,fld1,newfld) x mem),(mod (pc+1) (length mem)), alive)
instruct (JMP, f1, f2) mem pc alive = do
                            let newpc = pc +(getAval f1 mem)
                            (mem, (mod (newpc+1) (length mem)), alive)
instruct (JMZ, f1, f2) mem pc alive
                            |(getAval f2 mem) == 0 = instruct (JMP, f1, f2) mem pc alive
                            |otherwise = (mem,(mod (pc+1) (length mem)), alive)
instruct (JMN, f1, f2) mem pc alive
                            |(getAval f2 mem) < 0 = instruct (JMP, f1,f2) mem pc alive
                            |otherwise = (mem,(mod (pc+1) (length mem)), alive)
instruct (DJN, f1, f2) mem pc alive
                            |((getAval f2 mem)-1)<0 = instruct(JMP, f1, f2) mem pc alive
                            |otherwise = (mem,(mod (pc+1) (length mem)), alive)
instruct(CMP, f1, f2) mem pc alive
                            |(getAval f1 mem) == (getAval f2 mem) = (mem, (mod (pc+2) (length mem)), alive)
                            |otherwise = (mem,(mod (pc+1) (length mem)), alive)

                            
                            

--if trying to ADD, check if you can first, if not return something you can add with
chkAdd :: Instruction -> Int
chkAdd (DAT, _, (Immediate, x)) = x
chkAdd (_,_,_) = 0
                            
--returns instruction at an address, unless immediate. returns a DAT instruction
getInstruct :: Field -> [Instruction] -> Int -> Instruction
getInstruct (Direct, x) mem pc = mem!!(pc + x)
getInstruct (Indirect, x) mem pc = do 
                                  let (op,f1,f2) = mem!!(pc + x)
                                  let inst = getInstruct f2 mem pc
                                  inst
getInstruct (AutoDecrement, x) mem pc = getInstruct (Indirect, (x-1)) mem pc
getInstruct (Immediate, x) mem pc = (DAT,(Direct,0),(Immediate, x))

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

--returns mem addr of operand
getAval :: Field -> [Instruction]-> Int
getAval (Direct, i) xs = i
getAval (Indirect, i) xs = getAval (getBField (xs!!i)) xs
getAval (Immediate, i) xs = i
getAval (AutoDecrement, i) xs = i-1


-- getting second field because indirect address should point to a DAT instuction
getBField :: Instruction -> Field
getBField (op,f1,f2) = f2

--parcing control field
cfA:: [Char] -> Field
cfA ('#':xs) = (Immediate, (read (init xs)::Int))
cfA ('@':xs) = (Indirect,(read (init xs)::Int))
cfA ('<':xs) = (AutoDecrement, (read (init xs)::Int))
cfA (xs) = (Direct, (read (init xs)::Int))

cfB:: [Char] -> Field
cfB ('#':xs) = (Immediate, (read xs::Int))
cfB ('@':xs) = (Indirect, (read xs::Int))
cfB ('<':xs) = (AutoDecrement, (read xs::Int))
cfB (xs) = (Direct, (read xs::Int))


type Field = (Control, Int)



bldInstruct:: [String] -> Instruction
bldInstruct (op:fs) = ( (operation op), (cfA (fs!!0)), (cfB (fs!!1)) )

--get rid of comma in A field
parseA :: String -> String
parseA s = init s


bldProgram :: [String] ->[Instruction]
bldProgram xs = (map bldInstruct (map splitInstr (xs)))

mkMem:: ([Instruction], Int) -> [Instruction]
mkMem (xs, 0) = xs
mkMem (xs, len) = mkMem (newXs, newLen)
          where (newXs, newLen) = (addInstruction xs len)


addInstruction :: [Instruction] -> Int -> ([Instruction], Int)
addInstruction xs len = ((mappend xs [placeHolder]), len-1)

placeHolder::Instruction
placeHolder = (DAT,(Direct,0), (Direct,0))


--execute:: [Instruction] -> Int -> ([Instruction] , Int)
--execute mem pc = 
