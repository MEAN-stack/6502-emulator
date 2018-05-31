--
-- 6502 Emulator
--
module Em6502
    ( em6502Test
    ) where
--
-- Address Modes
--
data Mode = Acc
          | Abs
          | AbsX
          | AbsY
          | Imm
          | Impl
          | Ind
          | XInd
          | IndY
          | Rel
          | Zpg
          | ZpgX
          | ZpgY

byteLen :: Mode -> Int
byteLen m = case m of
               Acc  -> 0
               Abs  -> 2
               AbsX -> 2
               AbsY -> 2
               Imm  -> 1
               Impl -> 0
               Ind  -> 2
               XInd -> 1
               IndY -> 1
               Rel  -> 1
               Zpg  -> 1
               ZpgX -> 1
               ZpgY -> 1

--
-- InstructionSet
--
data Instruction = ADC Mode
                 | AND Mode
                 | ASL Mode
                 | BCC Mode
                 | BCS Mode
                 | BEQ Mode
                 | BIT Mode
                 | BMI Mode
                 | BNE Mode
                 | BPL Mode
                 | BRK Mode
                 | BVC Mode
                 | BVS Mode
                 | CLC Mode
                 | CLD Mode
                 | CLI Mode
                 | CLV Mode
                 | CMP Mode
                 | CPX Mode
                 | CPY Mode
                 | DEC Mode
                 | DEX Mode
                 | DEY Mode
                 | EOR Mode
                 | INC Mode
                 | INX Mode
                 | INY Mode
                 | JMP Mode
                 | JSR Mode
                 | LDA Mode
                 | LDX Mode
                 | LDY Mode
                 | LSR Mode
                 | NOP Mode
                 | ORA Mode
                 | PHA Mode
                 | PHP Mode
                 | PLA Mode
                 | PLP Mode
                 | ROL Mode
                 | ROR Mode
                 | RTI Mode
                 | RTS Mode
                 | SBC Mode
                 | SEC Mode
                 | SED Mode
                 | SEI Mode
                 | STA Mode
                 | STX Mode
                 | STY Mode
                 | TAX Mode
                 | TAY Mode
                 | TSX Mode
                 | TXA Mode
                 | TXS Mode
                 | TYA Mode

--
-- Association list
-- Lookup instruction/mode by machine code value
-- 
instructionSet :: [(Int, Instruction)]
instructionSet = [(0, BRK Impl), (1, ORA XInd)]

--
-- Status Register
-- stored as an 8-bit byte Carry is lsb, Negative is msb
--
data Flags = Flags { c :: Bool
                   , z :: Bool
                   , i :: Bool
                   , d :: Bool
                   , b :: Bool
                   , u :: Bool
                   , o :: Bool
                   , n :: Bool} deriving (Show)

--
-- CPU registers
--
data Regs = Regs   { a :: Char
                   , x :: Char
                   , y :: Char
                   , pc :: Int
                   , sp :: Char
                   , sr :: Flags} deriving (Show)

--
-- status register getters and setters
--
setC   f = f { c = True }
clearC f = f { c = False }
setZ   f = f { z = True }
clearZ f = f { z = False }
setI   f = f { i = True }
clearI f = f { i = False }
setD   f = f { d = True }
clearD f = f { d = False }
setB   f = f { b = True }
clearB f = f { b = False }
setO   f = f { o = True }
clearO f = f { o = False }
setN   f = f { n = True }
clearN f = f { n = False }

setA r x = r { a = x }
setX r x = r { x = x }
setY r x = r { y = x }
setPc r x = r { pc = x }
setSp r x = r { sp = x }
setSr r x = r { sr = x }

setCarry       r = setSr r (setC   (sr r)) 
clearCarry     r = setSr r (clearC (sr r))
setZero        r = setSr r (setZ   (sr r))
clearZero      r = setSr r (clearZ (sr r))
setInterrupt   r = setSr r (setI   (sr r))
clearInterrupt r = setSr r (clearI (sr r))
setDecimal     r = setSr r (setD   (sr r))
clearDecimal   r = setSr r (clearD (sr r))
setBreak       r = setSr r (setB   (sr r))
clearBreak     r = setSr r (clearB (sr r))
setOverflow    r = setSr r (setO   (sr r))
clearOverflow  r = setSr r (clearO (sr r))
setNegative    r = setSr r (setN   (sr r))
clearNegative  r = setSr r (clearN (sr r))

valAtEffectiveAddress :: [Char] -> Char
valAtEffectiveAddress _ = '\0'

--
-- ADC
-- Add Memory to Accumulator with Carry
-- A + M + C -> A, C            N Z C I D V
--                              + + + - - +
--
-- addressing    assembler    opc  bytes  cyles
-- --------------------------------------------
-- immidiate     ADC #oper     69    2     2
-- zeropage      ADC oper      65    2     3
-- zeropage,X    ADC oper,X    75    2     4
-- absolute      ADC oper      6D    3     4
-- absolute,X    ADC oper,X    7D    3     4*
-- absolute,Y    ADC oper,Y    79    3     4*
-- (indirect,X)  ADC (oper,X)  61    2     6
-- (indirect),Y  ADC (oper),Y  71    2     5*
--
adc' :: Int -> Int -> Flags -> (Int, Flags)
adc' a m f = if (c f) 
             then ((a+m+1) `mod` 256, newf)
             else ((a+m) `mod` 256, newf)
             where
                newf

adc :: Mode -> ([Char], Regs) -> ([Char], Regs)
adc Imm (mem, regs) = (newMem, newRegs) where
    newMem = tail mem
    m = head mem
    acc = a regs
    carry = c (sr regs)
   
    newRegs = regs
--    newRegs::a = regs.a + m

--
-- BRK
-- Single byte instruction
-- Address mode is always implied
brk :: ([Char], Regs) -> ([Char], Regs)
brk (x:xs, r) = (x:xs, r)

ora :: Mode -> ([Char], Regs) -> ([Char], Regs)
ora _ (x:xs, r) = (x:xs, r)


execInst :: ([Char], Regs) -> ([Char], Regs)
execInst (i:is, r) = case (lookup (fromEnum i) instructionSet) of
                         Just (BRK Impl) -> brk (is, r)
                         Just (ORA XInd) -> ora XInd (is, r)

execute :: ([Char], Regs) -> ([Char], Regs)
execute = undefined

regs = Regs {a='\0', x='\0', y='\0', pc=0, sp='\0', sr=Flags {c=False, z=False, i=False, d=False, b=False, u=False, o=False, n=False}}
mem = take 65536 $ repeat 0

--main = do
--  let memory = readFile "program.bin"
--  let regs = Regs {a='\0', x='\0', y='\0', pc='\0', sp='\0', sr=Flags {c=False, z=False, i=False, d=False, b=False, u=False, o=False, n=False}}
--  execute (memory, regs)


em6502Test :: IO ()
em6502Test = putStrLn "Testing 6502 Emulator"
