----------------------------------------------------------------------------
--
--  Atmel AVR Program Unit Definition
--
--  This is the Atmel AVR Program Unit Definition.
--
--  Revision History:
--     12 Feb 13  Sean Keenan       Initial revision.
--
----------------------------------------------------------------------------

-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Include Glen's opcode definitions
library work;
use work.opcodes.all;

entity  PROG  is

    port(   
        IR        :  in  opcode_word;                   -- Instruction Register
        PCIn      :  in  std_logic_vector(15 downto 0); -- New value for PC from memory unit
        CycleCnt  :  in  std_logic_vector(1 downto 0);  -- The clk of an instruction we're on
        LastCycle :  in  std_logic;                     -- The last cycle of an instruction
        ProgDB    :  in  std_logic_vector(15 downto 0); -- Program Data Bus
        DataDB    :  in  std_logic_vector(15 downto 0); -- Data Data Bus
        Registers :  in  std_logic_vector(7 downto 0);  -- The ALU 
        Status    :  in  std_logic_vector(7 downto 0);
        ZeroLine  :  in  std_logic;
        ProgAB    :  out std_logic_vector(15 downto 0)  -- Program Address Bus (PC)
        -- ProgToWr  :  out std_logic_vector(7 downto 0);  -- Data to write to Memory
        GetNextIR :  out std_logic;                     -- Signal to get next IR
        PC        :  buffer std_logic_vector(15 downto 0) -- PC for other entities to use
    );

end  PROG;

architecture regBehavior of PROG is

                                                        -- Not always used, only on certain instructions
    signal  ToUsePCIn       :  std_logic;               -- Flag to note when to use PCIn

    signal  PCinternal      :  std_logic_vector(15 downto 0);

    signal  PCaddone        :  std_logic_vector(15 downto 0);

    signal  intrHas2Wrds    :  boolean;
    signal  RetCmd          :  boolean;
    signal  SkipCmd         :  boolean;
    signal  isBitSet        :  boolean;

    signal  UseConstPC      :  std_logic;

    signal  constantPC      :  std_logic_vector(15 downto 0);

    signal  bitMask         :  std_logic_vector(15 downto 0);

    -- Internal IR, can be either IR, or what the next IR is for skip instructions
    signal  internalIR      :  std_logic_vector(15 downto 0); 

begin

    process (clock)
    begin

        if (rising_edge(clock) )  then

            PC <= PCinternal;

            if CycleCnt = "00" then
                constantPC := ProgDB;
            end if;

        end if;

    end process;

    --Convenience Signals (booleans)

    intrHas2Wrds := std_match(internalIR, OpCALL) or std_match(internalIR, OpJMP) or
                    std_match(internalIR, OpLDS)  or std_match(internalIR, OpSTS);

    RetCmd       := std_match(IR, OpRet) or std_match(IR, OpRETI);

    SkipCmd      := std_match(IR, OpCPSE) or std_match(IR, OpSBRC) or
                    std_match(IR, OpSBRS);


    -- bitMask      := "00000001" when (std_match(IR(2 downto 0), "000")) else
    --                 "00000010" when (std_match(IR(2 downto 0), "001")) else
    --                 "00000100" when (std_match(IR(2 downto 0), "010")) else
    --                 "00001000" when (std_match(IR(2 downto 0), "011")) else
    --                 "00010000" when (std_match(IR(2 downto 0), "100")) else
    --                 "00100000" when (std_match(IR(2 downto 0), "101")) else
    --                 "01000000" when (std_match(IR(2 downto 0), "110")) else
    --                 "10000000"; --when (std_match(IR(2 downto 0), "111"))

    -- -- Is Bit on bitLookedAt set
    -- isBitSet     := '0' when (std_match(bitLookedAt and bitMask, "00000000")) else
    --                 '1';

    isBitSet     := bitLookedAt(to_integer(unsigned(IR(2 downto 0))));


    bitLookedAt  := Status when (std_match(IR, BRBC) or std_match(IR, BRBS)) else
                    Registers;


    internalIR := ProgDB when (SkipCmd) else
                  IR;


    GetNextIR  <= '1' when ((std_match(IR, BRBS) and not isBitSet) or
                            (std_match(IR, BRBC) and     isBitSet) or
                            (std_match(IR, SBRS) and not isBitSet) or
                            (std_match(IR, SBRC) and     isBitSet) or
                            (std_match(IR, CPSE) and not ZeroLine)) else
                  '0';


    PCaddone   := std_logic_vector(unsigned(PC) + '1');

    UseConstPC <= '1' when ((std_match(IR, OpJMP)   or -- and CycleCnt = "01") or 
                            (std_match(IR, OpCALL)  and CycleCnt = "10")) else
                  '0';

    ToUsePCIn  := '1' when ((std_match(IR, OpRJMP)  or -- and CycleCnt = "00") or
                            (std_match(IR, OpIJMP)) or -- and CycleCnt = "00") or
                            (std_match(IR, OpRCALL) and CycleCnt = "01") or
                            (std_match(IR, OpICALL) and CycleCnt = "01") or
                            (std_match(IR, OpBRBC)  or std_match(IR, OpBRBS))) else
                  '0';


    PCinternal := PC[15 downto 8] & DataDB when (CycleCnt = "01" and RetCmd) else
                  DataDB & PC[7 downto 0]  when (CycleCnt = "10" and RetCmd) else
                  PCaddone   when (LastCycle = '1' or SkipCmd or GetNextIR = '1' or
                                   (CycleCnt = "00" and intrHas2Wrds)) else
                  constantPC when (UseConstPC = '1') else
                  PCIn       when (ToUsePCIn = '1')
                  PC;

    ProgAB     <= PC;

end regBehavior;