----------------------------------------------------------------------------
--
--  Atmel AVR Program Unit Definition
--
--  The program unit is responsible for the following things:
--  1. Maintain and supply the program counter
--  2. Latch memory constants from the Program Data Bus for 
--      the rest of the system to use
--  3. Tell the Control unit to fetch the next instruction from
--      the program bus if a branch or skip wants to terminate
--      early.
--  4. Supply the program address bus. 
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
        DataDB    :  in  std_logic_vector(7 downto 0);  -- Data Data Bus
        Registers :  in  std_logic_vector(7 downto 0);  -- The Register Unit
        Status    :  in  std_logic_vector(7 downto 0);
        ZeroLine  :  in  std_logic;
        clock     :  in  std_logic;
        reset     :  in  std_logic;
        ProgAB    :  out std_logic_vector(15 downto 0); -- Program Address Bus (PC)
        -- ProgToWr  :  out std_logic_vector(7 downto 0);  -- Data to write to Memory
        GetNextIR :  buffer std_logic;                     -- Signal to get next IR
        PC        :  buffer std_logic_vector(15 downto 0); -- PC for other entities to use
        constantPC:  buffer std_logic_vector(15 downto 0)
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

    -- signal  bitMask         :  std_logic_vector(15 downto 0);
	 
    signal  bitLookedAt     :  std_logic_vector(7 downto 0);

    -- Internal IR, can be either IR, or what the next IR is for skip instructions
    signal  internalIR      :  std_logic_vector(15 downto 0); 

begin

    -- Update PC on clock edges.
    process (clock)
    begin

        if (rising_edge(clock) )  then

            PC <= PCinternal;

            if CycleCnt = "00" then
                constantPC <= ProgDB;
            end if;

        end if;

    end process;

    --Convenience Signals (booleans)

    -- An instruction which has a constant value in program memory associated with
    --  it.
    intrHas2Wrds <= std_match(internalIR, OpCALL) or std_match(internalIR, OpJMP) or
                    std_match(internalIR, OpLDS)  or std_match(internalIR, OpSTS);

    -- An instruction which is a return
    RetCmd       <= std_match(IR, OpRet) or std_match(IR, OpRETI);

    -- An instruction which is a skip
    SkipCmd      <= std_match(IR, OpCPSE) or std_match(IR, OpSBRC) or
                    std_match(IR, OpSBRS);

    -- If the bit which is considered at in a branch or skip instruction is
    --  equal to 1
    isBitSet     <= bitLookedAt(to_integer(unsigned(IR(2 downto 0)))) = '1';


    -- If doing a BRBC or BRBS, use the status register to get the data for checking
    --  to see if the bit is set, otherwise use the data from the registers unit
    bitLookedAt  <= Status when (std_match(IR, OpBRBC) or std_match(IR, OpBRBS)) else
                    Registers;


    -- If there is a skip, then need to remember the next instruction
    internalIR <= constantPC when (SkipCmd) else
                  IR;


    -- Branches and skips are defined to take their maximum number of clocks. 
    --  this signal is generated to end a branch or skip early in accordance
    --  with the logic of the skip.
    GetNextIR  <= '1' when ((std_match(IR, OpBRBS) and not isBitSet) or
                            (std_match(IR, OpBRBC) and     isBitSet) or
                            (std_match(IR, OpSBRS) and not isBitSet) or
                            (std_match(IR, OpSBRC) and     isBitSet) or
                            (std_match(IR, OpCPSE) and ZeroLine = '0') or
                            (SkipCmd and (not intrHas2Wrds) and CycleCnt = "01")) else
                  '0';


    -- PC = PC + 1
    PCaddone   <= std_logic_vector(unsigned(PC) + 1);

    -- If we have an instruction which needs the second instruction word
    UseConstPC <= '1' when ((std_match(IR, OpJMP))   or -- and CycleCnt = "01") or 
                            (std_match(IR, OpCALL)  and CycleCnt = "10")) else
                  '0';

    -- If we need to use the active value on the PC line
    ToUsePCIn  <= '1' when ((std_match(IR, OpRJMP)) or -- and CycleCnt = "00") or
                            (std_match(IR, OpIJMP)) or -- and CycleCnt = "00") or
                            (std_match(IR, OpRCALL) and CycleCnt = "01") or
                            (std_match(IR, OpICALL) and CycleCnt = "01") or
                            (std_match(IR, OpBRBC)  or std_match(IR, OpBRBS))) else
                  '0';

    -- Update PC with some complicated logic. 
    PCinternal <= (others => '0')          when (reset = '0') else
                  PC(15 downto 8) & DataDB when (CycleCnt = "01" and RetCmd) else
                  DataDB & PC(7 downto 0)  when (CycleCnt = "10" and RetCmd) else
                  PCaddone   when (LastCycle = '1' or SkipCmd or GetNextIR = '1' or
                                   (CycleCnt = "00" and intrHas2Wrds)) else
                  constantPC when (UseConstPC = '1') else
                  PCIn       when (ToUsePCIn = '1') else
                  PC;

    -- Put PC asynchronously on the address bus. 
    ProgAB     <= PC;

end regBehavior;