----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Definition
--
--  This is the Atmel AVR Register Array Definition.
--
--  This contains 32 registers, it also reads out or writes the appropriate
--  registers based on the Instruction Register that is wired to it. It does
--  however require an additional CycleCnt line which is used for the ADIW
--  and SBIW instructions. 
--
--  The only DFF's in the system are the 32 registers, where everything else is
--  combinational logic. This means that newly written values are not valid
--  until slightly after the clock (but will be valid shortly thereafter)
--
--  For all instructions RegBOut is output based on the bits of the Instruction
--  Register whether or not those bits are indended to be interpreted as a 
--  Register address, this is done to reduce logic.
--
--  For some exceptions (ANDI, CPI, ORI, SBCI, SUBI) only the second half of
--  registers are used (in outputting RegAOut). In these cases the normal 
--  location of the High bit on the instruction Register is ignored and is
--  internally set to 1.
--  
--  For the ADIW and SBIW instructions we have 4 different register sets we can
--  use, (24/25, 26/27, 28/29, 30/31). The CycleCnt line determines the low bit
--  of the output register to use (given from the ALU) and the two input bits
--  are the 2nd lowest and 3rd lowest bits. The rest of the bits are set to 1.
--
--  In particular we introduced the CycleCnt line to our system because the
--  ALU already has to calculate what CycleCnt we are on and while it ideally
--  the Register Array would be entirely self contained in the interest of
--  minimizing logic, only the ALU performs this calculation. On top of this,
--  if performing ADIW/SBIW correctly for the second clock is not relevant
--  the CycleCnt line can be tied low.
--
--  Revision History:
--     30 Jan 13  Sean Keenan       Initial revision.
--     31 Jan 13  Sean Keenan       Massive Debugging + Tons of commenting
--      7 Feb 13  Sean Keenan       Updates to work with Memory/Control Unit
--
----------------------------------------------------------------------------

-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Include Glen's opcode definitions
library work;
use work.opcodes.all;

entity  REG  is

    port(
        IR        :  in  opcode_word;                   -- Instruction Register
        MemIn     :  in  std_logic_vector(7 downto 0);  -- Input from Memory Data Bus
        ALUIn     :  in  std_logic_vector(7 downto 0);  -- Input from ALU
        clock     :  in  std_logic;                     -- System clock
        CycleCnt  :  in  std_logic_vector(1 downto 0);  -- The clk of an instruction we're on
        WriteReg  :  in  std_logic;                     -- Write signal
        RegInSel  :  in  std_logic;                     -- 0 = ALU, 1 = Memory Data Bus
        selXYZ    :  in  std_logic_vector(1 downto 0);  -- Select read from X/Y/Z
        RegAOut   :  out std_logic_vector(7 downto 0);  -- Register bus A out
        RegBOut   :  out std_logic_vector(7 downto 0);  -- Register bus B out
        XYZAddr   :  out std_logic_vector(15 downto 0)  -- Output from XYZ
    );

end  REG;

architecture regBehavior of REG is

    -- Declare a large std_logic_vector block for all of our registers
    -- (32 registers * 8 bits each = 256 bits -> 255 downto 0)
    signal  registers       :  std_logic_vector(255 downto 0);
    
    -- Internal A/B select lines, 5 bits to select from 32 registers
    signal  internalASelect :  std_logic_vector(4 downto 0);
    signal  internalBSelect :  std_logic_vector(4 downto 0);

    -- Internal Data Write line, muxed from RegB, ALU or Memory Data Bus
    signal  internalDataWrite : std_logic_vector(7 downto 0);

    -- Convenience boolean to demark when ADIW or SBIW
    signal  isImmWord       :  boolean;

    -- Internal signal to denote when we should write on the next clock
    signal  write_reg       : std_logic;

begin

    process (clock)
    begin

        if (rising_edge(clock) )  then

            -- Only write out to register A if write is high
            -- Since write_reg is Combinational Logic it is still valid from
            -- the last Instruction Register Value, and we write to the
            -- appropriate register based on internalASelect
            if (write_reg = '1')  then

                -- Write to the appropriate lines on the register
                -- By putting this in a clock process we are effectively DFF'ing
                -- each of our registers
                registers(8 * to_integer(unsigned(internalASelect)) + 7 downto
                          8 * to_integer(unsigned(internalASelect))) 
                          <= internalDataWrite(7 downto 0);

            end if;

            -- Write to the X, Y, or Z registers as selected from enables

            -- Write to X register from Addr line
            if (enables(0) = '1')  then
                registers(8 * 27 + 7 downto 8 * 26) <= Addr(15 downto 0);
            end if;

            -- Write to Y register from Addr line
            if (enables(1) = '1')  then
                registers(8 * 29 + 7 downto 8 * 28) <= Addr(15 downto 0);
            end if;

            -- Write to Z register from Addr line
            if (enables(2) = '1')  then
                registers(8 * 31 + 7 downto 8 * 30) <= Addr(15 downto 0);
            end if;

        end if;

    end process;

    -- Internally mux the RegBOut, MemIn and ALUIn to the internal Data Line
    internalDataWrite <= RegBOut when (std_match(IR, OpMOV)) else
                         MemIn when (RegInSel = '1') else
                         ALUIn; --when (RegInSel = '0')

    -- Convenience signal that marks when we are processing a 2-clock command (SPIW/ADIW)
    isImmWord <= std_match(IR, OpADIW) or std_match(IR, OpSBIW);

    -- If we work with two clock instructions, or with ANDI, ORI, SUBI, SBCI, LDI
    -- We only work with the second half of registers, and set the input high
    -- Otherwise we map the bit normally
    internalASelect(4) <= '1' when (isImmWord or std_match(IR, OpCPI ) or 
                                    std_match(IR, OpANDI) or std_match(IR, OpORI ) or
                                    std_match(IR, OpSUBI) or std_match(IR, OpSBCI) or
                                    std_match(IR, OpLDI)) else
                          IR(8);

    -- If we work with two clock instructions we always use registers 24-31 
    -- Which requires setting bit 4 high, otherwise we map the bit normally
    internalASelect(3) <= '1' when (isImmWord) else
                           IR(7);

    -- If we are performing SPIW/ADIW we shift the input to the left by one
    -- since the two bits we get refer to the bits 2 and 1, but are in the
    -- place that bits 1 and 0 are normally. Otherwise we map the bit normally
    internalASelect(2 downto 1) <=  IR(5 downto 4) when (isImmWord) else
                                    IR(6 downto 5);

    -- Handle the two clock cycle instructions, if we are performing SPIW/ADIW
    -- then the low bit is CycleCnt, otherwise we map the bit normally
    internalASelect(0) <= CycleCnt when (isImmWord) else
                          IR(4);

    -- Map op code to the B select line (always a direct mapping)
    internalBSelect(4 downto 0) <= IR(9) & IR(3 downto 0);

    -- Assigns output A to the register as determined by internalASelect
    RegAOut <= registers(8 * to_integer(unsigned(internalASelect)) + 7 downto 
                         8 * to_integer(unsigned(internalASelect)));
   
    -- Assigns output B to the register as determined by internalBSelect
    RegBOut <= registers(8 * to_integer(unsigned(internalBSelect)) + 7 downto
                         8 * to_integer(unsigned(internalBSelect)));

    -- Select Register to output on XYZ address line based on selXYZ,
    -- We don't care about the case where selXYZ = "01", and output Z
    XYZAddr <= registers(8 * 27 + 7 downto 8 * 26) when (selXYZ = "11") else
               registers(8 * 29 + 7 downto 8 * 28) when (selXYZ = "10") else
               registers(8 * 31 + 7 downto 8 * 30); -- when (selXYZ = "00")

end regBehavior;

-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

-- Include Glen's opcode definitions, and the ALU, and REG entities
library work;
use work.opcodes.all;
use work.alu;
use work.reg;

--
--  REG_TEST
--
--  This is the register array testing interface.  It just brings all the
--  important register array signals out for testing along with the
--  Instruction Register. 
--
--  This is included mostly for compatibility with Glen's Code. In addition
--  our test vectors are written off of this entity.
--
--  The main thing that this entity does it tie the ALU's CycleCnt output to
--  the REG's entity (an input that the test-bench does not supply)
--
--  Inputs:
--    IR      - Instruction Register (16 bits)
--    RegIn   - input to the register array (8 bits)
--    clock   - the system clock
--
--  Outputs:
--    RegAOut - register bus A output (8 bits), eventually will connect to ALU
--    RegBOut - register bus B output (8 bits), eventually will connect to ALU
--

entity  REG_TEST  is

    port(
        IR        :  in  opcode_word;                   -- Instruction Register
        RegIn     :  in  std_logic_vector(7 downto 0);  -- input register bus
        clock     :  in  std_logic;                     -- system clock
        RegAOut   :  out std_logic_vector(7 downto 0);  -- register bus A out
        RegBOut   :  out std_logic_vector(7 downto 0)   -- register bus B out
    );

end  REG_TEST;

architecture RegTestBehavior of REG_TEST is

    signal CycleCnt : std_logic(1 downto 0);  -- Clock cycle we are on if ADIW or SBIW

    signal Result    : std_logic_vector(7 downto 0);  -- Trash ALU result
    signal StatReg   : std_logic_vector(7 downto 0);  -- Trash Status Reg result

    signal internalAOut : std_logic_vector(7 downto 0);
    signal internalBOut : std_logic_vector(7 downto 0);


begin

    REGTest : entity REG  port map(IR, RegIn, clock, CycleCnt, internalAOut, internalBOut);
    ALUTest : entity ALU  port map(IR, internalAOut, internalBOut, clock, Result, StatReg);

    RegAOut <= internalAOut;
    RegBOut <= internalBOut;

end architecture ; -- RegTestBehavior