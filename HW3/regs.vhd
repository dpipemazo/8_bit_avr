----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Definition
--
--  This is the Atmel AVR Register Array Definition
--
--  Revision History:
--     30 Jan 13  Glen George       Initial revision.
--
----------------------------------------------------------------------------

--
--  REG_TEST
--
--  This is the register array testing interface.  It just brings all the
--  important register array signals out for testing along with the
--  Instruction Register.
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.opcodes.all;

entity  REG  is

    port(
        IR        :  in  opcode_word;                   -- Instruction Register
        RegIn     :  in  std_logic_vector(7 downto 0);  -- Input register bus
        clock     :  in  std_logic;                     -- System clock
        clk_cycle :  in  std_logic;    -- The first or second clk of instruction
        RegAOut   :  out std_logic_vector(7 downto 0);  -- Register bus A out
        RegBOut   :  out std_logic_vector(7 downto 0)   -- Register bus B out
    );

end  REG;

architecture regBehavior of REG is

    type regArrType is array (0 to 31) of std_logic_vector(7 downto 0);

    signal regArr : regArrType;

    signal  registers       :  std_logic_vector(255 downto 0);
    
    signal  internalASelect :  std_logic_vector(4 downto 0);
    signal  internalBSelect :  std_logic_vector(4 downto 0);

    signal  is2Cycles       :  boolean;

    signal  write_reg       : std_logic;
	 
    signal   v0,  v1,  v2,  v3,  v4,  v5,  v6,  v7,  v8,  v9,
            v10, v11, v12, v13, v14, v15, v16, v17, v18, v19,
            v20, v21, v22, v23, v24, v25, v26, v27, v28, v29,
            v30, v31        : std_logic_vector(7 downto 0);

begin

    regArr(0)  <= v0;
    regArr(1)  <= v1;
    regArr(2)  <= v2;
    regArr(3)  <= v3;
    regArr(4)  <= v4;
    regArr(5)  <= v5;
    regArr(6)  <= v6;
    regArr(7)  <= v7;
    regArr(8)  <= v8;
    regArr(9)  <= v9;
    regArr(10) <= v10;
    regArr(11) <= v11;
    regArr(12) <= v12;
    regArr(13) <= v13;
    regArr(14) <= v14;
    regArr(15) <= v15;
    regArr(16) <= v16;
    regArr(17) <= v17;
    regArr(18) <= v18;
    regArr(19) <= v19;
    regArr(20) <= v20;
    regArr(21) <= v21;
    regArr(22) <= v22;
    regArr(23) <= v23;
    regArr(24) <= v24;
    regArr(25) <= v25;
    regArr(26) <= v26;
    regArr(27) <= v27;
    regArr(28) <= v28;
    regArr(29) <= v29;
    regArr(30) <= v30;
    regArr(31) <= v31;


    process (clock)
    begin

        if (rising_edge(clock) )  then

            -- Only write out to register A if write is high
            if (write_reg = '1')  then
                -- regArr(to_integer(unsigned(internalASelect))) <= RegIn(7 downto 0);
                -- regArr(0) <= RegIn(7 downto 0);
                registers(8 * to_integer(unsigned(internalASelect)) + 7 downto
                          8 * to_integer(unsigned(internalASelect))) 
                        <= RegIn(7 downto 0);

            end if;

            --
            -- WHEN TO WRITE RESULT
            --
            if(  std_match(IR, OpBCLR) or std_match(IR, OpBSET) or
                 std_match(IR, OpBST)  or std_match(IR, OpCP) or
                 std_match(IR, OpCPC)  or std_match(IR, OpCPI)) then

                write_reg <= '1';
            else
                write_reg <= '1';
            end if;

        end if;

    end process;

    -- Convenience signal that marks when we are processing a 2-clock command (SPIW/ADIW)
    is2Cycles <= std_match(IR, OpADIW) or std_match(IR, OpSBIW);

    -- If we work with two clock instructions, or with ANDI, ORI, SUBI, SBCI
    -- We only work with the second half of registers, and set the input high
    -- Otherwise we map the bit normally
    internalASelect(4) <= '1' when (is2Cycles or
                                    std_match(IR, OpANDI) or std_match(IR, OpORI ) or
                                    std_match(IR, OpSUBI) or std_match(IR, OpSBCI)) else
                          IR(8);

    -- If we work with two clock instructions we always use registers 24-31 
    -- Setting bit 4 high, otherwise we map the bit normally
    internalASelect(3) <= '1' when (is2Cycles) else
                           IR(7);

    -- If we are performing SPIW/ADIW we shift the input to the left by one
    internalASelect(2 downto 1) <=  IR(5 downto 4) when (is2Cycles) else
                                    IR(6 downto 5);

    -- Handle the two clock cycle instructions, if we are performing SPIW/ADIW
    -- then the low bit is clk_cycle, otherwise we map the bit normally
    internalASelect(0) <= clk_cycle when (is2Cycles) else
                          IR(4);

    -- Map op code to the B select line (always a direct mapping)
    internalBSelect(4 downto 0) <= IR(9) & IR(3 downto 0);

    -- Assigns output A to the register as determined by internalASelect
    RegAOut <= registers(8 * to_integer(unsigned(internalASelect)) + 7 downto 
                         8 * to_integer(unsigned(internalASelect)));
    -- RegAOut <= regArr(to_integer(unsigned(internalASelect)));
   
    -- Assigns output B to the register as determined by internalBSelect
    RegBOut <= registers(8 * to_integer(unsigned(internalBSelect)) + 7 downto
                         8 * to_integer(unsigned(internalBSelect)));

    -- RegBOut <= regArr(to_integer(unsigned(internalASelect)));

end regBehavior;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library work;
use work.opcodes.all;
use work.alu;
use work.reg;

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

    signal clk_cycle : std_logic;

    signal Result    : std_logic_vector(7 downto 0);  -- Trash ALU result
    signal StatReg   : std_logic_vector(7 downto 0);  -- Trash Status Reg result

    signal internalAOut : std_logic_vector(7 downto 0);
    signal internalBOut : std_logic_vector(7 downto 0);


begin

    REGTest : entity REG  port map(IR, RegIn, clock, clk_cycle, internalAOut, internalBOut);
    ALUTest : entity ALU  port map(IR, internalAOut, internalBOut, clock, Result, StatReg, clk_cycle);

    RegAOut <= internalAOut;
    RegBOut <= internalBOut;

end architecture ; -- RegTestBehavior

