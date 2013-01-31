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
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;


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

    REG : entity REG  port map(IR, RegIn, clock, '0', '0', RegAOut, RegBOut);

begin



end architecture ; -- RegTestBehavior

entity  REG  is

    port(
        IR        :  in  opcode_word;                   -- Instruction Register
        RegIn     :  in  std_logic_vector(7 downto 0);  -- Input register bus
        clock     :  in  std_logic;                     -- System clock
        write_reg :  in  std_logic;                     -- Whether we write Reg In
        clk_cycle :  in  std_logic;                     -- The first or second clk of instruction
        RegAOut   :  out std_logic_vector(7 downto 0);  -- Register bus A out
        RegBOut   :  out std_logic_vector(7 downto 0)   -- Register bus B out
    );

end  REG;

architecture regBehavior of REG is

    signal  Registers       :  std_logic_vector(255 downto 0);
    
    signal  internalASelect :  std_logic_vector(4 downto 0);
    signal  internalBSelect :  std_logic_vector(4 downto 0);

    signal  clk_cycle_dff   :  std_logic;

begin

    process (clk)
    begin

        if (rising_edge(clk) )  then

            -- DFF the clk_cycle
            clk_cycle_dff <= clk_cycle;

            -- Only write out to register A if write is high
            if (write_reg = '1')  then
                Registers(8 * conv_integer(internalASelect) + 7 downto
                          8 * conv_integer(internalASelect)) <= RegIn(7 downto 0);
            end if;

        end if;

    end process;

    -- If we work with two clock instructions, or with ANDI, ORI, SUBI, SBCI
    -- We only work with the second half of registers, and set the input high
    internalASelect(4) <= '1' when (std_match(opcodes(7 downto 0), OpADIW) or std_match(opcodes(7 downto 0), OpSPIW) or
                                    std_match(opcodes(7 downto 0), OpANDI) or std_match(opcodes(7 downto 0), OpORI ) or
                                    std_match(opcodes(7 downto 0), OpSUBI) or std_match(opcodes(7 downto 0), OpSBCI)) else
                          opcodes(8);

    -- If we work with two clock instructions we always use registers 24-31 
    -- Where the highest 3 bits are set high
    internalASelect(3 downto 2) <= "11" when (std_match(opcodes(7 downto 0), OpADIW) or std_match(opcodes(7 downto 0), OpSPIW)) else
                                   opcodes(7 downto 0);

    -- Bit 6 on the opcode is always mapped to bit 2 on the A select
    internalASelect(1) <= opcodes(5);

    -- Handle the two clock cycle instructions, where on the second cycle
    -- we need to access the next register
    internalASelect(0) <= opcodes(4) and (not clk_cycle_dff);

    -- Map op code to the B select line (always a direct mapping)
    internalBSelect(4 downto 0) <= opcodes(9) & opcodes(3 downto 0);


    RegAOut <= registers(8 * conv_integer(internalASelect) + 7 downto 
        8 * conv_integer(internalASelect));

    RegBOut <= registers(8 * conv_integer(internalBSelect) + 7 downto
        8 * conv_integer(internalBSelect));

end regBehavior;

