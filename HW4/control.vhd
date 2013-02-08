---------------------------------------------------------------------
-- Control Unit
---------------------------------------------------------------------

-- Import IEEE libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;

-- Import the custom libraries which Glen gave
library work;
use work.opcodes.all; 

entity Control is

    port(
        clock   : in std_logic;                     -- the system clock;
        reset   : in std_logic;                     -- resets SP to all ones and 
                                                    -- the cycle count to 0
        SP_in   : in std_logic_vector(15 downto 0);        -- New value of SP computed 
                                                    -- by MMU.  
        Write_SP: in std_logic;                     -- Write new value of SP 
        IR_in   : in opcode_word;                   -- Instruction register.Will 
                                                    -- delete this after HW4.
        IR_out  : out opcode_word;                  -- Instruction register.Will 
                                                    -- delete this after HW4.
        ProgDB  : in std_logic_vector(15 downto 0); -- The program data bus
        SP      : out std_logic_vector(15 downto 0);-- stack pointer
        MemCnst : out std_logic_vector(15 downto 0);-- memory constant from 
                                                    -- LDS and STS instructions
        WriteReg: out std_logic;                    -- write signal for regs.
        RegInSel: out std_logic;                    -- Which input to use for 
                                                    -- the registers. 0 = ALU, 
                                                    -- 1 = Data Data Bus
        CycleCnt: buffer std_logic_vector(1 downto 0)         -- Which cycle of an 
                                                    -- instruction we are on. 
    );

end Control;

architecture behavior of Control is 
    
    -- Declare internal signals
    signal num_cycles : std_logic_vector(1 downto 0); -- Compare for clock cycle
                                                      -- counter

begin

    --
    -- Logic for figuring out how many cycles to take
    --
    num_cycles     <=   "10" when(  std_match(IR_in, OpLDS) or 
                                    std_match(IR_in, OpSTS) ) else
                        "01" when(  std_match(IR_in, OpLDX ) or
                                    std_match(IR_in, OpLDXI) or
                                    std_match(IR_in, OpLDXD) or
                                    std_match(IR_in, OpLDYI) or
                                    std_match(IR_in, OpLDYD) or
                                    std_match(IR_in, OpLDDY) or
                                    std_match(IR_in, OpLDZI) or
                                    std_match(IR_in, OpLDZD) or
                                    std_match(IR_in, OpLDDZ) or
                                    std_match(IR_in, OpSTX ) or
                                    std_match(IR_in, OpSTXI) or
                                    std_match(IR_in, OpSTXD) or
                                    std_match(IR_in, OpSTYI) or
                                    std_match(IR_in, OpSTYD) or
                                    std_match(IR_in, OpSTDY) or
                                    std_match(IR_in, OpSTZI) or
                                    std_match(IR_in, OpSTZD) or
                                    std_match(IR_in, OpSTDZ) or
                                    std_match(IR_in, OpPOP ) or
                                    std_match(IR_in, OpPUSH) or
                                    std_match(IR_in, OpADIW) or
                                    std_match(IR_in, OpSBIW) ) else
                        "00";

    --
    -- Implement the clock cycle counter
    --
    counter: process(clock)
    begin
        if ( rising_edge(clock) ) then
            if (CycleCnt = "UU" or std_match(CycleCnt, num_cycles)) then
                CycleCnt <= "00";
            else
                CycleCnt <= CycleCnt + 1;
            end if;
        end if;
    end process counter;

    --
    -- Implement the write logic
    --

    WriteReg <=  '0' when (std_match(IR_in, OpBCLR) or std_match(IR_in, OpBSET) or
                           std_match(IR_in, OpBST)  or std_match(IR_in, OpCP)   or
                           std_match(IR_in, OpCPC)  or std_match(IR_in, OpCPI)  or
                           std_match(IR_in, OpSTX)  or std_match(IR_in, OpSTXI) or
                           std_match(IR_in, OpSTXD) or std_match(IR_in, OpSTYI) or
                           std_match(IR_in, OpSTYD) or std_match(IR_in, OpSTDY) or
                           std_match(IR_in, OpSTZI) or std_match(IR_in, OpSTZD) or
                           std_match(IR_in, OpSTDZ) or std_match(IR_in, OpPOP)  or
                           std_match(IR_in, OpPUSH) or std_match(IR_in, OpSTS) ) else
                 '1';

    --
    -- Implement SP
    --
    updateSP : process(clock)
    begin
        if ( rising_edge(clock) ) then
            if (Write_SP = '1') then
                SP <= SP_in;
            elsif( reset = '0' ) then
                SP <= "1111111111111111";
            end if;
        end if;
    end process updateSP;

    --
    -- Update the memory constant
    --
    updateMemCnst: process(clock)
    begin
        if ( rising_edge(clock) ) then
            if ( std_match(CycleCnt, "01") ) then
                MemCnst <= ProgDB;
            end if;
        end if;
    end process updateMemCnst;

    --
    -- Generate the select logic for the input to the registers
    -- block
    --
    RegInSel <= '1' when(   std_match(IR_in, OpLDX ) or std_match(IR_in, OpLDXI) or
                            std_match(IR_in, OpLDXD) or std_match(IR_in, OpLDYI) or
                            std_match(IR_in, OpLDYD) or std_match(IR_in, OpLDDY) or
                            std_match(IR_in, OpLDZI) or std_match(IR_in, OpLDZD) or
                            std_match(IR_in, OpLDDZ) or std_match(IR_in, OpLDI)  or 
                            std_match(IR_in, OpLDS) ) else
                '0';

    IR_out <= IR_in;

end architecture;

