---------------------------------------------------------------------
-- Control Unit
---------------------------------------------------------------------

-- Import IEEE libraries
library ieee;
use ieee.std_logic_1164.all;

-- Import the custom libraries which Glen gave
library work;
use work.opcodes.all; 

entity Control is

    port(
        clock   : in std_logic;                     -- the system clock;
        reset   : in std_logic;                     -- resets SP to all ones and 
                                                    -- the cycle count to 0
        SP_in   : in std_logic(15 downto 0);        -- New value of SP computed 
                                                    -- by MMU.  
        Write_SP: in std_logic;                     -- Write new value of SP 
        IR      : inout opcode_word;                -- Instruction register.Will 
                                                    -- delete this after HW4.
        SP      : out std_logic_vector(15 downto 0);-- stack pointer
        MemCnst : out std_logic_vector(15 downto 0);-- memory constant from 
                                                    -- LDS and STS instructions
        WriteReg: out std_logic;                    -- write signal for regs.
        RegInSel: out std_logic;                    -- Which input to use for 
                                                    -- the registers. 0 = ALU, 
                                                    -- 1 = Data Data Bus
        CycleCnt: out std_logic(1 downto 0)         -- Which cycle of an 
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
    num_cycles     <=   "10" when(  std_match(IR, OpLDS) or 
                                    std_match(IR, OpSTS) ) else
                        "01" when(  std_match(IR, OpLDX ) or
                                    std_match(IR, OpLDXI) or
                                    std_match(IR, OpLDXD) or
                                    std_match(IR, OpLDYI) or
                                    std_match(IR, OpLDYD) or
                                    std_match(IR, OpLDDY) or
                                    std_match(IR, OpLDZI) or
                                    std_match(IR, OpLDZD) or
                                    std_match(IR, OpLDDZ) or
                                    std_match(IR, OpSTX ) or
                                    std_match(IR, OpSTXI) or
                                    std_match(IR, OpSTXD) or
                                    std_match(IR, OpSTYI) or
                                    std_match(IR, OpSTYD) or
                                    std_match(IR, OpSTDY) or
                                    std_match(IR, OpSTZI) or
                                    std_match(IR, OpSTZD) or
                                    std_match(IR, OpSTDZ) or
                                    std_match(IR, OpPOP ) or
                                    std_match(IR, OpPUSH) or
                                    std_match(IR, OpADIW) or
                                    std_match(IR, OpSBIW) ) else
                        "00";

    --
    -- Implement the clock cycle counter
    --
    counter: process(clock)
    begin
        if ( rising_edge(clock) ) then
            if (std_match(CycleCnt, num_cycles)) then
                CycleCnt = 0;
            else
                CycleCnt = CycleCnt + 1;
            end if;
        end if;
    end process counter;

    --
    -- Implement the write logic
    --

    WriteReg <=  '0' when (std_match(IR, OpBCLR) or std_match(IR, OpBSET) or
                           std_match(IR, OpBST)  or std_match(IR, OpCP)   or
                           std_match(IR, OpCPC)  or std_match(IR, OpCPI)  or
                           std_match(IR, OpSTX)  or std_match(IR, OpSTXI) or
                           std_match(IR, OpSTXD) or std_match(IR, OpSTYI) or
                           std_match(IR, OpSTYD) or std_match(IR, OpSTDY) or
                           std_match(IR, OpSTZI) or std_match(IR, OpSTZD) or
                           std_match(IR, OpSTDZ) or std_match(IR, OpPOP)  or
                           std_match(IR, OpPUSH) or std_match(IR, OpSTS) ) else
                 '1';

    --
    -- Implement SP
    --
    updateSP : process(clock)
    begin
        if ( rising_edge(clock) ) then
            if (Write_SP = '1') then
                SP <= SP_in;
            elsif( reset = '1' ) then
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
    RegInSel <= "1" when(   std_match(IR, OpLDX ) or std_match(IR, OpLDXI) or
                            std_match(IR, OpLDXD) or std_match(IR, OpLDYI) or
                            std_match(IR, OpLDYD) or std_match(IR, OpLDDY) or
                            std_match(IR, OpLDZI) or std_match(IR, OpLDZD) or
                            std_match(IR, OpDDZ)  or std_match(IR, OpLDI)  or 
                            std_match(IR, OpLDS) ) else
                "0";







    




