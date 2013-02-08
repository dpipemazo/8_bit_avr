---------------------------------------------------------------------
-- VHDL Control Unit for Atmel AVR Processor
--
--  This unit currently takes in the following signals:
--      1. The system clock
--      2. An active-low reset signal
--      3. The instruction register
--      4. The program data bus
--      5. A bus to write to the stack pointer
--      6. A signal to write to the stack pointer
--
--  This unit then outputs the following signals:
--      1. The instruction register
--      2. The stack pointer
--      3. A constant address off of the program data bus 
--          necessary for LDS and STS instructions (m)
--      4. A control write signal to the registers unit telling 
--          it to write the input value or not.
--      5. A mux select signal to select the input to the registers
--          array from either the ALU output or the data data bus
--      6. An instruction cycle counter which tells which instruction
--          cycle is currently being executed. 
--
--
--  This unit has the following responsibilities associated with the 
--      above signals:
--
-----------------------
-- INSTRUCTION REGISTER
-----------------------
--      Currently, the control unit simply take in a bus which represents
--          the instruction register, and forwards it through to the system.
--      In the fufure, this unit will not take this bus as an input, but only
--          the program data bus. The control unit will then be responsible
--          for latching the value of the IR from the program data bus, 
--          storing it in a register, and then forwarding it to the 
--          rest of the system. 
--
------------------------
-- CYCLE COUNTER
------------------------
--      The control unit is responsible for dictating the timing of the system. 
--          It looks at the current instruction, determines how many cycles
--          the instruction should run for, and counts the cycles as they pass. 
--          This count is then sent to the rest of the system. The count is 
--          synchronous, so during the entier 1st cycle, the count is "00", 
--          then during the entire second cycle the count is "01", etc. The
--          count is currently only 2 bits, since the longest instructions
--          currently implemented are only 3 clock cycles. 
--
------------------------
-- REGISTER CONTROLS
------------------------
--      The control unit is also responsible for taking care of sending 
--          control signals to the register unit. It is responsible
--          for telling the register unit when to write the input data to 
--          the register, and also for telling the register unit which 
--          input data line to use: the system data bus or the output of the
--          ALU. In the future, the output of the ALU may be added to the 
--          system data bus, which would eliminate the need for this 
--          logic. 
--
-------------------------
-- STACK POINTER
-------------------------
--      The instruction register is responsible for storing and updating the
--          stack pointer. It takes a write signal from the memory access
--          unit which tells when to update the stack pointer. It also takes
--          a bus with new data for the dtack pointer from the memory access
--          unit. This is so that the adder in the MAU can be repurposed for 
--          incrementing/decrementing the stack pointer. 
--
---
----
-----
------
------- REVISION HISTORY
-----
----    2/7/13  Dan Pipe-Mazo   Initial Revision
---
--
--
---------------------------------------------------------------------

-- Import IEEE libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Import the custom libraries which Glen gave
library work;
use work.opcodes.all; 

--
-- DECLARE THE ENTITY
--

entity Control is

    port(
        clock   : in std_logic;                     -- the system clock;
        reset   : in std_logic;                     -- resets SP to all ones and 
                                                    -- the cycle count to 0
        SP_in   : in std_logic_vector(15 downto 0); -- New value of SP computed 
                                                    -- by MMU.  
        Write_SP: in std_logic;                     -- Write new value of SP 
        IR_in   : in opcode_word;                   -- Instruction register.Will 
                                                    -- delete this after HW4.
        IR_out  : out opcode_word;                  -- Instruction register.Will 
                                                    -- delete this after HW4.
        ProgDB  : in std_logic_vector(15 downto 0); -- The program data bus
        SP      : out std_logic_vector(15 downto 0);-- stack pointer
        WriteReg: out std_logic;                    -- write signal for regs.
        RegInSel: out std_logic;                    -- Which input to use for 
                                                    -- the registers. 0 = ALU, 
                                                    -- 1 = Data Data Bus
        CycleCnt: buffer std_logic_vector(1 downto 0) -- Which cycle of an 
                                                    -- instruction we are on. 
    );

end Control;

--
-- DECLARE THE ARCHITECTURE
--

architecture behavior of Control is 
    
    -- Declare internal signals
    signal num_cycles : std_logic_vector(1 downto 0); -- Compare for clock cycle
                                                      -- counter

begin

    --
    -- Logic for figuring out how many cycles to take. Fill in this table
    --  as mroe instrctions are implemented. 
    --
                        -- 3 clock instructions
    num_cycles     <=   "10" when(  std_match(IR_in, OpLDS) or 
                                    std_match(IR_in, OpSTS) ) else
                        -- 2 clock instructions
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
                        -- 1 clock instructions
                        "00";

    --
    -- Implement the clock cycle counter
    --
    counter: process(clock)
    begin
        if ( rising_edge(clock) ) then
            -- Need the check for UU to handle simulation startup issues. 
            --      shouldn't make a difference in imeplementation.
            if (CycleCnt = "UU" or std_match(CycleCnt, num_cycles)) then
                -- If we reached the end of a cycle, reset the counter
                CycleCnt <= "00";
            else
                -- Else, increment the counter
                CycleCnt <= std_logic_vector(unsigned(CycleCnt) + 1);
            end if;
        end if;
    end process counter;

    --
    -- Implement the write logic
    --

                -- Operations not to write to the register array which do not 
                --  require writing the output to the register array
    WriteReg <=  '0' when (std_match(IR_in, OpBCLR) or std_match(IR_in, OpBSET) or
                           std_match(IR_in, OpBST)  or std_match(IR_in, OpCP)   or
                           std_match(IR_in, OpCPC)  or std_match(IR_in, OpCPI)  or
                           std_match(IR_in, OpSTX)  or std_match(IR_in, OpSTXI) or
                           std_match(IR_in, OpSTXD) or std_match(IR_in, OpSTYI) or
                           std_match(IR_in, OpSTYD) or std_match(IR_in, OpSTDY) or
                           std_match(IR_in, OpSTZI) or std_match(IR_in, OpSTZD) or
                           std_match(IR_in, OpSTDZ) or std_match(IR_in, OpPOP)  or
                           std_match(IR_in, OpPUSH) or std_match(IR_in, OpSTS) ) else
                -- If not one of the above operations, write to the register
                --  array.
                 '1';

    --
    -- Implement SP
    --
    updateSP : process(clock)
    begin
        if ( rising_edge(clock) ) then
            -- If we get a write signal, then update SP.
            if (Write_SP = '1') then
                SP <= SP_in;
            -- If we get an active low reset, then reset SP to all ones. 
            elsif( reset = '0' ) then
                SP <= "1111111111111111";
            end if;
        end if;
    end process updateSP;

    --
    -- Generate the select logic for the input to the registers
    -- block
    --
                -- Use the data bus as input to the register arrays on the 
                --  instructions below. 
    RegInSel <= '1' when(   std_match(IR_in, OpLDX ) or std_match(IR_in, OpLDXI) or
                            std_match(IR_in, OpLDXD) or std_match(IR_in, OpLDYI) or
                            std_match(IR_in, OpLDYD) or std_match(IR_in, OpLDDY) or
                            std_match(IR_in, OpLDZI) or std_match(IR_in, OpLDZD) or
                            std_match(IR_in, OpLDDZ) or std_match(IR_in, OpLDS) ) else
                -- If not an instruction above, use the ALU output as input
                --  to the register arrays. 
                '0';

    -- Simply feed IR through for now. Next week, implement a register. 
    IR_out <= IR_in;

end architecture;

