---------------------------------------------------------------------
-- Data Memory Access Unit for Atmel AVR CPU
--
--  This unit take the following inputs:
--      1. The Instruction Register from the control unit
--      2. A bus containing the current value of either the X, Y or Z register
--              from the registers unit.
--      3. The stack pointer from the control unit
--      4. The output "A" register from the register unit
--      5. The cycle count from the control unit.
--      6. The system clock
--
--  This unit outputs the following signals to the system which are used for 
--     memory accesses:
--      1. The data data bus
--      2. The data address bus
--      3. The data read signal
--      4. The data write signal
--
--  This unit outputs the following signals which are forwarded directly
--    to the register and control units in order to implement simultaneous 
--    access to the X, Y, Z and SP registers:
--      1. A select signal to mux the output of the XYZ register block and
--          send the desired one to the memory access unit.
--      2. WriteX, WriteY, WriteZ and WriteSP signals which are send to their
--          respective registers, and when high indicate that a data write
--          should be performed.
--      3. A bus containing new values for X, Y, Z or SP which then in turn
--          gets written to the respective register when the register's write
--          signal goes high. 
--
--
--  This unit has the following responsibilities:
--
------------------------
-- XYZ REGISTER OUTPUT
------------------------
--      This unit must generate the select line to the mux which governs whether
--          X, Y or Z shows up on the output bus from the registers. 
--
------------------------
-- 16-BIT REGISTER WRITE SIGNALS
------------------------
--      This unit generates the write signals for the 16-bit registers X, Y, 
--          Z and SP. It was decided to put this functionality in this unit 
--          as opposed to the control unit since these registers are used 
--          mainly for memory access purposes. 
--
------------------------
-- ADDRESS BUS
------------------------
--      This unit is responsible for outputting the address bus to memory. 
--          It must multiplex the bus between the XYZ and SP buses, as well
--          as deal with constant offset or memory adress commands. This is
--          mainly done through a single adder. The adder takes the appropriate
--          bus, either XYZ or SP as input, and adds the appropriate offset 
--          which the instruction requires. The output of this adder is then
--          sent to the address bus for the final clock of every instruction. 
--          In the event of an STS or LDS instruction, rather than outputting
--          the result of the adder, the value of the ProgramDB at the rising
--          edge of the final clock of the instruction is sent to the address
--          bus. This will allow us to directly output the constant memory
--          address to the address bus. 
--
------------------------
-- DATA BUS
------------------------
---     This unit is responsible for dealing with putting values sent over from
--          the registers unit onto the data bus. It must tri-state the bus 
--          in all cases when we are not outputting to it, and then put 
--          the 'Register A' input from the registers unit onto the data bus
--          when we do a store instruction or similar. 
--
------------------------
-- READ/WRITE SIGNALS TO MAIN MEMORY
------------------------
----    This unit needs to drop the read signal low for the final half-clock of
--          any load instruction, and drop the write signal low for the final
--          half-clock of any store instruction. It does so by calculating
--          whether it is the final clock of either respective instruction on
--          a rising clock edge, latching a '0' if it is the final clock, and
--          then OR-ing this latched value with the clock to produce a 
--          half-clock low pulse on either DataRD or DataWR.   
-----
------
------- REVISION HISTORY
------
-----      7 Feb 13  Sean Keenan       Port Definitions
----       7 Feb 13  Dan Pipe-Mazo     Expanded functonality
---        7 Feb 13  DaSean PiMaKeen   Debugging!
--
---------------------------------------------------------------------


-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Include Glen's opcode definitions
library work;
use work.opcodes.all;

entity  Memory  is

    port(
        -- Inputs
        IR          : in  opcode_word;                   -- Instruction Register
        XYZ         : in  std_logic_vector(15 downto 0); -- Input from XYZ
        SP          : in  std_logic_vector(15 downto 0); -- Stack Pointer
        RegA        : in  std_logic_vector(7 downto 0);  -- Register A from regs
        CycleCnt    : in  std_logic_vector(1 downto 0);  -- Cycle for instruction we're on
        clock       : in  std_logic;
        PC          : in  std_logic_vector(15 downto 0); -- program counter
        clockedPC   : in  std_logic_vector(15 downto 0);
        -- Dealing with memory
        DataDB      : out std_logic_vector(7 downto 0);-- Memory Data Bus
        AddrB       : out std_logic_vector(15 downto 0); -- Address Bus
        DataRd      : out std_logic;                     -- read to main memory, active low
        DataWr      : buffer std_logic;                     -- write to main memory, active low
        -- Dealing with register unit
        selXYZ      : out std_logic_vector(1 downto 0);  -- Select read from X/Y/Z
        writeX      : out std_logic;                     -- write to the X register
        writeY      : out std_logic;                     -- write to the Y register
        writeZ      : out std_logic;                     -- write to the Z register
        writeSP     : out std_logic;                     -- write to the SP register
        AdderOut    : out std_logic_vector(15 downto 0)  -- Updated value of X/Y/Z/SP/PC after
                                                         -- pre/post increment
    );

end  Memory;

architecture memoryBehavior of Memory is

-- Signals for the adder
signal AdderInA : std_logic_vector(15 downto 0);
signal AdderInB : std_logic_vector(15 downto 0);
signal AdderResult : std_logic_vector(15 downto 0);
signal clockedRead : std_logic;
signal clockedWrite : std_logic;
signal latchedAddr : std_logic_vector(15 downto 0);

begin

--
-- REGISTER UNIT CONTROL
--


    -- Map the select lines of the XYZ mux in the register unit
    SelXYZ <=   IR(3) & '0' when (  std_match(IR, OpLDDY) or 
                                    std_match(IR, OpLDDZ) or 
                                    std_match(IR, OpSTDY) or 
                                    std_match(IR, OpSTDZ) ) else

                "00" when (         std_match(IR, OpIJMP) or
                                    std_match(IR, OpICALL)) else

                IR(3 downto 2);

    -- When to write the X register
    writeX <= '1' when( std_match(CycleCnt, "01") and (
                            std_match(IR, OpLDXI) or 
                            std_match(IR, OpSTXI) or 
                            std_match(IR, OpLDXD) or
                            std_match(IR, OpSTXD)) ) else
                '0';

    -- When to write the Y register
    writeY <= '1' when( std_match(CycleCnt, "01") and (
                            std_match(IR, OpLDYI) or
                            std_match(IR, OpSTYI) or 
                            std_match(IR, OpLDYD) or 
                            std_match(IR, OpSTYD)) ) else
                '0';

    -- When to write the Z register
    writeZ <= '1' when( std_match(CycleCnt, "01") and (
                            std_match(IR, OpLDZI) or
                            std_match(IR, OpSTZI) or 
                            std_match(IR, OpLDZD) or 
                            std_match(IR, OpSTZD)) ) else
                '0';

    -- When to write the SP register
    writeSP <= '1' when(  (std_match(CycleCnt, "00") and (
                            std_match(IR, OpRCALL) or
                            std_match(IR, OpICALL) or 
                            std_match(IR, OpRET) or
                            std_match(IR, OpRETI) ) ) or

                          (std_match(CycleCnt, "01") and (
                            std_match(IR, OpPUSH) or 
                            std_match(IR, OpPOP) or
                            std_match(IR, OpRET) or
                            std_match(IR, OpRETI) or
                            std_match(IR, OpCALL) ) ) or

                          (std_match(CycleCnt, "10") and (
                            std_match(IR, OpRCALL) or
                            std_match(IR, OpICALL) or
                            std_match(IR, OpCALL) ) ) ) else

                '0';

--
-- Address Bus Control
--
    
    AdderInA <= SP      when(   std_match(IR, OpPUSH) or 
                                std_match(IR, OpPOP) or 
                                std_match(IR, OpCALL) or
                                std_match(IR, OpRET) or
                                std_match(IR, OpRETI) or
                              ((std_match(CycleCnt, "00") or
                                std_match(CycleCnt, "10")) and (
                                std_match(IR, OpRCALL) or
                                std_match(IR, OpICALL)))) else
                PC      when(   std_match(IR, OpBRBC) or
                                std_match(IR, OpBRBS) or
                                std_match(IR, OpRJMP) or
                               (std_match(CycleCnt, "01") and 
                                std_match(IR, OpRCALL)
                                )) else
                XYZ;

    AdderInB <= (others => '1') when(   std_match(IR, OpLDXD) or
                                        std_match(IR, OpLDYD) or
                                        std_match(IR, OpLDZD) or
                                        std_match(IR, OpSTXD) or
                                        std_match(IR, OpSTYD) or
                                        std_match(IR, OpSTZD) or
                                        std_match(IR, OpCALL) or

                                       (std_match(CycleCnt, "01") and
                                        std_match(IR, OpPUSH)) or

                                      ((std_match(CycleCnt, "00") or
                                        std_match(CycleCnt, "10")) and (
                                        std_match(IR, OpRCALL) or
                                        std_match(IR, OpICALL) ) ) ) else

                "0000000000000001" when(std_match(IR, OpPOP) or 
                                        std_match(IR, OpRET) or
                                        std_match(IR, OpRETI) or

                                       (std_match(CycleCnt, "01") and(
                                        std_match(IR, OpLDXI) or
                                        std_match(IR, OpLDYI) or
                                        std_match(IR, OpLDZI) or
                                        std_match(IR, OpSTXI) or
                                        std_match(IR, OpSTYI) or
                                        std_match(IR, OpSTZI)))) else

                "0000000000" & IR(13) & IR(11 downto 10) & IR(2 downto 0) when(
                                        std_match(IR, OpLDDY) or 
                                        std_match(IR, OpLDDZ) or
                                        std_match(IR, OpSTDY) or
                                        std_match(IR, OpSTDZ)) else

                -- Perform a positive relative jump
                "0000" & IR(11 downto 0) when( (IR(11) = '0') and (
                                       (std_match(CycleCnt, "01") and
                                        std_match(IR, OpRCALL)) or 
													 std_match(IR, OpRJMP))) else

                -- Perform a negative relative jump
                "1111" & IR(11 downto 0) when( (IR(11) = '1') and (
                                       (std_match(CycleCnt, "01") and
                                        std_match(IR, OpRCALL)) or 
                                                     std_match(IR, OpRJMP))) else

                "000000000" & IR(9 downto 3) when(
                                        std_match(IR, OpBRBC) or
                                        std_match(IR, OpBRBS)) else
                (others => '0');

    -- Now put the sum of AdderInA and AdderInB on the address bus
    AdderResult <= std_logic_vector(unsigned(AdderInA) + unsigned(AdderInB));

    -- Process to latch the address bus so that we can repurpose the adder for 
    --  instructions, offsets and constant addressing
    Address_Latch : process(clock)
    begin

        if (rising_edge(clock)) then
            -- Use the adder for most memory access instructions
            if (std_match(CycleCnt, "00") and not (std_match(IR, OpSTS) or std_match(IR, OpLDS))) then
                    latchedAddr <= AdderResult;
            end if;

            -- Use the program data bus for STS and LDS
            if (std_match(CycleCnt, "01") and (std_match(IR, OpSTS) or std_match(IR, OpLDS))) then
                    latchedAddr <= clockedPC;
            end if;
        end if;
    end process;

    -- Asynchronously mux the latched address line and SP
    AddrB <=    SP     when (   std_match(IR, OpCALL) or
                                std_match(IR, OpRCALL) or
                                std_match(IR, OpICALL) or
                                std_match(IR, OpRET) or 
                                std_match(IR, OpRETI) ) else
                latchedAddr;



    -- Put the adder result on the AdderOut line
    AdderOut <= AdderResult;

--
-- Data Bus Control 
--

    -- Tri-state the bus when we are not writing to it.    
    DataDB  <=  RegA when( (    std_match(CycleCnt, "01") and(
                                std_match(IR, OpSTX)  or
                                std_match(IR, OpSTXI) or
                                std_match(IR, OpSTXD) or
                                std_match(IR, OpSTYI) or
                                std_match(IR, OpSTYD) or
                                std_match(IR, OpSTZI) or
                                std_match(IR, OpSTZD) or
                                std_match(IR, OpSTDY) or
                                std_match(IR, OpSTDZ) or 
                                std_match(IR, OpPUSH))) or 
                               (std_match(CycleCnt, "10") and
                                std_match(IR, OpSTS) ) ) else
                PC(15 downto 8) when( 
                               (std_match(CycleCnt, "00") and (
                                std_match(IR, OpRCALL) or 
                                std_match(IR, OpICALL))) or 

                               (std_match(CycleCnt, "01") and 
                                std_match(IR, OpCALL))) else
                PC(7 downto 0) when(
                               (std_match(CycleCnt, "01") and (
                                std_match(IR, OpRCALL) or 
                                std_match(IR, OpICALL))) or

                               (std_match(CycleCnt, "10") and 
                                std_match(IR, OpCALL))) else
                (others => 'Z');

--
-- Memory Read/Write Control
--

    clockedRead <= '0' when(  (std_match(CycleCnt, "01") and (
                                std_match(IR, OpLDX) or
                                std_match(IR, OpLDXI) or
                                std_match(IR, OpLDXD) or
                                std_match(IR, OpLDYI) or
                                std_match(IR, OpLDYD) or
                                std_match(IR, OpLDZI) or
                                std_match(IR, OpLDZD) or
                                std_match(IR, OpLDDY) or
                                std_match(IR, OpLDDZ) or
                                std_match(IR, OpPOP) or
                                std_match(IR, OpRET) or
                                std_match(IR, OpRETI))) or
                               
                               (std_match(CycleCnt, "10") and (
                                std_match(IR, OpLDS) or
                                std_match(IR, OpRET) or
                                std_match(IR, OpRETI))) ) else
                    '1';

    clockedWrite <= '0' when(  (std_match(CycleCnt, "00") and (
                                std_match(IR, OpRCALL) or
                                std_match(IR, OpICALL))) or
                               
                               (std_match(CycleCnt, "01") and (
                                std_match(IR, OpSTX) or
                                std_match(IR, OpSTXI) or
                                std_match(IR, OpSTXD) or
                                std_match(IR, OpSTYI) or
                                std_match(IR, OpSTYD) or
                                std_match(IR, OpSTZI) or
                                std_match(IR, OpSTZD) or
                                std_match(IR, OpSTDY) or
                                std_match(IR, OpSTDZ) or
                                std_match(IR, OpPUSH) or 
                                std_match(IR, OpCALL)  or
                                std_match(IR, OpRCALL) or
                                std_match(IR, OpICALL))) or
                               
                               (std_match(CycleCnt, "10") and (
                                std_match(IR, OpSTS) or
                                std_match(IR, OpCALL)))) else
                    '1';
--
-- Now assign the values of clockedRead and clockedWrite to DataRd and DataWr, 
--  OR-ing them with clock so that they only go low for the final half-clock
--  of the cycle
--
    DataRd <= clockedRead or clock;
    DataWr <= clockedWrite or clock;

end memoryBehavior;

--
-- Now declare the testable memory unit
--

-- library ieee;
-- use ieee.std_logic_1164.all;
-- use ieee.std_logic_arith.all;
-- use ieee.numeric_std.all;

-- library work;
-- use work.opcodes.all;
-- use work.alu;
-- use work.reg;
-- use work.control;
-- use work.memory;


-- entity  MEM_TEST  is

--     port (
--         IR      :  in     opcode_word;                      -- Instruction Register
--         ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
--         Reset   :  in     std_logic;                        -- system reset signal (active low)
--         clock   :  in     std_logic;                        -- system clock
--         DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
--         DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
--         DataRd  :  out    std_logic;                        -- data read (active low)
--         DataWr  :  out    std_logic                         -- data write (active low)
--     );

-- end  MEM_TEST;

-- architecture MemTestBehavior of MEM_TEST is

-- signal OperandA : std_logic_vector(7 downto 0);
-- signal OperandB : std_logic_vector(7 downto 0);
-- signal ALU_result : std_logic_vector(7 downto 0);
-- signal StatusReg : std_logic_vector(7 downto 0);
-- signal cycle_count : std_logic_vector(1 downto 0);
-- signal write_register : std_logic;
-- signal RegInSel : std_logic;
-- signal selXYZ : std_logic_vector(1 downto 0);
-- signal writeX : std_logic;
-- signal writeY : std_logic;
-- signal writeZ : std_logic;
-- signal writeSP: std_logic;
-- signal XYZ : std_logic_vector(15 downto 0);
-- signal IR_from_control : opcode_word;
-- signal StackPointer : std_logic_vector(15 downto 0);
-- signal writeData : std_logic;
-- signal newXYZ : std_logic_vector(15 downto 0);

-- signal FakeX : std_logic_vector(15 downto 0);
  
-- begin

--
-- DO A LOT OF WIRING
--

    -- ALUUnit : entity ALU port map(
    --                 IR => IR_from_control, 
    --                 OperandA => OperandA, 
    --                 OperandB => OperandB, 
    --                 clock => clock, 
    --                 Result => ALU_result,
    --                 StatReg => StatusReg,
    --                 cycle_cnt => cycle_count
    --             );

    -- REGUnit : entity REG port map(
    --                 IR => IR_from_control, 
    --                 DataDB => DataDB, 
    --                 ALUIn => ALU_result, 
    --                 clock => clock, 
    --                 CycleCnt => cycle_count, 
    --                 WriteReg => write_register, 
    --                 RegInSel => RegInSel,
    --                 selXYZ => selXYZ,
    --                 writeX => writeX, 
    --                 writeY => writeY, 
    --                 writeZ => writeZ, 
    --                 Addr => newXYZ,
    --                 RegAOut => OperandA, 
    --                 RegBOut => OperandB, 
    --                 XYZ => XYZ
    --             );

    -- ControlUnit : entity Control port map(
    --                 clock => clock, 
    --                 reset => Reset, 
    --                 SP_in => newXYZ, 
    --                 Write_SP => writeSP,
    --                 IR_in => IR,  
    --                 IR_out => IR_from_control, 
    --                 ProgDB => ProgDB, 
    --                 SP => StackPointer, 
    --                 WriteReg => write_register, 
    --                 RegInSel => RegInSel, 
    --                 CycleCnt => cycle_count
    --             );

    -- MemUnit : entity memory port map(
    --                 IR => IR_from_control, 
    --                 XYZ => XYZ, 
    --                 SP => StackPointer, 
    --                 RegA => OperandA, 
    --                 CycleCnt => cycle_count, 
    --                 ProgDB => ProgDB, 
    --                 clock => clock, 
    --                 DataDB => DataDB, 
    --                 AddrB => DataAB, 
    --                 DataRd => DataRd, 
    --                 DataWr => writeData, 
    --                 selXYZ => selXYZ, 
    --                 writeX => writeX, 
    --                 writeY => writeY, 
    --                 writeZ => writeZ, 
    --                 writeSP => writeSP,
    --                 newXYZ => newXYZ
    --             );

    -- DataWr <= writeData;

-- end architecture;