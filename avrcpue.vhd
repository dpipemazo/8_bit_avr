----------------------------------------------------------------------------
--
--  Atmel AVR CPU Entity Declaration
--
--  This is entity declaration for the complete AVR CPU.  The design should
--  implement this entity to make testing possible.
--
--  Revision History:
--     11 May 98  Glen George       Initial revision.
--      9 May 00  Glen George       Updated comments.
--      7 May 02  Glen George       Updated comments.
--
----------------------------------------------------------------------------


--
--  AVR_CPU
--
--  This is the complete entity declaration for the AVR CPU.  It is used to
--  test the complete design.
--
--  Inputs:
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    INT0   - active low interrupt
--    INT1   - active low interrupt
--    clock  - the system clock
--
--  Outputs:
--    ProgAB - program memory address bus (16 bits)
--    DataAB - data memory address bus (16 bits)
--    DataWr - data write signal
--    DataRd - data read signal
--
--  Inputs/Outputs:
--    DataDB - data memory data bus (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library work;
use work.opcodes.all;
use work.alu;
use work.reg;
use work.control;
use work.memory;
use work.prog;


entity  AVR_CPU  is

    port (
        ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
        Reset   :  in     std_logic;                       -- reset signal (active low)
        INT0    :  in     std_logic;                       -- interrupt signal (active low)
        INT1    :  in     std_logic;                       -- interrupt signal (active low)
        clock   :  in     std_logic;                       -- system clock
        ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
        DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
        DataWr  :  out    std_logic;                       -- data memory write enable (active low)
        DataRd  :  out    std_logic;                       -- data memory read enable (active low)
        DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
    );

end  AVR_CPU;

architecture AvrCpuArch of AVR_CPU is

signal OperandA : std_logic_vector(7 downto 0);
signal OperandB : std_logic_vector(7 downto 0);
signal ALU_result : std_logic_vector(7 downto 0);
signal StatusReg : std_logic_vector(7 downto 0);
signal cycle_count : std_logic_vector(1 downto 0);
signal write_register : std_logic;
signal RegInSel : std_logic;
signal selXYZ : std_logic_vector(1 downto 0);
signal writeX : std_logic;
signal writeY : std_logic;
signal writeZ : std_logic;
signal writeSP: std_logic;
signal XYZ : std_logic_vector(15 downto 0);
signal IR_from_control : opcode_word;
signal StackPointer : std_logic_vector(15 downto 0);
signal writeData : std_logic;
signal newXYZ : std_logic_vector(15 downto 0);
signal PC : std_logic_vector(15 downto 0); -- program counter
signal GetNextIR : std_logic;
signal lastCycle : std_logic;
signal result_zero : std_logic;

begin

--
-- DO A LOT OF WIRING
--

    ALUUnit : entity ALU port map(
                    IR => IR_from_control, 
                    OperandA => OperandA, 
                    OperandB => OperandB, 
                    clock => clock, 
                    Result => ALU_result,
                    StatReg => StatusReg,
                    cycle_cnt => cycle_count,
                    result_zero => result_zero
                );

    REGUnit : entity REG port map(
                    IR => IR_from_control, 
                    DataDB => DataDB, 
                    ALUIn => ALU_result, 
                    clock => clock, 
                    CycleCnt => cycle_count, 
                    WriteReg => write_register, 
                    RegInSel => RegInSel,
                    selXYZ => selXYZ,
                    writeX => writeX, 
                    writeY => writeY, 
                    writeZ => writeZ, 
                    Addr => newXYZ,
                    RegAOut => OperandA, 
                    RegBOut => OperandB, 
                    XYZ => XYZ
                );

    ControlUnit : entity Control port map(
                    clock => clock, 
                    reset => Reset, 
                    SP_in => newXYZ, 
                    Write_SP => writeSP,
                    IR => IR_from_control, 
                    GetNextIR => GetNextIR,
                    ProgDB => ProgDB, 
                    SP => StackPointer, 
                    WriteReg => write_register, 
                    RegInSel => RegInSel, 
                    lastCycle => lastCycle,
                    CycleCnt => cycle_count
                );

    MemUnit : entity memory port map(
                    IR => IR_from_control, 
                    XYZ => XYZ, 
                    SP => StackPointer, 
                    RegA => OperandA, 
                    CycleCnt => cycle_count, 
                    ProgDB => ProgDB, 
                    clock => clock, 
                    PC => PC,
                    DataDB => DataDB, 
                    AddrB => DataAB, 
                    DataRd => DataRd, 
                    DataWr => writeData, 
                    selXYZ => selXYZ, 
                    writeX => writeX, 
                    writeY => writeY, 
                    writeZ => writeZ, 
                    writeSP => writeSP,
                    AdderOut => newXYZ
                );

    ProgUnit : entity prog port map(
                    IR        => IR_from_control,
                    PCIn      => newXYZ,
                    CycleCnt  => cycle_count,
                    LastCycle => lastCycle,
                    ProgDB    => ProgDB,
                    DataDB    => DataDB,
                    Registers => ALU_result,
                    Status    => StatusReg,
                    ZeroLine  => result_zero,
                    ProgAB    => ProgAB,
                    GetNextIR => GetNextIR,
                    PC        => PC
                );

    DataWr <= writeData;

end architecture;
