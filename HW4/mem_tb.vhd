----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Test Entity 
--
--  This is the test Entity for the Register Array. 
--
--  It first loads random values into all of the registers, using all of the 
--  commands that load registers, and then checks to make sure the value was 
--  correctly stored using all of the commands that don't alter registers, 
--  and makes sure that these commands don't alter the registers. 
--
--  It then goes on to test the special cases, where some instructions only 
--  operate on the upper half of registers, performing the same tests as
--  above
--
--  It then goes on to check that ADIW and SBIW work as intended, over two 
--  clocks. In this case it sets all of the registers to their index 
--  (ie register 5 = 0x05). While doing this it ensures that OpBCLR/OpBSET
--  do not overwrite other registers when they are run (but don't need
--  to output any valid registers on RegAOut or RegBOut)
--
--  When running the test vector a warning will appear:
--    "Warning: NUMERIC_STD.TO_INTEGER: metavalue detected, returning 0"
--  This is a byproduct of some of instructions not having valid bits ("-")
--  in spaces where the regs.vhd checks for operand B. This isn't an issue
--  since in system these signals will always have values, and infact by
--  not throwing errors on asserts when we are passing these values means
--  that our system doesn't care about these values, when it infact shouldn't
--  care about these values (which is good!)
--
--  Revision History:
--     31 Jan 13  Sean Keenan  Initial Revision
--
----------------------------------------------------------------------------


-- Include std libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;       -- Include this for random number generation 

-- Include Glen's opcode definitions
library work;
use work.opcodes.all;

-- Define Test Bench Entity
entity MEM_tb is
end MEM_tb;

architecture TB_MEM_ARCH of MEM_tb is

  -- Component declaration of the tested unit
  component  MEM_TEST

    port (
        IR      :  in     opcode_word;                      -- Instruction Register
        ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
        Reset   :  in     std_logic;                        -- system reset signal (active low)
        clock   :  in     std_logic;                        -- system clock
        DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
        DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
        DataRd  :  out    std_logic;                        -- data read (active low)
        DataWr  :  out    std_logic                         -- data write (active low)
    );

  end component;


  -- Instruction Register
  signal IR           :  opcode_word;

  -- Internal Clock Signal
  signal CLK          :  std_logic;

  -- Reset signal
  signal Reset        :  std_logic;

  -- Program Data Bus
  signal ProgDB       : std_logic_vector(15 downto 0);

  -- Data Address Bus
  signal DataAB       : std_logic_vector(15 downto 0);

  -- Data data bus
  signal DataDB       : std_logic_vector(7 downto 0);

  -- Data read signal
  signal DataRd       : std_logic;

  -- Data write signal
  signal DataWr       : std_logic;

  --Signal used to stop clock signal generators
  signal  END_SIM     :  BOOLEAN := FALSE;


  --
  -- Simple Load and Store Opcodes that only inc/dec or keep X unchanged
  --

  constant loadStoreSimpleSize : integer := 13;

  type LOAD_STR_SIMPLE_OP is array (0 to loadStoreSimpleSize) of std_logic_vector(15 downto 0);

  constant loadStoreSimple : LOAD_STR_SIMPLE_OP := (
      OpLDX,
      OpSTX,
      OpLDXI,
      OpSTXI,
      OpLDXD,
      OpSTXD,
      OpLDYI,
      OpSTYI,
      OpLDYD,
      OpSTYD,
      OpLDZI,
      OpSTZI,
      OpLDZD,
      OpSTZD
    );

  --
  -- Load/Store Opcodes that add a constant to addressed location
  --

  constant loadConstSize : integer := 3;

  type LOAD_CONST_OP is array (0 to loadConstSize) of std_logic_vector(15 downto 0);

  constant loadConst : LOAD_CONST_OP := (
      OpLDDY,
      OpSTDY,
      OpLDDZ,
      OpSTDZ
    );

  --
  -- Commands that use memory (take three clocks)
  --

  constant memoryCmdsSize : integer := 1;

  type MEM_CMD_OP is array (0 to memoryCmdsSize) of std_logic_vector(15 downto 0);

  constant memoryCmds : MEM_CMD_OP := (
      OpLDS,
      OpSTS
    );


  --
  -- Commands that store some value to registers
  --

  constant simpleRegSize : integer := 1;

  type SIMPLE_REG_OP is array (0 to simpleRegSize) of std_logic_vector(15 downto 0);

  constant simpleReg : SIMPLE_REG_OP := (
      OpMOV,
      OpLDI
    );


  --
  -- Commands that store some value to registers
  --

  constant popPushSize : integer := 1;

  type POP_PUSH_OP is array (0 to popPushSize) of std_logic_vector(15 downto 0);

  constant popPush : SIMPLE_REG_OP := (
      OpPOP,
      OpPUSH
    );


begin

  -- Unit Under Test port map
  UUT : MEM_TEST
    port map(
      IR            =>  IR,
      ProgDB        =>  ProgDB,
      Reset         =>  Reset,
      clock         =>  clk,
      DataAB        =>  DataAB,
      DataDB        =>  DataDB,
      DataRd        =>  DataRd,
      DataWr        =>  DataWr
    );

  process
    variable loopVar : integer;

  begin

    wait for 10 ns;

    for loopVar in 0 to loadStoreSimpleSize loop
      IR <= loadStoreSimple(loopVar);
      wait for 40 ns;
    end loop;

  end process;


  CLOCK_CLK : process

  begin

    -- this process generates a 20 ns period, 50% duty cycle clock

    -- only generate clock if still simulating

    if END_SIM = FALSE then
      CLK <= '0';
      wait for 10 ns;
    else
      wait;
    end if;

    if END_SIM = FALSE then
      CLK <= '1';
      wait for 10 ns;
    else
      wait;
    end if;

  end process;

end architecture ; -- TB_MEM_ARCH