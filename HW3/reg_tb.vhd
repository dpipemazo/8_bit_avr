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
entity REG_tb is
end REG_tb;

architecture TB_REG_ARCH of REG_tb is

  -- Component declaration of the tested unit
  component  REG_TEST

      port(
          IR       :  in  opcode_word;                        -- Instruction Register
          RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
          clock    :  in  std_logic;                          -- system clock
          RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
          RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
      );

  end component;

  -- Internal Clock Signal
  signal CLK          :  std_logic;

  -- Instruction Register
  signal IR           :  opcode_word;

  -- Register signal lines to the Register Array
  signal RegIn        :  std_logic_vector(7 downto 0);
  signal RegAOut      :  std_logic_vector(7 downto 0);
  signal RegBOut      :  std_logic_vector(7 downto 0);

  --Signal used to stop clock signal generators
  signal  END_SIM     :  BOOLEAN := FALSE;


  --
  -- Opcodes that don't write data based on RegIn Line
  --

  constant dontWriteOpSize : integer := 2;

  type DONT_WRITE_OP is array (0 to dontWriteOpSize) of std_logic_vector(15 downto 0);

  constant dontWriteOp : DONT_WRITE_OP := (
      OpBST,    -- Don't write Operand 2
      OpCP,
      OpCPC
    );

  --
  -- Opcodes that Write based on the RegIn Line
  -- All of them output Operand 1, but only after index 8 do they output Operand 2
  -- And don't alter Operand 1 in any abnormal way
  --

  constant writeOpSize : integer := 15; --6;

  type WRITE_OP is array (0 to writeOpSize) of std_logic_vector(15 downto 0);

  constant writeOp : WRITE_OP := (
        OpASR,  -- Don't write Operand 2
        OpBLD,  -- Don't write Operand 2
        OpCOM,  -- Don't write Operand 2
        OpDEC,  -- Don't write Operand 2
        OpINC,  -- Don't write Operand 2
        OpLSR,  -- Don't write Operand 2
        OpNEG,  -- Don't write Operand 2
        OpROR,  -- Don't write Operand 2
        OpSWAP, -- Don't write Operand 2
        OpADC,
        OpADD,
        OpAND,
        OpEOR, 
        OpOR,  
        OpSBC, 
        OpSUB
    );
  
  --
  -- Opcodes that only operate on registers 16 to 31
  --

  constant secondHalfWriteSize : natural := 3;

  type SECOND_HALF_WRITE_OP is array (0 to secondHalfWriteSize) of std_logic_vector(15 downto 0);

  constant secondHalfWrite : SECOND_HALF_WRITE_OP := (
        OpANDI,
        OpORI,
        OpSBCI,
        OpSUBI
  );

  --
  -- Opcodes that take two clocks
  --
  
  constant twoClocksSize : natural := 1;

  type TWO_CLOCKS is array (0 to twoClocksSize) of std_logic_vector(15 downto 0);

  constant twoClocks : TWO_CLOCKS := (
        OpADIW,
        OpSBIW
  );

begin

  -- Unit Under Test port map
  UUT : REG_TEST
    port map(
      clock         =>  clk,
      IR            =>  IR,
      RegIn         =>  RegIn,
      RegAOut       =>  RegAOut,
      RegBOut       =>  RegBOut
    );

  process

  -- Index used to determine what register we are looking at in our loops
  variable j : integer range 0 to 31;

  -- Variable that temporarily stores an op-code that is then transfered to the IR
  variable temp_op : std_logic_vector(15 downto 0);

  variable temp_b_reg : std_logic_vector(4 downto 0);

  variable seed1, seed2: positive;               -- Seed values for random generator
  variable randInt, oldRandInt, randInt2 : integer;
  variable rand: real;                           -- Random real-number value in range 0 to 1.0

  variable a, b : integer;

  begin

  wait for 11 ns;

  -- Check to make sure that we can write to all of our registers, read out of all of
  -- our registers on both lines A and B and make sure that CP doesn't alter any of the
  -- registers. We only test the Add and CP command over all of the possible registers

  for a in 0 to dontWriteOpSize loop
    for b in 0 to writeOpSize loop
      for j in 0 to 31 loop

        -- Load a command that performs a write
        temp_op  := writeOp(b);

        -- Use Register j
        temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

        -- If we are past register 0 (and thus have loaded a value into reg j - 1)
        -- Then load register j-1 into RegBOut
        if (j > 0 and a > 8 and b > 0) then
          temp_b_reg := std_logic_vector(to_unsigned(j-1, temp_b_reg'LENGTH));
          temp_op(9) := temp_b_reg(4);
          temp_op(3 downto 0) := temp_b_reg(3 downto 0);
        end if;

        IR  <= temp_op;

        -- Store the old Rand Int number, since that's what's stored in reg (j - 1)
        oldRandInt := randInt;

        -- Generate a random number to store in reg (j)
        UNIFORM(seed1, seed2, rand);             -- generate random number
        randInt := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..65536, find integer part
        RegIn <= std_logic_vector(to_unsigned(randInt, RegIn'LENGTH));  -- convert to std_logic_vector

        wait for 18 ns;

        -- If we have 
        if (j > 0 and a > 8 and b > 0) then
          assert (to_integer(unsigned(RegBOut)) = oldRandInt) 
          report "Non-Writing cmd altered register OR Reg B not valid"
          severity ERROR;
        end if;

        wait for 2 ns;

        -- Check to see if we have written properly (and that we aren't writing)

        temp_op  := dontWriteOp(a);
        temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

        -- If we are past register 0 (and thus have loaded a value into reg j - 1)
        -- Then load register j-1 into RegBOut
        if (j > 0) then
          temp_b_reg := std_logic_vector(to_unsigned(j-1, temp_b_reg'LENGTH));
          temp_op(9) := temp_b_reg(4);
          temp_op(3 downto 0) := temp_b_reg(3 downto 0);
        end if;

        IR <= temp_op;

        -- Generate a random number to store in reg (j)
        UNIFORM(seed1, seed2, rand);             -- generate random number
        randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..65536, find integer part
        RegIn <= std_logic_vector(to_unsigned(randInt2, RegIn'LENGTH));  -- convert to std_logic_vector


        wait for 18 ns;

        assert(to_integer(unsigned(RegAOut)) = randInt) 
        report "Did not store Register Properly"
        severity ERROR;

        wait for 2 ns;

      end loop;
    end loop;
  end loop;
  

  -- Now look at the registers that only operate on the second half of registers
  for a in 0 to secondHalfWriteSize loop
    for j in 0 to 15 loop

      -- Load a command that performs a write
      temp_op  := secondHalfWrite(a);

      -- Use Register j
      temp_op(7 downto 4) := std_logic_vector(to_unsigned(j, 4));

      -- If we are past register 0 (and thus have loaded a value into reg j - 1)
      -- Then load register j-1 into RegBOut
      if (j > 0) then
        temp_b_reg := std_logic_vector(to_unsigned(j-1, temp_b_reg'LENGTH));
        temp_op(9) := '1';
        temp_op(3 downto 0) := temp_b_reg(3 downto 0);
      end if;

      IR  <= temp_op;

      -- Store the old Rand Int number, since that's what's stored in reg (j - 1)
      oldRandInt := randInt;

      -- Generate a random number to store in reg (j)
      UNIFORM(seed1, seed2, rand);             -- generate random number
      randInt := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..65536, find integer part
      RegIn <= std_logic_vector(to_unsigned(randInt, RegIn'LENGTH));  -- convert to std_logic_vector

      wait for 18 ns;

      -- If we have 
      if (j > 0) then
        assert (to_integer(unsigned(RegBOut)) = oldRandInt) 
        report "CPI altered register OR Reg B not valid"
        severity ERROR;
      end if;

      wait for 2 ns;

      -- Check to see if we have written properly (and that we aren't writing)
      -- OpCPI is the only Register that operates on only the second half of all registers
      -- and doesn't write, so lets use that.
      temp_op  := OpCPI;
      temp_op(7 downto 4) := std_logic_vector(to_unsigned(j, 4));

      IR <= temp_op;

      -- Generate a random number to store in reg (j)
      UNIFORM(seed1, seed2, rand);             -- generate random number
      randInt2 := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..65536, find integer part
      RegIn <= std_logic_vector(to_unsigned(randInt2, RegIn'LENGTH));  -- convert to std_logic_vector


      wait for 18 ns;

      assert(to_integer(unsigned(RegAOut)) = randInt) 
      report "Did not store Register Properly with second half reg commands"
      severity ERROR;

      wait for 2 ns;

    end loop;
  end loop;

  -- Test ADIW and SBIW commands

  for a in 0 to twoClocksSize loop

    -- First we will load registers 24-31 with their own register number
    -- So that we can test these instructions more easily

    for j in 24 to 31 loop

      -- Load a command that performs a write
      temp_op  := OpADD;

      -- Use Register j
      temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

      IR    <= temp_op;

      RegIn <= std_logic_vector(to_unsigned(j, RegIn'LENGTH));  -- convert to std_logic_vector

      wait for 20 ns;

      RegIn <= "00000000";
      IR    <= OpBCLR;

      wait for 20 ns;

      RegIn <= "00000000";
      IR    <= OpBSET;

      wait for 20 ns;

      temp_op  := OpCP;
      -- Use Register j
      temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

      IR    <= temp_op;

      wait for 18 ns;

      assert(to_integer(unsigned(RegAOut)) = j) 
      report "OpBCLR/OpBSET overwrote value (or ADD/CP are broken)"
      severity ERROR;

      wait for 2 ns;

    end loop;

    for j in 0 to 3 loop

      temp_op := twoClocks(a);

      temp_op(5 downto 4) := std_logic_vector(to_unsigned(j, 2));

      IR <= temp_op;

      RegIn <= std_logic_vector(to_unsigned(24 + j*2, RegIn'LENGTH));

      wait for 18 ns;

      assert(to_integer(unsigned(RegAOut)) = 24 + j*2)
      report "First clock on ADIW/SBIW out wrong"
      severity ERROR;

      wait for 20 ns;

      assert(to_integer(unsigned(RegAOut)) = 24 + j*2 + 1)
      report "Second clock on ADIW/SBIW out wrong"
      severity ERROR;

      wait for 2 ns;

    end loop;
  end loop;

  END_SIM <= TRUE;
  wait;
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

end architecture ; -- TB_REG_ARCH