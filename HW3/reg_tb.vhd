----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Test Entity Declaration
--
--  This is the entity declaration which must be used for building the
--  register array portion of the AVR design for testing.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     22 Apr 02  Glen George       Updated comments.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
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

library isim_temp;
use isim_temp.opcodes.all;

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

  signal CLK          :  std_logic;

  signal IR           :  opcode_word;

  signal RegIn        :  std_logic_vector(7 downto 0);
  signal RegAOut      :  std_logic_vector(7 downto 0);
  signal RegBOut      :  std_logic_vector(7 downto 0);

  --Signal used to stop clock signal generators
  signal  END_SIM     :  BOOLEAN := FALSE;


  --
  -- Opcodes that don't write data based on RegIn Line
  --
  type dont_write_op is(
      OP_BCLR,
      OP_BLD,
      OP_BSET,
      OP_BST,
      OP_CP,
      OP_CPC
  );

  --
  -- Opcodes that Write based on the RegIn Line
  -- And don't alter Operand 1 in any abnormal way
  --
  type write_op is(
        OP_ADC,
        OP_ADD,
        OP_AND,
        OP_ASR,
        OP_COM,
        OP_DEC,
        OP_EOR,
        OP_INC,
        OP_LSR,
        OP_NEG,
        OP_OR,
        OP_ROR,
        OP_SBC,
        OP_SUB,
        OP_SWAP
    );

  --
  -- Opcodes that only operate on registers 16 to 31
  --
  type second_half_write is (
        OP_ANDI,
        OP_ORI,
        OP_SBCI,
        OP_SUBI
  );

  --
  -- Opcodes that take two clocks
  --
  type two_clocks is (
        OP_ADIW,
        OP_SPIW
  );

begin

    -- Unit Under Test port map
  UUT : REG_TEST
    port map(
      clk           =>  clock,
      IR            =>  IR,
      RegIn         =>  RegIn,
      RegAOut       =>  RegAOut,
      RegBOut       =>  RegBOut
    );

  process

  variable j : integer range 0 to 128;

  variable temp_op : std_logic_vector(15 downto 0);

  variable temp_b_reg : std_logic_vector(4 downto 0);

  variable seed1, seed2: positive;               -- Seed values for random generator
  variable randInt, oldRandInt : integer;
  variable rand: real;                           -- Random real-number value in range 0 to 1.0

  begin

  -- Check to make sure that we can write to all of our registers, read out of all of
  -- our registers on both lines A and B and make sure that CP doesn't alter any of the
  -- registers. We only test the Add and CP command over all of the possible registers

  for dontWrite in dont_write_op loop
    for doWrite in write_op loop
      for j in 0 to 31 loop

        -- Load a command that performs a write
        temp_op  := doWrite;

        -- Use Register j
        temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

        -- If we are past register 0 (and thus have loaded a value into reg j - 1)
        -- Then load register j-1 into RegBOut
        if (j > 0) then
          temp_b_reg = std_logic_vector(to_unsigned(j+1, temp_b_reg'LENGTH));
        end if;

        IR  <= temp_op;

        -- Store the old Rand Int number, since that's what's stored in reg (j - 1)
        oldRandInt := randInt;

        -- Generate a random number to store in reg (j)
        UNIFORM(seed1, seed2, rand);             -- generate random number
        randInt := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..65536, find integer part
        RegIn <= std_logic_vector(to_unsigned(randInt, RegIn'LENGTH));  -- convert to std_logic_vector

        wait for 20 ns;

        -- If we have 
        if (j > 0) then
          assert (conv_integer(RegBOut) = oldRandInt) 
          report "Non-Writing cmd altered register OR Reg B not valid"
          severity ERROR;
        end if;

        -- Check to see if we have written properly (and that we aren't writing)

        temp_op  := dontWrite;
        temp_op(8 downto 4) := std_logic_vector(to_unsigned(j, 5));

        IR <= temp_op;

        wait for 20 ns;

        assert(conv_integer(RegAOut) = randInt) 
        report "Did not store Register Properly"
        severity ERROR;
      end loop;
    end loop;
  end loop;
  

  -- Now look at the registers that only operate on the second half of registers
  for doWrite in second_half_write loop
    for j in 0 to 15 loop

      -- Load a command that performs a write
      temp_op  := doWrite;

      -- Use Register j
      temp_op(7 downto 4) := std_logic_vector(to_unsigned(j, 4));

      -- If we are past register 0 (and thus have loaded a value into reg j - 1)
      -- Then load register j-1 into RegBOut
      if (j > 0) then
        temp_b_reg = std_logic_vector(to_unsigned(j+1, temp_b_reg'LENGTH));
      end if;

      IR  <= temp_op;

      -- Store the old Rand Int number, since that's what's stored in reg (j - 1)
      oldRandInt := randInt;

      -- Generate a random number to store in reg (j)
      UNIFORM(seed1, seed2, rand);             -- generate random number
      randInt := INTEGER(TRUNC(rand*256.0));                -- rescale to 0..65536, find integer part
      RegIn <= std_logic_vector(to_unsigned(randInt, RegIn'LENGTH));  -- convert to std_logic_vector

      wait for 20 ns;

      -- If we have 
      if (j > 0) then
        assert (conv_integer(RegBOut) = oldRandInt) 
        report "CPI altered register OR Reg B not valid"
        severity ERROR;
      end if;

      -- Check to see if we have written properly (and that we aren't writing)

      temp_op  := OP_CPI;
      temp_op(7 downto 4) := std_logic_vector(to_unsigned(j, 4));

      IR <= temp_op;

      wait for 20 ns;

      assert(conv_integer(RegAOut) = randInt) 
      report "Did not store Register Properly with second half reg commands"
      severity ERROR;
    end loop;
  end loop;

  -- Test ADIW and SBIW commands

  -- First we will load registers 24-31 with their own register number
  -- So that we can test these instructions more easily

  for j in 24 to 31 loop

    -- Load a command that performs a write
    temp_op  := OP_ADD;

    -- Use Register j
    temp_op(7 downto 4) := std_logic_vector(to_unsigned(j, 4));

    IR  <= temp_op;

    RegIn <= std_logic_vector(to_unsigned(j, RegIn'LENGTH));  -- convert to std_logic_vector

    wait for 20 ns;

  end loop;

  for longOp in two_clocks loop
    for j in 0 to 3 loop

      temp_op := longOp;

      temp_op(5 downto 4) := std_logic_vector(to_unsigned(j, 2));

      IR <= temp_op;

      RegIn <= std_logic_vector(to_unsigned(24 + j*2, RegIn'LENGTH));

      wait for 20 ns;

      assert(conv_integer(RegAOut) = 24 + j*2);

      wait for 20 ns;

      assert(conv_integer(RegAOut) = 24 + j*2 + 1);

    end loop;
  end loop;

  END_SIM <= TRUE;
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