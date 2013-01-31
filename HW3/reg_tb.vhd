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

  begin

  

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


