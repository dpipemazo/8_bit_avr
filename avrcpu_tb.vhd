---------------------------------------------------------
--
--  AVR CPU TEST BENCH
--
---------------------------------------------------------

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
use work.avrcpue;

entity AVRCPU_tb is
end AVRCPU_tb;

architecture  TB_AVR_CPU  of AVRCPU_tb is

    -- Component Declaration

    component  AVR_CPU
        port(
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
    end  component;

    -- Signals
    signal ProgDB : std_logic_vector(15 downto 0);
    signal Reset  : std_logic;                    
    signal INT0   : std_logic;                    
    signal INT1   : std_logic;                    
    signal clock  : std_logic;                    
    signal ProgAB : std_logic_vector(15 downto 0);
    signal DataAB : std_logic_vector(15 downto 0);
    signal DataWr : std_logic;                    
    signal DataRd : std_logic;                    
    signal DataDB : std_logic_vector(7 downto 0);  

    -- Constants


    begin

    UUT : AVR_CPU
        port map(
            ProgDB => ProgDB,
            Reset  => Reset,
            INT0   => INT0,
            INT1   => INT1,
            clock  => clock,
            ProgAB => ProgAB,
            DataAB => DataAB,
            DataWr => DataWr,
            DataRd => DataRd,
            DataDB => DataDB
        );

    -- MAKE THE CLOCK

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

end architecture;























