----------------------------------------------------------------------------
--
--  Atmel AVR Program Memory
--
--  This component describes the program memory for the AVR CPU.  It creates
--  a 64K x 16 ROM.
--
--  Revision History:
--     11 May 98  Glen George       Initial revision.
--      9 May 00  Glen George       Added Reset input for easy initialization
--                                  of the program ROM.
--     11 May 00  Glen George       Updated comments.
--      7 May 02  Glen George       Updated comments.
--
----------------------------------------------------------------------------


--
--  PROG_MEMORY
--
--  This is the program memory component.  It is just a 64 Kword ROM with no
--  timing information.  It is meant to be connected to the AVR CPU.  The ROM
--  is always enabled and may be initialized when Reset is active.
--
--  Inputs:
--    ProgAB - address bus (16 bits)
--    Reset  - system reset (active low)
--
--  Outputs:
--    ProgDB - program memory data bus (16 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity  PROG_MEMORY  is

    port (
        ProgAB  :  in   std_logic_vector(15 downto 0);  -- program address bus
        Reset   :  in   std_logic;                      -- system reset (active low)
        ProgDB  :  out  std_logic_vector(15 downto 0)   -- program data bus
    );

end  PROG_MEMORY;


architecture  ROM  of  PROG_MEMORY  is

    -- define the type for the ROM (an array)
    type  ROMtype  is  array(0 to 65535) of std_logic_vector(15 downto 0);

    -- define the actual ROM (initialized to X)
    signal  ROMbits  :  ROMtype  :=  (others => (others => 'X'));


begin


    -- for any address being accessed - read the data
    ProgDB <= ROMbits(CONV_INTEGER(ProgAB));


    -- process to handle Reset
    process(Reset)
    begin

        -- check if Reset is low now
        if  (Reset = '0')  then
            -- reset is active - initialize the ROM (nothing for now)
            ROMbits(0) <= X"0000";
        end if;

    end process;

end  ROM;
