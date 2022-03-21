
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity avr_core_sim is
end avr_core_sim;

architecture sim of avr_core_sim is
	component avr_core 
		port(
			reset_n : in std_logic;
			clock : in std_logic;
			portA : inout std_logic_vector(7 downto 0);
			portB : inout std_logic_vector(7 downto 0);
			portC : inout std_logic_vector(7 downto 0);
			portD : inout std_logic_vector(7 downto 0)
	--		out_addr_bus : out std_logic_vector(15 downto 0);
	--		inout_data_bus : inout std_logic_vector(7 downto 0);
		);
	end component;
	signal reset : std_logic;
	signal clk : std_logic;
begin
avr : avr_core
	port map(
		reset_n => reset,
		clock => clk
	);

r:
	process
	begin
		reset <= '0';
		wait for 50 ns;
		reset <= '1';
		wait;
	end process;
	
c:
	process
	begin
		clk <= '0';
		wait for 10 ns; --50MHz
		clk <= '1';
		wait for 10 ns;
	end process;

--io:
--	process
--	begin
--		wait for 200 ns;
--		portA <= X"01";
--		portB <= X"02";
--		portC <= X"03";
--		portD <= X"04";
--		wait;
--	end process;
	
end sim;